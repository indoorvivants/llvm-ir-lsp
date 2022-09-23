//> using lib "tech.neander::langoustine-app::0.0.16"

import langoustine.lsp.app.*
import langoustine.lsp.all.*
import cats.effect.IO
import langoustine.lsp.LSPBuilder

import SpannedTree._

import cats.effect.*
import cats.effect.std.*

import jsonrpclib.fs2._
import langoustine.lsp.Communicate
import fs2.io.file.Files
import fs2.io.file.Path
import langoustine.lsp.aliases.MarkedString.apply

object LLVM_Lsp extends LangoustineApp.Simple:
  def server: IO[LSPBuilder[cats.effect.IO]] =
    IO.ref(Map.empty).map(lsp)

extension (s: cats.parse.Caret)
  def toPosition: Position =
    Position(line = s.line, character = s.col)

extension (s: Span)
  def toRange: Range = Range(s.from.toPosition, s.to.toPosition)

extension (s: Position)
  def toCaret(offset: Int) =
    cats.parse.Caret(s.line.value, s.character.value, offset)

case class Index(
    definitions: Map[Atom.Ref, Span],
    detectReferences: IntervalTree[Atom.Ref],
    text: TextIndex
)

def index(text: String, p: Program): Index =
  val defns = p.asses.collect {
    case WithSpan(span, Statement.Assignment(spannedId, spannedExpr)) =>
      spannedId.value -> span
  }

  def extractReferences(
      expression: WithSpan[Expression[WithSpan]]
  ): Vector[WithSpan[SpannedTree.Atom.Ref]] =
    def go(e: WithSpan[Expression[WithSpan]]): Vector[WithSpan[Atom.Ref]] =
      import SpannedTree.*

      e.value match
        case NamedData(struct, fields) =>
          fields.collect { case Field.KeyValue(name, value) =>
            go(value)
          }.flatten
        case Bag(exprs)     => exprs.flatMap(go)
        case Distinct(expr) => go(expr)
        case rf: Atom.Ref   => Vector(e.copy(value = rf))
        case _              => Vector.empty

    go(expression)

  val occurrences = p.asses
    .flatMap { case WithSpan(_, value) =>
      value match
        case Statement.Assignment(id, value) => extractReferences(value)
    }
    .map { ws => ws.span -> ws.value }

  Index(
    defns.toMap,
    IntervalTree.construct(occurrences.toMap),
    TextIndex.construct(text)
  )

def lsp(state: Ref[IO, Map[DocumentUri, Index]]) =
  val utils = Utils(state)
  LSPBuilder
    .create[IO]
    .handleRequest(initialize) { in =>
      IO {
        InitializeResult(
          capabilities = ServerCapabilities(
            textDocumentSync = Opt(TextDocumentSyncKind.Full),
            hoverProvider = Opt(true),
            definitionProvider = Opt(true),
            documentSymbolProvider = Opt(true)
          ),
          serverInfo = Opt(InitializeResult.ServerInfo("LLVM LSP"))
        )
      }
    }
    .handleRequest(textDocument.definition) { in =>
      utils.get(in.params.textDocument.uri).flatMap { idx =>
        utils.definitionSpan(idx, in.params.position).map {
          case head :: tail =>
            if tail.nonEmpty then
              scribe.warn(s"Unexpectedly, got several definitions: $tail")

            Opt(
              Definition(Location(in.params.textDocument.uri, head.toRange))
            )
          case Nil =>
            Opt.empty

        }
      }
    }
    .handleRequest(textDocument.hover) { in =>
      utils.get(in.params.textDocument.uri).flatMap { idx =>
        utils.definitionSpan(idx, in.params.position).map {
          case head :: tail =>
            if tail.nonEmpty then
              scribe.warn(s"Unexpectedly, got several definitions: $tail")

            Opt(
              Hover(
                contents = MarkedString(
                  MarkedString.S0(language = "llvm", idx.text.sliceOut(head))
                ),
                range = Opt(head.toRange)
              )
            )
          case Nil =>
            Opt.empty
        }
      }
    }
    .handleRequest(textDocument.documentSymbol) { in =>
      utils.get(in.params.textDocument.uri).map { idx =>
        Opt {
          idx.definitions.toVector.sortBy(_._1.id.value).map {
            case (ref, span) =>
              SymbolInformation(
                name = s"!${ref.id}",
                location = Location(in.params.textDocument.uri, span.toRange),
                kind = SymbolKind.Variable
              )
          }
        }
      }
    }
    .handleNotification(textDocument.didOpen) { in =>
      utils.recompile(in.params.textDocument.uri, in.toClient)
    }
    .handleNotification(textDocument.didSave) { in =>
      utils.recompile(in.params.textDocument.uri, in.toClient)
    }

class Utils(state: Ref[IO, Map[DocumentUri, Index]]):
  def get(u: DocumentUri) =
    state.get.flatMap(
      _.get(u)
        .map(IO.pure)
        .getOrElse(
          IO.raiseError(
            new RuntimeException(s"No valid document found for ${u}")
          )
        )
    )
  def definitionSpan(idx: Index, position: Position) =
    val lineSpan = idx.text.lines(position.line.value)
    val cursorPosition =
      lineSpan.from.offset + position.character.value
    val resolved = idx.detectReferences.resolve(
      position.toCaret(cursorPosition)
    )
    IO.pure(resolved.flatMap(idx.definitions.get))

  def recompile(documentUri: DocumentUri, back: Communicate[IO]) =
    Files[IO]
      .readUtf8(Path(documentUri.value.drop("file:".length)))
      .compile
      .string
      .flatMap { str =>
        parsers.parse(str) match
          case Left(err) =>
            back.sendMessage(
              s"Failed to parse $err",
              MessageType.Error
            )
          case Right(st) =>
            state.update(_.updated(documentUri, index(str, st))) *>
              back.sendMessage(s"Successfully parsed")
      }

extension (back: Communicate[IO])
  def sendMessage(
      msg: String,
      tpe: MessageType = MessageType.Info
  ) =
    back.notification(
      window.showMessage,
      ShowMessageParams(tpe, msg)
    )

  def publish(uri: DocumentUri, vec: Vector[Diagnostic]) =
    back.notification(
      textDocument.publishDiagnostics,
      PublishDiagnosticsParams(uri, diagnostics = vec)
    )
