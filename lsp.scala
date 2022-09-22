//> using lib "tech.neander::langoustine-app:0.0.16"

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

object LLVM_Lsp extends LangoustineApp.Simple:
  def server: IO[LSPBuilder[cats.effect.IO]] =
    IO.ref(Map.empty).map(lsp)

extension (s: cats.parse.Caret)
  def toPosition: Position =
    Position(line = s.line, character = s.col)

extension (s: Span)
  def toRange: Range = Range(s.from.toPosition, s.to.toPosition)

extension (s: Position)
  def toCaret = cats.parse.Caret(s.line.value, s.character.value, -1)

case class Index(definitions: Map[Atom.Ref, Span])

def index(p: Program): Index =
  Index(p.asses.collect {
    case WithSpan(span, Statement.Assignment(spannedId, spannedExpr)) =>
      spannedId.value -> span
  }.toMap)

def lsp(state: Ref[IO, Map[DocumentUri, Index]]) =
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
    .handleRequest(textDocument.documentSymbol) { in =>
      state.get.map(_.get(in.params.textDocument.uri)).map {
        case None => Opt(Vector.empty)
        case Some(idx) =>
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
      val documentUri = in.params.textDocument.uri
      Files[IO]
        .readUtf8(Path(documentUri.value.drop("file:".length)))
        .compile
        .string
        .map(parsers.parse)
        .flatMap {
          case Left(err) =>
            in.toClient.sendMessage(s"Failed to parse $err", MessageType.Error)
          case Right(st) =>
            state.update(_.updated(documentUri, index(st))) *>
              in.toClient.sendMessage(s"Successfully parsed")
        }
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
