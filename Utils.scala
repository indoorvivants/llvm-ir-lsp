import SpannedTree._
import cats.effect.*
import cats.effect.std.*
import fs2.io.file.*
import jsonrpclib.fs2._
import langoustine.lsp.*
import langoustine.lsp.all.*
import langoustine.lsp.app.*

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
            state.update(_.updated(documentUri, Index.create(str, st))) *>
              back.sendMessage(s"Successfully parsed")
      }

extension (s: cats.parse.Caret)
  def toPosition: Position =
    Position(line = s.line, character = s.col)

extension (s: Span)
  def toRange: Range = Range(s.from.toPosition, s.to.toPosition)

extension (s: Position)
  def toCaret(offset: Int) =
    cats.parse.Caret(s.line.value, s.character.value, offset)

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
