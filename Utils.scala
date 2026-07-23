package llvm_lsp

import cats.effect.*
import cats.effect.syntax.all.*
import cats.syntax.all.*
import fs2.io.file.*
import langoustine.lsp.*
import langoustine.lsp.all.*

import SpannedParsers.*

class Utils(state: Ref[IO, Map[DocumentUri, Index]]) extends UtilsCommon:
  def get(u: DocumentUri) =
    val nu = normalizeUri(u)
    state.get.flatMap(
      _.get(nu)
        .map(IO.pure)
        .getOrElse(
          IO.raiseError(
            new RuntimeException(s"No valid document found for ${nu}")
          )
        )
    )

  // Canonical URI form: `file://<absolute-path>` with three slashes on Unix
  // (empty authority + leading `/`). LSP clients (Neovim, VS Code) emit this
  // form, and `java.io.File#toURI` produces it. Store keys in this form so
  // request URIs and scan URIs collide correctly in the state Map.
  private def uriToPath(u: DocumentUri): Path =
    val s = u.value
    val stripped =
      if s.startsWith("file://") then s.drop("file://".length)
      else if s.startsWith("file:") then s.drop("file:".length)
      else s
    Path(stripped)

  private def pathToUri(p: Path): DocumentUri =
    DocumentUri(s"file://${p.absolute.toString}")

  // Canonicalize a URI the client sent (may be `file:///x` or `file:/x`) into
  // the same form we use as map keys. Applied on every request handler entry
  // point that looks the URI up in state.
  def normalizeUri(u: DocumentUri): DocumentUri =
    pathToUri(uriToPath(u))

  def parseAndIndex(
      uri: DocumentUri,
      src: String
  ): Either[String, Index] =
    parse(src) match
      case Left(err) => Left(err.toString)
      case Right(st) => Right(Index.create(SpannedParsers.tree, src, st))

  /** Reparse a file if we don't already have it indexed (e.g. it was opened
    * from outside the workspace root and thus missed by `scanWorkspace`).
    * Never re-reads on `didOpen` for files we already know — that would be
    * wasted work and would race with the workspace scan.
    */
  def recompileIfMissing(
      documentUri: DocumentUri,
      back: Communicate[IO]
  ): IO[Unit] =
    val nu = normalizeUri(documentUri)
    state.get.flatMap { m =>
      if m.contains(nu) then IO.unit
      else recompile(nu, back)
    }

  /** Force a reparse from disk. Used by `didSave`. Publishes any parse error
    * as a diagnostic on line 1 of the file (never as a window/showMessage —
    * those block the editor); clears diagnostics on success.
    */
  def recompile(documentUri: DocumentUri, back: Communicate[IO]): IO[Unit] =
    val nu = normalizeUri(documentUri)
    Files[IO]
      .readUtf8(uriToPath(nu))
      .compile
      .string
      .flatMap { str =>
        parseAndIndex(nu, str) match
          case Left(err) =>
            back.publish(nu, Vector(parseErrorDiagnostic(err)))
          case Right(idx) =>
            state.update(_.updated(nu, idx)) *>
              back.publish(nu, Vector.empty)
      }
      .handleErrorWith { e =>
        scribe.cats.io.warn(
          s"recompile: I/O error on $nu: ${e.getMessage}"
        )
      }

  private def parseErrorDiagnostic(err: String): Diagnostic =
    Diagnostic(
      range = Range(
        Position(line = 0, character = 0),
        Position(line = 0, character = 1)
      ),
      severity = Option(DiagnosticSeverity.Error),
      source = Option("llvm-ir-lsp"),
      message = err
    )

  /** Walk each workspace root, parse every .ll file, index all of them.
    * Silently ignores per-file parse failures (best-effort).
    *
    * If `back` is provided and the client supports server-initiated progress,
    * emits `$/progress` notifications with a per-file percentage. On clients
    * that don't support work-done progress the `create` request fails and we
    * just skip the progress emission; indexing still happens.
    */
  private val parseParallelism = 4

  def scanWorkspace(
      folders: Vector[WorkspaceFolder],
      back: Option[Communicate[IO]] = None,
      clientSupportsProgress: Boolean = false
  ): IO[Unit] =
    // Discover files first, then index — keeps the progress denominator honest.
    folders
      .traverse { folder =>
        val root = Path(folder.uri.value.drop("file:".length))
        Files[IO]
          .walk(root)
          .filter(p => p.extName == ".ll")
          .compile
          .toVector
      }
      .map(_.flatten)
      .flatMap { paths =>
        val total = paths.size
        (back, clientSupportsProgress) match
          case (Some(b), true) if total > 0 =>
            // Progress requires a monotonically-advancing counter. Use a Ref
            // so parallel workers can safely bump it.
            IO.ref(0).flatMap { done =>
              withProgress(b, "Indexing LLVM IR workspace", total) { report =>
                paths.parTraverseN(parseParallelism) { p =>
                  indexOne(p) *>
                    done.updateAndGet(_ + 1).flatMap(n =>
                      report(n, s"${p.fileName.toString} ($total files)")
                    )
                }.void
              }
            }
          case _ =>
            paths.parTraverseN(parseParallelism)(indexOne).void
      }

  private def indexOne(p: Path): IO[Unit] =
    Files[IO]
      .readUtf8(p)
      .compile
      .string
      .flatMap { src =>
        val uri = pathToUri(p)
        parseAndIndex(uri, src) match
          case Right(idx) => state.update(_.updated(uri, idx))
          case Left(err)  =>
            scribe.cats.io.warn(s"scanWorkspace: failed to parse $uri: $err")
      }
      .handleErrorWith { e =>
        scribe.cats.io.warn(s"scanWorkspace: I/O error on $p: ${e.getMessage}")
      }

  /** Bracket work with LSP $/progress begin/report/end notifications. The
    * `report` callback advances a percentage counter (0..100 clipped) with an
    * optional message.
    */
  private def withProgress(
      back: Communicate[IO],
      title: String,
      total: Int
  )(work: ((Int, String) => IO[Unit]) => IO[Unit]): IO[Unit] =
    import io.circe.syntax.EncoderOps
    val token = aliases.ProgressToken(java.util.UUID.randomUUID().toString)
    val create = back
      .request(
        window.workDoneProgress.create,
        WorkDoneProgressCreateParams(token)
      )
      .attempt
      .flatMap {
        case Right(_) => IO.pure(true)
        case Left(e)  =>
          scribe.cats.io.warn(
            s"workDoneProgress/create rejected: ${e.getMessage} — skipping progress"
          ) *> IO.pure(false)
      }
    def send(value: io.circe.Json): IO[Unit] =
      back.notification($DOLLAR.progress, ProgressParams(token, value))
    def pct(n: Int): runtime.uinteger =
      runtime.uinteger(math.max(0, math.min(100, (n.toDouble / total * 100).toInt)))
    create.flatMap {
      case false => work((_, _) => IO.unit)
      case true  =>
        val begin = WorkDoneProgressBegin(
          kind = "begin",
          title = title,
          percentage = Option(runtime.uinteger(0)),
          message = Option(s"0 / $total")
        ).asJson
        val end = WorkDoneProgressEnd(kind = "end", message = Option("done")).asJson
        send(begin) *>
          work((done, msg) =>
            send(
              WorkDoneProgressReport(
                kind = "report",
                percentage = Option(pct(done)),
                message = Option(s"$done / $total — $msg")
              ).asJson
            )
          ).guarantee(send(end))
    }
  end withProgress

  /** Cross-file goto-definition. Tries the local index first; if the cursor is
    * on a call to a function whose definition lives in another file, resolves
    * via the workspace-wide symbol table (built from `functionDefinitions` of
    * every indexed file).
    */
  def definitionLocation(
      uri: DocumentUri,
      position: Position
  ): IO[List[Location]] =
    val nu = normalizeUri(uri)
    state.get.flatMap { all =>
      all.get(nu) match
        case None => IO.pure(Nil)
        case Some(idx) =>
          val caret = toCaretAt(idx, position)
          val local = idx.detectReferences
            .resolve(caret)
            .flatMap(idx.definitions.get)
            .map(sp => Location(nu, sp.toRange))
          val localFuncs = idx.functionReferences
            .resolve(caret)
            .flatMap(idx.functionDefinitions.get)
            .map(entry => Location(nu, entry.nameSpan.toRange))
          val crossFile =
            if local.isEmpty && localFuncs.isEmpty then
              idx.functionReferences.resolve(caret).flatMap { name =>
                workspaceLookup(all, name).map { case (u, sp) =>
                  Location(u, sp.toRange)
                }
              }
            else Nil
          IO.pure((local ++ localFuncs ++ crossFile).toList)
    }

  /** Every call site of the function under the cursor, across every indexed
    * file. Returns Locations pointing at the funcName token of each call.
    */
  def referencesFor(
      uri: DocumentUri,
      position: Position
  ): IO[Vector[Location]] =
    val nu = normalizeUri(uri)
    state.get.flatMap { all =>
      all.get(nu) match
        case None => IO.pure(Vector.empty)
        case Some(idx) =>
          val caret = toCaretAt(idx, position)
          // Determine the function name under cursor — could come from a call
          // site (via functionReferences) or from the definition itself.
          val nameFromRef = idx.functionReferences.resolve(caret)
          val nameFromDef = idx.functionDefinitions.collect {
            case (n, entry) if entry.nameSpan.contains(caret) => n
          }
          val names = (nameFromRef ++ nameFromDef).toSet
          if names.isEmpty then IO.pure(Vector.empty)
          else
            IO.pure(
              all.toVector.flatMap { case (u, otherIdx) =>
                otherIdx.functionReferences.entries.collect {
                  case (sp, n) if names(n) =>
                    Location(u, sp.toRange)
                }
              }
            )
    }

  /** `workspace/symbol` — filter across every indexed file's function
    * definitions by substring match on the query.
    */
  def workspaceSymbols(query: String): IO[Vector[SymbolInformation]] =
    state.get.map { all =>
      val q = query.toLowerCase
      all.toVector.flatMap { case (u, idx) =>
        idx.functionDefinitions.collect {
          case (name, entry) if q.isEmpty || name.toLowerCase.contains(q) =>
            SymbolInformation(
              name = s"@$name",
              location = Location(u, entry.nameSpan.toRange),
              kind = SymbolKind.Function
            )
        }
      }
    }

  private def workspaceLookup(
      all: Map[DocumentUri, Index],
      name: String
  ): Option[(DocumentUri, Span)] =
    // Prefer entries from files that have a `define`; only fall back to a
    // `declare` if no `define` exists anywhere in the workspace.
    val hits = all.iterator.flatMap { case (u, idx) =>
      idx.functionDefinitions.get(name).map(entry => (u, entry))
    }.toVector
    hits
      .find(_._2.kind == FunctionDefKind.Define)
      .orElse(hits.headOption)
      .map((u, entry) => (u, entry.nameSpan))

  /** LSP semantic-tokens delta encoding over this file's function-name
    * spans. All spans emitted with tokenType=0 (see SemanticTokensLegend
    * in the initialize response) and no modifiers. Skips any multi-line
    * span defensively — the LSP encoding requires single-line tokens.
    */
  def encodeSemanticTokens(idx: Index): Vector[Int] =
    val buf     = Vector.newBuilder[Int]
    var prevLn  = 0
    var prevCol = 0
    idx.functionNameSpans.foreach { sp =>
      if sp.from.line == sp.to.line then
        val ln     = sp.from.line
        val col    = sp.from.col
        val length = sp.to.col - sp.from.col
        if length > 0 then
          val deltaLn = ln - prevLn
          val deltaCol =
            if deltaLn == 0 then col - prevCol else col
          buf += deltaLn
          buf += deltaCol
          buf += length
          buf += 0 // tokenType index — "function"
          buf += 0 // tokenModifiers bitmap
          prevLn = ln
          prevCol = col
    }
    buf.result()

  /** Function name (and the exact span it occupies) at this caret, either
    * from a call-site occurrence or from the definition itself. `None` if
    * the caret isn't on a function name.
    */
  def functionNameAt(idx: Index, caret: Caret): Option[(String, Span)] =
    val fromRef = idx.functionReferences.entries
      .collectFirst { case (sp, n) if sp.contains(caret) => (n, sp) }
    fromRef.orElse(
      idx.functionDefinitions.collectFirst {
        case (n, entry) if entry.nameSpan.contains(caret) => (n, entry.nameSpan)
      }
    )

  /** Look up where a function is defined across every indexed file. Prefers a
    * `define` over a `declare` when both exist. Returns the defining file's
    * URI, the name span, and a source snippet clamped to the actual body
    * (single line for `declare`, up to `previewLines` for `define`).
    */
  def lookupFunctionDefinition(
      name: String,
      previewLines: Int = 5
  ): IO[Option[(DocumentUri, Span, String)]] =
    state.get.map { all =>
      val hits = all.iterator.flatMap { case (u, idx) =>
        idx.functionDefinitions.get(name).map(entry => (u, idx, entry))
      }.toVector
      hits
        .find(_._3.kind == FunctionDefKind.Define)
        .orElse(hits.headOption)
        .map { case (uri, idx, entry) =>
          val lines = idx.text.text.linesIterator.toVector
          val start = entry.nameSpan.from.line
          // Clamp by both the caller's request and the entry's actual end
          // line, so declares yield one line and defines never bleed into
          // whatever comes after the closing `}`.
          val end = math.min(
            lines.size,
            math.min(start + previewLines, entry.endLine + 1)
          )
          val snippet =
            if start >= lines.size then ""
            else lines.slice(start, end).mkString("\n")
          (uri, entry.nameSpan, snippet)
        }
    }

  /** Best-effort demangle of a Scala Native symbol. Falls back to the input
    * string on failure (the demangler throws on non-mangled names).
    */
  def demangle(name: String): String =
    scala.util
      .Try(sn_demangler.Demangler.demangle(name, checkComplete = false))
      .getOrElse(name)

  def caretAt(idx: Index, position: Position): Caret =
    val lineSpan       = idx.text.lines(position.line.value)
    val cursorPosition = lineSpan.from.offset + position.character.value
    position.toCaret(cursorPosition)

  private def toCaretAt(idx: Index, position: Position): Caret = caretAt(idx, position)
end Utils

private trait UtilsCommon:
  def definitionSpan(idx: Index, position: Position) =
    val lineSpan = idx.text.lines(position.line.value)
    val cursorPosition =
      lineSpan.from.offset + position.character.value
    val caret     = position.toCaret(cursorPosition)
    val mdSpans   = idx.detectReferences.resolve(caret).flatMap(idx.definitions.get)
    val funcSpans =
      idx.functionReferences
        .resolve(caret)
        .flatMap(idx.functionDefinitions.get)
        .map(_.nameSpan)
    IO.pure(mdSpans ++ funcSpans)
end UtilsCommon

object Utils extends UtilsCommon

extension (s: Caret)
  def toPosition: Position =
    Position(line = s.line, character = s.col)

extension (s: Span)
  def toRange: Range = Range(s.from.toPosition, s.to.toPosition)

extension (s: Position)
  def toCaret(offset: Int) =
    Caret(s.line.value, s.character.value, offset)

extension (back: Communicate[IO])
  // Parse errors surface as diagnostics; nothing else uses window/showMessage
  // — those popup dialogs block Neovim.
  def publish(uri: DocumentUri, vec: Vector[Diagnostic]) =
    back.notification(
      textDocument.publishDiagnostics,
      PublishDiagnosticsParams(uri, diagnostics = vec)
    )
end extension
