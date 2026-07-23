package llvm_lsp

import cats.effect.*
import jsonrpclib.fs2.*
import langoustine.lsp.*
import langoustine.lsp.aliases.*
import langoustine.lsp.runtime.*
import langoustine.lsp.requests.*
import langoustine.lsp.structures.*
import langoustine.lsp.app.*
import langoustine.lsp.enumerations.*

import SpannedTree.*

object LLVM_Lsp extends LangoustineApp:
  // stdout is reserved for LSP JSON-RPC framing — never write logs there.
  // We log to `./llvm-ir-lsp.log` (relative to the CWD of the launched
  // process, which is typically the client's workspace root).
  private val logFile =
    java.nio.file.Paths.get("llvm-ir-lsp.log").toAbsolutePath
  scribe.Logger.root
    .clearHandlers()
    .withHandler(
      writer = scribe.file.FileWriter(
        pathBuilder = scribe.file.PathBuilder.static(logFile)
      ),
      minimumLevel = Some(scribe.Level.Debug)
    )
    .replace()

  def server(args: List[String]): Resource[IO, LSPBuilder[IO]] =
    for
      _          <- Resource.eval(
                      scribe.cats.io.info(s"LLVM LSP starting, logging to $logFile")
                    )
      supervisor <- cats.effect.std.Supervisor[IO]
      state      <- Resource.eval(IO.ref(Map.empty[DocumentUri, Index]))
    yield lsp(state, supervisor)

def lsp(
    state: Ref[IO, Map[DocumentUri, Index]],
    supervisor: cats.effect.std.Supervisor[IO]
) =
  val utils = Utils(state)
  LSPBuilder
    .create[IO]
    .handleRequest(initialize) { in =>
      val folders =
        in.params.workspaceFolders.getOrElse(Vector.empty)
      val clientProgress = in.params.capabilities.window
        .flatMap(_.workDoneProgress)
        .getOrElse(false)
      // Kick off the workspace scan in the background under the server-scoped
      // Supervisor. `initialize` returns immediately so Neovim isn't blocked
      // for the multi-second scan; requests that need the index (goto-def,
      // hover, symbol) will just see partial results until the scan finishes.
      val scanFiber =
        utils.scanWorkspace(
          folders,
          back = Some(in.toClient),
          clientSupportsProgress = clientProgress
        ) *>
          state.get.flatMap { m =>
            scribe.cats.io.info(
              s"scanWorkspace done: ${m.size} .ll file(s); total function definitions: ${m.values
                  .map(_.functionDefinitions.size)
                  .sum}"
            )
          }
      scribe.cats.io.info(
        s"initialize: rootUri=${in.params.rootUri}, folders=${folders
            .map(_.uri.value)
            .mkString(",")}, clientSupportsProgress=$clientProgress"
      ) *>
        supervisor.supervise(scanFiber.handleErrorWith { e =>
          scribe.cats.io.error(s"scanWorkspace crashed: ${e.getMessage}")
        }).void *>
        IO {
          InitializeResult(
            capabilities = ServerCapabilities(
              textDocumentSync = Option(TextDocumentSyncKind.Full),
              hoverProvider = Option(true),
              definitionProvider = Option(true),
              referencesProvider = Option(true),
              documentSymbolProvider = Option(true),
              workspaceSymbolProvider = Option(true),
              semanticTokensProvider = Option(
                SemanticTokensOptions(
                  legend = SemanticTokensLegend(
                    tokenTypes = Vector("function"),
                    tokenModifiers = Vector.empty
                  ),
                  full = Option(true),
                  range = Option(false)
                )
              )
            ),
            serverInfo = Option(InitializeResult.ServerInfo("LLVM LSP"))
          )
        }
    }
    .handleRequest(textDocument.definition) { in =>
      val uri = in.params.textDocument.uri
      val pos = in.params.position
      scribe.cats.io.info(
        s"textDocument/definition: uri=$uri line=${pos.line.value} char=${pos.character.value}"
      ) *>
        state.get.flatMap { all =>
          val local = all.get(uri)
          scribe.cats.io.debug(
            s"  known files: ${all.keys.map(_.value).mkString(", ")}"
          ) *>
            (local match
              case None =>
                scribe.cats.io.warn(s"  no index for $uri") *> IO.pure(None)
              case Some(idx) =>
                val caret = utils.caretAt(idx, pos)
                val mdHits =
                  idx.detectReferences.resolve(caret)
                val funcHits = idx.functionReferences.resolve(caret)
                scribe.cats.io.info(
                  s"  caret=$caret; mdRefHits=$mdHits; funcRefHits=$funcHits"
                ) *>
                  utils.definitionLocation(uri, pos).flatMap { locs =>
                    scribe.cats.io.info(
                      s"  -> ${locs.size} definition location(s): ${locs
                          .map(l =>
                            s"${l.uri.value}@${l.range.start.line.value}:${l.range.start.character.value}"
                          )
                          .mkString(", ")}"
                    ) *>
                      IO.pure(locs match
                        case head :: tail =>
                          if tail.nonEmpty then
                            scribe.warn(
                              s"Unexpectedly, got several definitions: $tail"
                            )
                          Some(Definition(head))
                        case Nil => None
                      )
                  }
            )
        }
    }
    .handleRequest(textDocument.references) { in =>
      val uri = in.params.textDocument.uri
      val pos = in.params.position
      scribe.cats.io.info(
        s"textDocument/references: uri=$uri line=${pos.line.value} char=${pos.character.value}"
      ) *>
        state.get.flatMap { all =>
          all.get(uri) match
            case None =>
              scribe.cats.io.warn(s"  no index for $uri") *>
                IO.pure(Option(Vector.empty[Location]))
            case Some(idx) =>
              val caret       = utils.caretAt(idx, pos)
              val nameFromRef = idx.functionReferences.resolve(caret)
              val nameFromDef = idx.functionDefinitions.collect {
                case (n, entry) if entry.nameSpan.contains(caret) => n
              }.toList
              scribe.cats.io.info(
                s"  caret=$caret; nameFromRef=$nameFromRef; nameFromDef=$nameFromDef"
              ) *>
                utils.referencesFor(uri, pos).flatMap { refs =>
                  scribe.cats.io.info(
                    s"  -> ${refs.size} reference(s): ${refs
                        .map(l =>
                          s"${l.uri.value}@${l.range.start.line.value}:${l.range.start.character.value}"
                        )
                        .mkString(", ")}"
                  ) *> IO.pure(Option(refs))
                }
        }
    }
    .handleRequest(workspace.symbol) { in =>
      scribe.cats.io.info(s"workspace/symbol: query='${in.params.query}'") *>
        utils.workspaceSymbols(in.params.query).flatMap { syms =>
          scribe.cats.io.info(s"  -> ${syms.size} symbol(s)") *>
            IO.pure(Option(syms))
        }
    }
    .handleRequest(textDocument.hover) { in =>
      utils.get(in.params.textDocument.uri).flatMap { idx =>
        val caret = utils.caretAt(idx, in.params.position)
        utils.functionNameAt(idx, caret) match
          case Some((name, span)) =>
            utils.lookupFunctionDefinition(name, previewLines = 5).flatMap { defn =>
              val demangled = utils.demangle(name)
              val header =
                if demangled == name then s"### `@$name`"
                else s"### `@$name`\n\n$demangled"
              val body = defn match
                case Some((defUri, _, snippet)) =>
                  val defined = defUri.value.stripPrefix("file://").stripPrefix("file:")
                  s"$header\n\n_Defined in_ `$defined`\n\n```llvm\n$snippet\n```"
                case None =>
                  s"$header\n\n_Definition not found in workspace._"
              scribe.cats.io.info(
                s"textDocument/hover: function=$name defnFound=${defn.isDefined}"
              ) *>
                IO.pure(
                  Some(
                    Hover(
                      contents = MarkedString(MarkedString.S0("markdown", body)),
                      range = Option(span.toRange)
                    )
                  )
                )
            }
          case None =>
            // Fall back to metadata-reference hover: resolve to definition
            // and preview its source.
            utils.definitionSpan(idx, in.params.position).map {
              case head :: tail =>
                if tail.nonEmpty then
                  scribe.warn(s"Unexpectedly, got several definitions: $tail")
                Some(
                  Hover(
                    contents = MarkedString(
                      MarkedString
                        .S0(language = "llvm", idx.text.sliceOut(head))
                    ),
                    range = Option(head.toRange)
                  )
                )
              case _ => None
            }
      }
    }
    .handleRequest(textDocument.documentSymbol) { in =>
      val uri = utils.normalizeUri(in.params.textDocument.uri)
      scribe.cats.io.info(s"textDocument/documentSymbol: uri=$uri") *>
        state.get.flatMap(_.get(uri) match
          case None =>
            scribe.cats.io.warn(s"  no index for $uri") *> IO.pure(None)
          case Some(idx) =>
            val mdSyms = idx.definitions.toVector
              // IDs can be numeric (`!17`) or named (`!llvm.dbg.cu`). Sort
              // numerics first (by int value) then named (lexicographically)
              // — never call .toInt on a non-numeric id, that used to crash.
              .sortBy { case (ref, _) =>
                val s = ref.id.toString
                s.toIntOption match
                  case Some(n) => (0, n, "")
                  case None    => (1, 0, s)
              }
              .map { case (ref, span) =>
                SymbolInformation(
                  name = s"!${ref.id}",
                  location = Location(uri, span.toRange),
                  kind = SymbolKind.Variable
                )
              }
            val funcSyms = idx.functionDefinitions.toVector
              .sortBy(_._2.nameSpan.from.offset)
              .map { case (name, entry) =>
                SymbolInformation(
                  name = s"@$name",
                  location = Location(uri, entry.nameSpan.toRange),
                  kind = SymbolKind.Function
                )
              }
            scribe.cats.io.info(
              s"  -> ${mdSyms.size} metadata + ${funcSyms.size} function symbol(s)"
            ) *> IO.pure(Some(mdSyms ++ funcSyms))
        )
    }
    .handleRequest(textDocument.semanticTokens.full) { in =>
      val uri = utils.normalizeUri(in.params.textDocument.uri)
      scribe.cats.io.info(s"textDocument/semanticTokens/full: uri=$uri") *>
        state.get.flatMap(_.get(uri) match
          case None =>
            scribe.cats.io.warn(s"  no index for $uri") *> IO.pure(None)
          case Some(idx) =>
            val data = utils.encodeSemanticTokens(idx)
            scribe.cats.io.info(
              s"  -> ${data.size / 5} token(s), ${data.size} uints"
            ) *>
              IO.pure(
                Some(
                  SemanticTokens(
                    resultId = None,
                    data = data.map(i => runtime.uinteger(i))
                  )
                )
              )
        )
    }
    .handleNotification(textDocument.didOpen) { in =>
      val uri = in.params.textDocument.uri
      // Only parse if we don't already have this file indexed. The initial
      // workspace scan covers everything under the workspace roots, so this
      // usually short-circuits.
      scribe.cats.io.info(s"textDocument/didOpen: uri=$uri") *>
        utils.recompileIfMissing(uri, in.toClient)
    }
    .handleNotification(textDocument.didSave) { in =>
      val uri = in.params.textDocument.uri
      scribe.cats.io.info(s"textDocument/didSave: uri=$uri") *>
        utils.recompile(uri, in.toClient)
    }
end lsp
