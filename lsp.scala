package llvm_lsp

import cats.effect.*
import jsonrpclib.fs2.*
import langoustine.lsp.*
import langoustine.lsp.all.*
import langoustine.lsp.app.*

import SpannedTree.*

object LLVM_Lsp extends LangoustineApp.Simple:
  def server: IO[LSPBuilder[cats.effect.IO]] =
    scribe.cats.io.info("Hello from LLVM LSP!") *>
      IO.ref(Map.empty).map(lsp)

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
        utils.definitionSpan(idx, in.params.position).map { l =>
          val x: Opt[Hover] = l match
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
            case _ =>
              Opt.empty
          end x

          x
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
end lsp
