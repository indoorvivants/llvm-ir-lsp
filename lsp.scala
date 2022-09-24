//> using lib "tech.neander::langoustine-app::0.0.16"

import SpannedTree._

import cats.effect.*
import cats.effect.std.*
import fs2.io.file.*
import jsonrpclib.fs2._
import langoustine.lsp.*
import langoustine.lsp.all.*
import langoustine.lsp.app.*

object LLVM_Lsp extends LangoustineApp.Simple:
  def server: IO[LSPBuilder[cats.effect.IO]] =
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

