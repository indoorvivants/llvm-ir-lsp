package llvm_lsp

import java.nio.file.Path
import java.nio.file.Paths

import cats.effect.IO
import cats.syntax.all.*
import fs2.io.file.Files
import langoustine.lsp.structures.Position

import SpannedParsers.*
import tree.Program

object IndexSpec extends weaver.SimpleIOSuite:
  test("full file indexing") {
    fileContents(basicLL).map { str =>
      expect(clue(parse(str)).isRight)
    }
  }

  val files = Seq("basic.ll", "basic2.ll").foreach { name =>
    val path = Paths.get("resources", name)

    test(s"file $name is correctly indexed") {
      parseFile(path).map { case (str, prog) =>
        val idx = Index.create(tree, str, prog)

        expect.all(
          idx.text.text == str
        )
      }
    }

  }

  test("function definitions are indexed with spans (basic.ll: 2 defines + 2 declares)") {
    parseFile(basicLL).map { case (str, prog) =>
      val idx = Index.create(tree, str, prog)
      expect.all(
        idx.functionDefinitions.contains("_Z1tf"),
        idx.functionDefinitions.contains("_Z6squarei"),
        idx.functionDefinitions.contains("puts"),
        idx.functionDefinitions.contains("llvm.dbg.declare"),
        idx.functionDefinitions.size == 4
      )
    }
  }

  test("call sites are recorded as function references") {
    parseFile(basicLL).map { case (str, prog) =>
      val idx = Index.create(tree, str, prog)
      val refs = idx.functionReferences.entries.map(_._2).toVector
      expect.all(
        refs.contains("llvm.dbg.declare"),
        // basic.ll: @_Z1tf calls dbg.declare once; @_Z6squarei calls
        // dbg.declare twice and @_Z1tf once.
        refs.count(_ == "llvm.dbg.declare") == 3,
        refs.contains("_Z1tf")
      )
    }
  }

  test("resolving inside a call-site funcName span yields that function") {
    parseFile(basicLL).map { case (str, prog) =>
      val idx = Index.create(tree, str, prog)
      val (span, name) = idx.functionReferences.entries.head
      val mid = (span.from.offset + span.to.offset) / 2
      val resolved = idx.functionReferences.resolve(mid)
      expect(resolved.contains(name))
    }
  }

  test("lookupFunctionDefinition returns the defining file's URI and a snippet") {
    import cats.effect.unsafe.implicits.global
    import langoustine.lsp.runtime.DocumentUri
    for
      state <- IO.ref(Map.empty[DocumentUri, Index])
      utils = new Utils(state)
      // Index basic.ll under a fake URI so we can look it up.
      (str, prog) <- parseFile(basicLL)
      idx = Index.create(tree, str, prog)
      uri = DocumentUri("file:///fake/basic.ll")
      _   <- state.set(Map(uri -> idx))
      // basic.ll defines @_Z1tf on the `define …` line.
      result <- utils.lookupFunctionDefinition("_Z1tf", previewLines = 5)
    yield
      val (defUri, sp, snippet) = result.get
      val lines = snippet.split('\n')
      expect.all(
        defUri == uri,
        sp.from.line >= 0,
        // Snippet should start with the `define` line.
        lines.headOption.exists(_.startsWith("define")),
        // At most 5 lines requested.
        lines.length <= 5
      )
  }

  test("demangle: mangled Scala Native names are decoded, others pass through") {
    import cats.effect.unsafe.implicits.global
    val utils = new Utils(
      cats.effect.IO
        .ref(Map.empty[langoustine.lsp.runtime.DocumentUri, Index])
        .unsafeRunSync()
    )
    val raw = "_SM39scala.jdk.javaapi.CollectionConverters$RE"
    val demangled = utils.demangle(raw)
    // On failure, demangle returns the input unchanged; a real Scala Native
    // decode should differ. We don't pin the exact form (that would couple us
    // to the demangler's output format) — just that it changed.
    IO.pure(
      expect.all(
        demangled != raw,
        // Non-mangled names should pass through untouched.
        utils.demangle("puts") == "puts",
        utils.demangle("scalanative_personality") == "scalanative_personality"
      )
    )
  }

  test("semantic tokens: every function name gets a token, delta-encoded") {
    import cats.effect.unsafe.implicits.global
    parseFile(basicLL).map { case (str, prog) =>
      val idx = Index.create(tree, str, prog)
      // Reuse Utils' encoder by hand-instantiating a throwaway state.
      val utils = new Utils(cats.effect.IO.ref(Map.empty[langoustine.lsp.runtime.DocumentUri, Index]).unsafeRunSync())
      val data = utils.encodeSemanticTokens(idx)
      // basic.ll: 4 function defs/declares + 4 call-site refs = 8 tokens = 40 uints.
      expect.all(
        data.size % 5 == 0,
        data.size / 5 == idx.functionNameSpans.size,
        // First token: absolute line/col (deltas from 0/0), tokenType==0
        // (function), no modifiers.
        data(3) == 0,
        data(4) == 0
      )
    }
  }

  test("call to @_Z1tf inside @_Z6squarei resolves to the definition span") {
    parseFile(basicLL).map { case (str, prog) =>
      val idx = Index.create(tree, str, prog)
      // Find the call-site span for _Z1tf.
      val callSite = idx.functionReferences.entries
        .collectFirst { case (sp, "_Z1tf") => sp }
        .get
      val mid = (callSite.from.offset + callSite.to.offset) / 2
      val defnLocs = idx.functionReferences
        .resolve(mid)
        .flatMap(idx.functionDefinitions.get)
      // Definition span for @_Z1tf sits on the `define ...` line.
      expect(defnLocs.contains(idx.functionDefinitions("_Z1tf")))
    }
  }

  val basicLL = Paths.get("resources", "basic.ll")

  def parseFile(p: Path): IO[(String, Program)] =
    for
      str  <- fileContents(p)
      prog <- IO.fromEither(parse(str))
    yield str -> prog

  def fileContents(p: Path) =
    Files[IO].readUtf8(fs2.io.file.Path.fromNioPath(p)).compile.string
end IndexSpec
