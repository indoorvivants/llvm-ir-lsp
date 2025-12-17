import java.nio.file.Path
import scala.util.Using
import java.io.FileReader
import upickle.core.CharBuilder.apply
import fs2.io.file.Files
import cats.effect.IO
import java.nio.file.Paths

import cats.syntax.all.*
import langoustine.lsp.structures.Position

import SpannedParsers.*, tree.Program

object IndexSpec extends weaver.SimpleIOSuite:
  test("full file indexing") {
    fileContents(basicLL).map { str =>
      expect(parse(str).isRight)
    }
  }

  val files = Seq("basic.ll", "basic2.ll").foreach { name =>
    val path = Paths.get("resources", name)

    test(s"file $name is correctly indexed") {
      parseFile(path).map { case (str, prog) =>
        val idx = Index.create(tree, str, prog)

        println(Utils.definitionSpan(idx, Position(line = 51, character = 15)))

        expect.all(
          idx.text.text == str
        )
      }
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
