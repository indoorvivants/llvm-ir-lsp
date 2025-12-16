import java.nio.file.Path
import scala.util.Using
import java.io.FileReader
import upickle.core.CharBuilder.apply
import fs2.io.file.Files
import cats.effect.IO
import java.nio.file.Paths

import cats.syntax.all.*
import langoustine.lsp.structures.Position

object IndexSpec extends weaver.SimpleIOSuite:
  test("full file indexing") {
    fileContents(basicLL).map { str =>
      expect(parsers.parse(str).isRight)
    }
  }

  val files = Seq("basic.ll", "basic2.ll").foreach { name =>
    val path = Paths.get("resources", name)

    test(s"file $name is correctly indexed") {
      parseFile(path).map { case (str, prog) =>
        val idx = Index.create(str, prog)

        println(Utils.definitionSpan(idx, Position(line = 51, character = 15)))

        expect.all(
          idx.text.text == str
        )
      }
    }

  }

  val basicLL = Paths.get("resources", "basic.ll")

  def parseFile(p: Path): IO[(String, SpannedTree.Program)] =
    for
      str  <- fileContents(p)
      prog <- IO.fromEither(parsers.parse(str))
    yield str -> prog

  def fileContents(p: Path) =
    Files[IO].readUtf8(fs2.io.file.Path.fromNioPath(p)).compile.string
end IndexSpec
