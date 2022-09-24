import java.nio.file.Path
import scala.util.Using
import java.io.FileReader
import upickle.core.CharBuilder.apply
import fs2.io.file.Files
import cats.effect.IO
import java.nio.file.Paths

import cats.syntax.all.*

object IndexSpec extends weaver.SimpleIOSuite:
  test("full file indexing") {
    fileContents(basicLL).map { str =>
      expect(parsers.parse(str).isRight)
    }
  }

  test("files are correctly indexed") {
    parseFile(basicLL).map { case (str, prog) =>
      val idx = Index.create(str, prog)

      expect.all(
        idx.text.text == str,
        
      )
    }
  }

  val basicLL = Paths.get("resources", "basic.ll")

  def parseFile(p: Path): IO[(String, SpannedTree.Program)] =
    for
      str <- fileContents(p)
      prog <- IO.fromEither(parsers.parse(str))
    yield str -> prog

  def fileContents(p: Path) =
    Files[IO].readUtf8(fs2.io.file.Path.fromNioPath(p)).compile.string
