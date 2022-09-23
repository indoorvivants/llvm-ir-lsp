object TextIndexSpec extends weaver.FunSuite:
  test("basics") {
    val text =
      """
      |hello
      |
      | asdasd
      |asdasdasd
      |asdasdasdasd
      |   
      |abstract class sd
      |asdasd""".stripMargin

    val index = TextIndex.construct(text)

    expect.all(index.lines.size == text.linesIterator.size) and
      forEach(text.linesIterator.toVector.zipWithIndex) { case (line, idx) =>
        val span = index.lines(idx)

        expect.all(
          // span covers only line content without the line breaks
          span.to.offset - span.from.offset == line.length,

          // slice out method returns line content
          index.sliceOut(span) == line,

          // offset lookup for line start returns correct line
          index.offsetLookup.resolve(span.from.offset) == List(idx),

          // offset lookup for line end returns correct line
          index.offsetLookup.resolve(span.to.offset) == List(idx),

          // offset lookup for line middle returns correct line
          index.offsetLookup.resolve(
            (span.to.offset + span.from.offset) / 2
          ) == List(idx)
        )
      }
  }
