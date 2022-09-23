import cats.parse.Caret

object IntervalTreeSpec extends weaver.FunSuite:
  test("basics") {
    def c(off: Int) = Caret(0, 0, off)

    val spans = Map(
      Span(c(0), c(4)) -> "hello",
      Span(c(5), c(7)) -> "yo",
      Span(c(11), c(14)) -> "yoasd"
    )

    val itree = IntervalTree.construct(spans)

    expect.all(
      itree.resolve(c(3)) == List("hello"),
      itree.resolve(c(12)) == List("yoasd"),
      itree.resolve(c(50)) == Nil,
      itree.resolve(c(6)) == List("yo")
    )

  }

  test("overlapping") {
    def c(off: Int) = Caret(0, 0, off)

    val spans = Map(
      Span(c(0), c(4)) -> "hello",
      Span(c(2), c(7)) -> "yo",
      Span(c(6), c(14)) -> "yoasd"
    )

    val itree = IntervalTree.construct(spans)

    expect.all(
      itree.resolve(c(3)).toSet == Set("hello", "yo"),
      itree.resolve(c(6)).toSet == Set("yo", "yoasd"),
      itree.resolve(c(10)).toSet == Set("yoasd"),
      itree.resolve(c(15)) == Nil
    )
  }
