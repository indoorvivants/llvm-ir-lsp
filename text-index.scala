import cats.parse.*
import scala.collection.SortedMap

case class TextIndex(lines: SortedMap[Int, Span], text: String):
  val offsetLookup: IntervalTree[Int] = IntervalTree.construct(lines.toMap.map(_.swap))
  def sliceOut(span: Span) =
    text.slice(span.from.offset, span.to.offset)

object TextIndex:
  def construct(text: String): TextIndex =
    val m =
      text.linesWithSeparators.zipWithIndex.foldLeft(0 -> List.empty[Span]) {
        case ((offset, spans), (line, idx)) =>
          val length = line.length - line.count(c => c == '\r' || c == '\n')

          val start = Caret(idx , 0, offset)
          val end = Caret(idx , length, offset + length)

          ((offset + line.length), Span(start, end) :: spans)
      }

    TextIndex(
      SortedMap(
        m._2.reverse.zipWithIndex.map(_.swap)*
      ),
      text
    )
