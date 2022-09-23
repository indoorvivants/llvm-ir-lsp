//> using lib "com.lihaoyi::pprint:0.7.3"

import cats.parse.Caret

trait IntervalTree[T]:
  def resolve(position: Caret): List[T] = resolve(position.offset)
  def resolve(offset: Int): List[T]

object IntervalTree:
  def construct[T](mp: Map[Span, T]): IntervalTree[T] =
    def split(sortedSpans: Vector[Span]): Tree =
      if sortedSpans.size == 1 then Tree.Leaf(sortedSpans.head)
      else if sortedSpans.size == 0 then Tree.Empty
      else
        val start = sortedSpans.head.from.offset
        val end = sortedSpans.last.to.offset
        val centerPoint = (start + end) / 2
        val toTheLeft = sortedSpans.takeWhile(_.to.offset < centerPoint)
        val toTheRight = sortedSpans.dropWhile(_.from.offset < centerPoint)
        val overlapping = sortedSpans.filter(s =>
          s.from.offset <= centerPoint && s.to.offset >= centerPoint
        )
        Tree.Split(
          centerPoint,
          split(toTheLeft),
          split(toTheRight),
          overlapping.toList
        )

    val sorted =
      mp.keys.toVector.sortBy(_.from.offset)

    val data = split(sorted)

    Impl(data, mp)

  private class Impl[T](tree: Tree, mp: Map[Span, T]) extends IntervalTree[T]:
    override def resolve(offset: Int): List[T] =
      import Tree.*
      def go(t: Tree): List[Span] =
        t match
          case Split(point, left, right, in) =>
            if offset == point then in
            else if offset > point then
              in.filter(_.to.offset >= offset) ++ go(right)
            else if offset < point then
              in.filter(_.from.offset <= offset) ++ go(left)
            else Nil
          case Leaf(span) =>
            if span.from.offset > offset || span.to.offset < offset
            then Nil
            else List(span)
          case Empty => Nil

      go(tree).flatMap(mp.get)

  private enum Tree:
    case Split(point: Int, left: Tree, right: Tree, in: List[Span])
    case Leaf(span: Span)
    case Empty
