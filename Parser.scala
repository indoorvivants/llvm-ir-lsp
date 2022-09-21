//> using lib "org.typelevel::cats-parse::0.3.8"
//> using scala "3.2.0"

import cats.parse.Parser as P
import cats.parse.Parser0 as P0
import cats.parse.Caret
import cats.parse.Rfc5234.{sp, alpha, digit, crlf, lf}

object SpannedTree extends Module[WithSpan]

case class Span(from: Caret, to: Caret):
  def contains(c: Caret) =
    val before =
      (from.line < c.line) || (from.line == c.line && from.col <= c.col)
    val after = (to.line > c.line) || (to.line == c.line && to.col >= c.col)

    before && after

case class WithSpan[A](span: Span, value: A) {
  def map[B](f: A => B): WithSpan[B] = copy(value = f(value))
}

private def withSpan[A](p: P[A]): P[WithSpan[A]] =
  (P.caret.with1 ~ p ~ P.caret).map { case ((start, a), end) =>
    WithSpan(Span(start, end), a)
  }

extension [A](p: P[A])
  def span: P[WithSpan[A]] =
    (P.caret.with1 ~ p ~ P.caret).map { case ((start, a), end) =>
      WithSpan(Span(start, end), a)
    }

import SpannedTree._

object parsers:
  // atoms and atom-like things
  val fieldName = alphanumeric.map(FieldName.apply(_))

  val structName = alphanumeric.map(Struct.apply(_))

  val const = alphanumeric.map(Atom.Const(_))
  val id = integer.map(Id.apply(_))
  val ref = (P.char('!') *> id).map(Atom.Ref.apply(_): Atom.Ref)
  val num = (((P.charIn('i', 'u') ~ integer)
    .map((c, i) => s"$c$i") <* sp.rep.void) ~ integer).map((sz, i) =>
    Atom.Num(i, sz)
  ) orElse integer.map(Atom.Num.apply(_, "i32"))
  val string =
    P.charsWhile(_ != '"').surroundedBy(P.char('"')).map(Atom.Str.apply)

  // composite expressions

  // expression
  val expr = P.recursive[Expression[WithSpan]] { recurse =>
    val e = recurse.surroundedBy(sp.rep0)
    val bag = EXCL *>
      recurse.span
        .surroundedBy(sp.rep0)
        .repSep0(COMMA)
        .between(LB, RB)
        .map(_.toVector)
        .map(Bag.apply)

    val fieldValue =
      ((fieldName <* COLON) ~ e.span).map((k, v) => Field.KeyValue(k, v))

    inline def named = ((EXCL *> structName) ~ fieldValue
      .surroundedBy(sp.rep0)
      .repSep0(COMMA)
      .map(_.toVector)
      .between(LP, RP)).map((s, f) => NamedData(s, f))

    inline def distinctNamed =
      (P.string("distinct") *> sp.rep.void *> named).span.map(Distinct.apply)

    P.oneOf(
      distinctNamed :: ref.backtrack :: bag.backtrack ::
        named :: num :: const :: string :: Nil
    )
  }

  // assignment
  val assignment: P[Statement] =
    ((withSpan(ref) <* sp.? <* ASS) ~ withSpan(expr))
      .map((rf, expr) => Statement.Assignment.apply(rf, expr))
      .surroundedBy(sp.rep0)

  // whole section
  val sep = crlf orElse lf

  val statement = withSpan(assignment)

  val statements = statement
    .repSep0(sep.rep)
    .surroundedBy(sep.rep0)

  val program = statements.map(_.toVector).map(Program.apply)

  inline def LP = P.char('(').surroundedBy(sp.rep0)
  inline def RP = P.char(')').surroundedBy(sp.rep0)
  inline def LB = P.char('{').surroundedBy(sp.rep0)
  inline def RB = P.char('}').surroundedBy(sp.rep0)
  inline def ASS = P.char('=').surroundedBy(sp.rep0)
  inline def SEMICOLON = P.char(';')
  inline def COLON = P.char(':')
  inline def EXCL = P.char('!')
  inline def COMMA = P.char(',')

  inline def integer = P.charsWhile(_.isDigit).map(_.toInt)

  inline def alphanumeric =
    (P.charWhere(_.isLetter) ~ P.charsWhile0(_.isLetter)).map(_.toString + _)

  case class ParsingError(caret: Caret, text: String) {
    override def toString() =
      val line = text.linesIterator.drop(caret.line)
      val nxt = (" " * caret.col) + Console.RED + "^" + Console.RESET
      line.next() + "\n" + nxt
  }

  def parse(s: String): Either[ParsingError, Program] =
    parseWithPosition(statements.map(_.toVector).map(Program.apply(_)), s)

  def parseExpr(s: String): Either[ParsingError, Expression[WithSpan]] =
    parseWithPosition(expr, s)

  def parseStatement(
      s: String
  ): Either[ParsingError, WithSpan[Statement]] =
    parseWithPosition(statement, s)

  private def parseWithPosition[A](
      p: P0[A] | P[A],
      s: String
  ): Either[ParsingError, A] =
    p.parseAll(s).left.map { err =>
      val offset = err.failedAtOffset
      var line = 0
      var col = 0
      (0 to (offset min s.length)).foreach { i =>
        if (s(i) == '\n') then
          line += 1
          col = 0
        else col += 1

      }

      ParsingError(Caret(line, col, offset), s)
    }

@main def hello =
  import parsers._
  def p(str: String) = parse(str) match
    case Left(value)  => println(value)
    case Right(value) => println(value)

  p("!25 = DWARF")
  p("!25 = \"DWARF\"")
  p("!25 = 25")
  p("!25 = !25")
  p("!25 = i8 5")
  p("!25 = !DiLocalVariable(a: 5)")
  p("!25 = distinct !DiLocalVariable(a: 5)")
  p("!25 = !{ i32 5, \"Dwarf Version\", 3}")
