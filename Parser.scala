import parsley.Parsley
import parsley.Parsley.{atomic, pure, empty}
import parsley.character.{char, string, satisfy}
import parsley.combinator.{many, some, sepBy, sepBy1, option, choice, eof}
import parsley.position.{pos, offset, line, col}
import parsley.syntax.character.{charLift, stringLift}
import parsley.lift.{lift2, lift3}

object SpannedTree extends Metadata[WithSpan]

case class Caret(line: Int, col: Int, offset: Int)

case class Span(from: Caret, to: Caret):
  def contains(c: Caret) =
    val before =
      (from.line < c.line) || (from.line == c.line && from.col <= c.col)
    val after = (to.line > c.line) || (to.line == c.line && to.col >= c.col)

    before && after

case class WithSpan[A](span: Span, value: A):
  def map[B](f: A => B): WithSpan[B] = copy(value = f(value))

private val caret: Parsley[Caret] =
  lift3[Int, Int, Int, Caret]((l, c, o) => Caret(l - 1, c - 1, o), line, col, offset)

private def withSpan[A](p: Parsley[A]): Parsley[WithSpan[A]] =
  lift3[Caret, A, Caret, WithSpan[A]]((start, a, end) => WithSpan(Span(start, end), a), caret, p, caret)

extension [A](p: Parsley[A])
  def spanned: Parsley[WithSpan[A]] =
    lift3[Caret, A, Caret, WithSpan[A]]((start, a, end) => WithSpan(Span(start, end), a), caret, p, caret)

import SpannedTree.*

object parsers:
  private val sp = char(' ')
  private val lfChar = char('\n')
  private val crlfStr = string("\r\n")

  // atoms and atom-like things
  lazy val fieldName: Parsley[FieldName] = alphanumeric.map(FieldName.apply(_))

  lazy val structName: Parsley[Struct] = alphanumeric.map(Struct.apply(_))

  lazy val const: Parsley[Atom.Const] = alphanumeric.map(Atom.Const(_))
  lazy val id: Parsley[Id] = integer.map(Id.apply(_))
  lazy val ref: Parsley[Atom.Ref] = (EXCL *> id).map(Atom.Ref.apply(_): Atom.Ref)

  lazy val num: Parsley[Atom.Num] =
    val withType = lift2[Char, Int, String]((c, i) => s"$c$i", satisfy(c => c == 'i' || c == 'u'), integer)
    val typed = lift2[String, Int, Atom.Num]((sz, i) => Atom.Num(i, sz), withType <* many(sp).void, integer)
    val untyped: Parsley[Atom.Num] = integer.map(Atom.Num.apply(_, "i32"))
    typed <|> untyped

  lazy val stringExpr: Parsley[Atom.Str] =
    val quotedStr: Parsley[Atom.Str] = (char('"') *> many(satisfy(_ != '"')) <* char('"')).map(cs => Atom.Str(cs.mkString))
    val withExcl: Parsley[Atom.Str] = atomic(EXCL *> quotedStr)
    withExcl <|> quotedStr

  // composite expressions

  // expression
  lazy val expr: Parsley[Expression[WithSpan]] = {
    lazy val e: Parsley[Expression[WithSpan]] = many(sp) *> expr <* many(sp)
    lazy val eSpanned: Parsley[WithSpan[Expression[WithSpan]]] = withSpan(e)

    lazy val bag: Parsley[Bag] = EXCL *>
      (LB *> sepBy(eSpanned, COMMA) <* RB)
        .map(_.toVector)
        .map(Bag.apply)

    lazy val fieldValue: Parsley[Field.KeyValue] =
      lift2[FieldName, WithSpan[Expression[WithSpan]], Field.KeyValue](
        (k, v) => Field.KeyValue(k, v),
        fieldName <* COLON,
        eSpanned
      )

    lazy val named: Parsley[NamedData] = lift2[Struct, Vector[Field.KeyValue], NamedData](
      (s, f) => NamedData(s, f),
      EXCL *> structName,
      LP *> sepBy(many(sp) *> fieldValue <* many(sp), COMMA).map(_.toVector) <* RP
    )

    lazy val distinctNamed: Parsley[Distinct] =
      withSpan(string("distinct") *> some(sp).void *> named).map(Distinct.apply)

    choice(
      distinctNamed,
      atomic(ref),
      atomic(bag),
      atomic(named),
      num,
      const,
      stringExpr
    )
  }

  // assignment
  lazy val debugAssignment: Parsley[Statement.Assignment] =
    lift2[WithSpan[Atom.Ref], WithSpan[Expression[WithSpan]], Statement.Assignment](
      (rf, expr) => Statement.Assignment(rf, expr),
      many(sp) *> withSpan(ref) <* option(sp) <* ASS,
      many(sp) *> withSpan(expr) <* many(sp)
    )

  // whole section
  lazy val sep: Parsley[Unit] = (crlfStr <|> lfChar).void

  lazy val statement: Parsley[Either[String, WithSpan[Statement]]] =
    atomic(withSpan(debugAssignment).map(ws => ws.map(s => s: Statement)).map(Right(_))) <|>
      many(satisfy(_ != '\n')).map(cs => Left(cs.mkString))

  lazy val statements: Parsley[List[Either[String, WithSpan[Statement]]]] =
    many(sep) *> sepBy(statement, some(sep)) <* many(sep)

  lazy val program: Parsley[Program] = statements
    .map(_.toVector)
    .map(_.collect { case Right(v) => v })
    .map(Program.apply)

  val LP: Parsley[Char]        = char('(')
  val RP: Parsley[Char]        = char(')')
  val LB: Parsley[Char]        = char('{')
  val RB: Parsley[Char]        = char('}')
  val ASS: Parsley[Char]       = char('=')
  val SEMICOLON: Parsley[Char] = char(';')
  val COLON: Parsley[Char]     = char(':')
  val EXCL: Parsley[Char]      = char('!')
  val COMMA: Parsley[Char]     = char(',')

  lazy val integer: Parsley[Int] = some(satisfy(_.isDigit)).map(_.mkString.toInt)

  lazy val alphanumeric: Parsley[String] =
    lift2[Char, List[Char], String](
      (first, rest) => first.toString + rest.mkString,
      satisfy(_.isLetter),
      many(satisfy(c => c.isLetterOrDigit || c == '_'))
    )

  class ParsingError(caret: Caret, text: String)
      extends Exception(
        s"Parsing failed at line ${caret.line} column ${caret.col}"
      ):
    def render =
      val line = text.linesIterator.drop(caret.line)
      val nxt  = (" " * caret.col) + Console.RED + "^" + Console.RESET
      line.next() + "\n" + nxt

  def parse(s: String): Either[ParsingError, Program] =
    parseWithPosition(program, s)

  def parseExpr(s: String): Either[ParsingError, Expression[WithSpan]] =
    parseWithPosition(expr, s)

  private def parseWithPosition[A](
      p: Parsley[A],
      s: String
  ): Either[ParsingError, A] =
    p.parse(s) match
      case parsley.Success(result) => Right(result)
      case parsley.Failure(err) =>
        // Extract position from error message - Parsley provides line/col info
        var lineNum = 0
        var colNum = 0
        var offsetNum = 0
        // Parse error position from Parsley's error message format
        val linePattern = "line (\\d+)".r
        val colPattern = "column (\\d+)".r
        linePattern.findFirstMatchIn(err.toString).foreach(m => lineNum = m.group(1).toInt - 1)
        colPattern.findFirstMatchIn(err.toString).foreach(m => colNum = m.group(1).toInt - 1)
        Left(ParsingError(Caret(lineNum, colNum, offsetNum), s))

end parsers
