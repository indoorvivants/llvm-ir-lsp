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
  val ref = (EXCL *> id).map(Atom.Ref.apply(_): Atom.Ref)
  val num = (((P.charIn('i', 'u') ~ integer)
    .map((c, i) => s"$c$i") <* sp.rep.void) ~ integer).map((sz, i) =>
    Atom.Num(i, sz)
  ) orElse integer.map(Atom.Num.apply(_, "i32"))
  val string =
    (EXCL *> P
      .charsWhile(_ != '"')
      .surroundedBy(P.char('"'))
      .map(Atom.Str.apply)).backtrack orElse
      P.charsWhile(_ != '"').surroundedBy(P.char('"')).map(Atom.Str.apply)

  // composite expressions

  // expression
  val expr = P.recursive[Expression[WithSpan]] { recurse =>
    val e = recurse.surroundedBy(sp.rep0)

    val bag = EXCL *>
      e.span
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
  val debugAssignment =
    ((withSpan(ref) <* sp.? <* ASS) ~ withSpan(expr).surroundedBy(sp.rep0))
      .map((rf, expr) => Statement.Assignment.apply(rf, expr))
      .surroundedBy(sp.rep0)

  // whole section
  val sep = crlf orElse lf

  val statement =
    withSpan(debugAssignment).map(Right(_)).backtrack orElse
      P.charsWhile(_ != '\n').map(Left(_))

  val statements = statement
    .repSep0(sep.rep)
    .surroundedBy(sep.rep0)

  val program = statements
    .map(_.toVector)
    .map(_.collect { case Right(v) => v })
    .map(Program.apply)

  inline def LP = P.char('(')
  inline def RP = P.char(')')
  inline def LB = P.char('{')
  inline def RB = P.char('}')
  inline def ASS = P.char('=')
  inline def SEMICOLON = P.char(';')
  inline def COLON = P.char(':')
  inline def EXCL = P.char('!')
  inline def COMMA = P.char(',')

  inline def integer = P.charsWhile(_.isDigit).map(_.toInt)

  inline def alphanumeric =
    (P.charWhere(_.isLetter) ~ P.charsWhile0(c =>
      c.isLetterOrDigit || c == '_'
    )).map(_.toString + _)

  class ParsingError(caret: Caret, text: String) extends Exception(s"Parsing failed at line ${caret.line} column ${caret.col}") {
      def render = 
        val line = text.linesIterator.drop(caret.line)
        val nxt = (" " * caret.col) + Console.RED + "^" + Console.RESET
        line.next() + "\n" + nxt
  }

  def parse(s: String): Either[ParsingError, Program] =
    parseWithPosition(program, s)

  def parseExpr(s: String): Either[ParsingError, Expression[WithSpan]] =
    parseWithPosition(expr, s)

  // def parseStatement(
  //     s: String
  // ): Either[ParsingError, WithSpan[Statement]] =
  //   parseWithPosition(statement, s)

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

// @main def hello =
//   import parsers._
//   def p(str: String) = parse(str) match
//     case Left(value)  => println(value)
//     case Right(value) => println(value)

//   // p("!25 = DWARF")
//   // p("!25 = \"DWARF\"")
//   // p("!25 = 25")
//   // p("!25 = !25")
//   // p("!25 = i8 5")
//   // p("!25 = !DiLocalVariable(a: !{25})")
//   // p("!25 = distinct !DiLocalVariable(a: 5)")
//   // p("!25 = !{ i32 5, \"Dwarf Version\", 3}")

//   val lines = """
// !0 = distinct !DICompileUnit(language: DW_LANG_C_plus_plus_14, file: !1, producer: "clang version 15.0.0 (https://github.com/llvm/llvm-project.git 4ba6a9c9f65bbc8bd06e3652cb20fd4dfc846137)", isOptimized: false, runtimeVersion: 0, emissionKind: FullDebug, splitDebugInlining: false, nameTableKind: None)
// !1 = !DIFile(filename: "/app/example.cpp", directory: "/app")
// !2 = !{i32 7, !"Dwarf Version", i32 4}
// !3 = !{i32 2, !"Debug Info Version", i32 3}
// !4 = !{i32 1, !"wchar_size", i32 4}
// !5 = !{i32 7, !"PIC Level", i32 2}
// !6 = !{i32 7, !"PIE Level", i32 2}
// !7 = !{i32 7, !"uwtable", i32 2}
// !8 = !{i32 7, !"frame-pointer", i32 2}
// !9 = !{!"clang version 15.0.0 (https://github.com/llvm/llvm-project.git 4ba6a9c9f65bbc8bd06e3652cb20fd4dfc846137)"}
// !10 = distinct !DISubprogram(name: "t", linkageName: "_Z1tf", scope: !11, file: !11, line: 2, type: !12, scopeLine: 2, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !15)
// !11 = !DIFile(filename: "example.cpp", directory: "/app")
// !12 = !DISubroutineType(types: !13)
// !13 = !{!14, !14}
// !14 = !DIBasicType(name: "float", size: 32, encoding: DW_ATE_float)
// !15 = !{}
// !16 = !DILocalVariable(name: "b", arg: 1, scope: !10, file: !11, line: 2, type: !14)
// !17 = !DILocation(line: 2, column: 15, scope: !10)
// !18 = !DILocation(line: 3, column: 12, scope: !10)
// !19 = !DILocation(line: 3, column: 14, scope: !10)
// !20 = !DILocation(line: 3, column: 5, scope: !10)
// !21 = distinct !DISubprogram(name: "square", linkageName: "_Z6squarei", scope: !11, file: !11, line: 6, type: !22, scopeLine: 6, flags: DIFlagPrototyped, spFlags: DISPFlagDefinition, unit: !0, retainedNodes: !15)
// !22 = !DISubroutineType(types: !23)
// !23 = !{!24, !24}
// !24 = !DIBasicType(name: "int", size: 32, encoding: DW_ATE_signed)
// !25 = !DILocalVariable(name: "num", arg: 1, scope: !21, file: !11, line: 6, type: !24)
// !26 = !DILocation(line: 6, column: 16, scope: !21)
// !27 = !DILocalVariable(name: "x", scope: !21, file: !11, line: 7, type: !14)
// !28 = !DILocation(line: 7, column: 11, scope: !21)
// !29 = !DILocation(line: 7, column: 17, scope: !21)
// !30 = !DILocation(line: 7, column: 15, scope: !21)
// !31 = !DILocation(line: 8, column: 12, scope: !21)
// !32 = !DILocation(line: 8, column: 18, scope: !21)
// !33 = !DILocation(line: 8, column: 16, scope: !21)
// !34 = !DILocation(line: 8, column: 5, scope: !21)
// """.trim.linesIterator.foreach { l =>
//     println(Console.BOLD + l + Console.RESET)
//     p(l)
//   }
