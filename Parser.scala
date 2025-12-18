import parsley.Parsley
import parsley.Parsley.atomic
import parsley.Parsley.many
import parsley.Parsley.some
import parsley.character
import parsley.character.char
import parsley.character.satisfy
import parsley.character.string
import parsley.combinator.choice
import parsley.combinator.option
import parsley.combinator.sepBy
import parsley.debug.combinator
import parsley.position.col
import parsley.position.line
import parsley.position.offset
import parsley.syntax.character.charLift
import parsley.syntax.character.stringLift
import parsley.syntax.zipped.*

case class Caret(line: Int, col: Int, offset: Int)

extension [A](p: Parsley[A])
  def named(v: String) = p // parsley.debug.combinator.named(p, v)

case class Span(from: Caret, to: Caret):
  def contains(c: Caret) =
    val before =
      (from.line < c.line) || (from.line == c.line && from.col <= c.col)
    val after = (to.line > c.line) || (to.line == c.line && to.col >= c.col)

    before && after

case class WithSpan[A](span: Span, value: A):
  def map[B](f: A => B): WithSpan[B] = copy(value = f(value))

private val caret: Parsley[Caret] =
  (line.map(_ - 1), col.map(_ - 1), offset).zipped.map(Caret.apply)

private def withSpan[A](p: => Parsley[A]): Parsley[WithSpan[A]] =
  (caret, p, caret).zipped((s, a, e) => WithSpan(Span(s, e), a))

extension [A](p: => Parsley[A])
  def spanned: Parsley[WithSpan[A]] =
    withSpan(p)

class ParsingError(caret: Caret, text: String)
    extends Exception(
      s"Parsing failed at line ${caret.line} column ${caret.col}"
    ):

  override lazy val toString = text

  // def render =
  //   val line = text.linesIterator.drop(caret.line)
  //   val nxt  = (" " * caret.col) + Console.RED + "^" + Console.RESET
  //   line.next() + "\n" + nxt
end ParsingError

@parsley.debuggable
trait Parsers[F[_]](val tree: Metadata[F]):
  extension [A](p: => Parsley[A]) def spanned: Parsley[F[A]]

  import tree.*

  // atoms and atom-like things
  lazy val fieldName: Parsley[FieldName] = alphanumeric.map(FieldName.apply)
  lazy val structName: Parsley[Struct]   = alphanumeric.map(Struct.apply)
  lazy val const: Parsley[Atom.Const]    = alphanumeric.map(Atom.Const.apply)
  lazy val id: Parsley[Id]               = integer.map(Id.apply)
  lazy val ref: Parsley[Atom.Ref] =
    ('!' *> id).map(Atom.Ref.apply)

  lazy val num: Parsley[Atom.Num] =
    val withType =
      ('i' <|> 'u', integer).zipped.map((c, i) => s"$c$i")

    val typed: Parsley[Atom.Num] =
      (withType <* character.whitespaces, integer).zipped.map((sz, i) =>
        Atom.Num(i, sz)
      )

    val untyped: Parsley[Atom.Num] = integer.map(Atom.Num(_, "i32"))

    (typed <|> untyped).named("numeric literal")
  end num

  lazy val stringExpr: Parsley[Atom.Str] =
    val quotedStr: Parsley[Atom.Str] =
      ('"' *> many(satisfy(_ != '"')) <* '"').map(cs => Atom.Str(cs.mkString))

    val withExcl: Parsley[Atom.Str] = atomic(EXCL *> quotedStr)

    withExcl <|> quotedStr
  end stringExpr

  // composite expressions

  lazy val bag: Parsley[Bag] = EXCL *>
    (LB *> sepBy(expr.spanned, COMMA) <* RB)
      .map(_.toVector)
      .map(Bag.apply)

  lazy val fieldValue: Parsley[Field.KeyValue] =
    (fieldName <* COLON, expr.spanned).zipped.map(Field.KeyValue.apply)

  lazy val namedData: Parsley[NamedData] =
    (
      EXCL *> structName,
      LP *> sepBy(lexeme(fieldValue), COMMA)
        .map(_.toVector) <* RP
    ).zipped.map(NamedData.apply)

  lazy val distinctNamed: Parsley[Distinct] =
    (string("distinct") *> lexeme(namedData).spanned)
      .map(Distinct.apply)

  // expression
  lazy val expr: Parsley[Expression[F]] =
    lexeme(
      choice(
        distinctNamed,
        atomic(ref),
        atomic(bag),
        atomic(namedData),
        num,
        const,
        stringExpr
      )
    )

  end expr

  private val skipWhitespace = character.spaces.void

  private def lexeme[A](p: Parsley[A]): Parsley[A] =
    p <~ skipWhitespace
  private def token[A](p: Parsley[A]): Parsley[A]  = lexeme(atomic(p))
  private def symbol(str: String): Parsley[String] = atomic(string(str))

  val nonEmptyString =
    token(character.stringOfSome(c => c != ' ' && c != '(' && c != ')').spanned)
      .named("nonEmptyString")
  val functionName       = llvmIdentifier.spanned
  val functionReturnType = nonEmptyString.named("functionReturnType")

  val localID =
    token(character.stringOfSome(c => c != ' ' && c != '(' && c != ')'))
      .map(LocalID.apply)
      .named("localID")

  val functionArg =
    (
      nonEmptyString.map(ArgumentType.apply),
      option(token("noundef")) *> "%" *> character
        .stringOfSome(character.digit)
        .spanned
    )
      .zipped(FunctionArgument.apply)
      .named("functionArg")

  val functionArguments = sepBy(functionArg, ",").named("functionArguments")

  val debugAttachment =
    many(token("!dbg " *> ref.spanned)).named("debugAttachment")

  enum LinkageAttr:
    case `private`, internal, available_externally, linkonce, weak, common,
      appending, extern_weak, linkonce_odr, weak_odr, external

  val linkageAttr =
    LinkageAttr.values
      .map(_.toString())
      .map(token)
      .reduce(_ | _)
      .named("linkageAttr")

  enum ParameterAttrSimple:
    case noalias, nocapture, noredzone, noinline, nounwind, optnone, optsize,
      readnone, readonly, returns_twice, ssp, sspreq, sspstrong, swiftself,
      swifterror, writeonly, noundef

  val parameterAttr = ( // TODO: verify
    choice(
      ParameterAttrSimple.values.map(_.toString()).map(token).reduce(_ | _),
      (
        token("dereferenceable_or_null("),
        integer,
        token(")")
      ).zipped(_ + _.toString + _)
    )
  ).named("parameterAttr")

  enum PreemptionAttr:
    case dso_local, dso_preemptable

  val preemptionAttr =
    PreemptionAttr.values
      .map(_.toString())
      .map(token)
      .reduce(_ | _)
      .named("preemptionAttr")

  enum VisibilityAttr:
    case default, hidden, `protected`

  val visibilityAttr =
    VisibilityAttr.values
      .map(_.toString())
      .map(token)
      .reduce(_ | _)
      .named("visibilityAttr")

  val functionAttrs =
    (
      option(linkageAttr),    // linkage
      option(preemptionAttr), // preemption specifier
      option(visibilityAttr), // visibility
      option(parameterAttr)
    ).zipped.named("functionAttrs")

  val someRandomBullshit = token("#" *> integer).named("attributesAttachment")

  val intrinsic =
    choice("call", "load", "alloca", "store", "fdiv", "ret")

  val ws = character.spaces

  lazy val llvmIdentifier =
    // [%@][-a-zA-Z$._][-a-zA-Z$._0-9]*
    val first = (c: Char) =>
      c == '-' || (c >= 'a' && c <= 'z') ||
        (c >= 'A' && c <= 'Z') ||
        c == '$' ||
        c == '_' ||
        c == '.'

    ("@" *> choice(
      '"' *> character.stringOfSome(_ != '"') <* '"',
      (
        character.satisfy(first),
        character.stringOfMany(character.satisfy(c => first(c) || c.isDigit))
      ).zipped(_ +: _)
    )).named("llvmIdentifier")
  end llvmIdentifier

  object instructions:
    object call:
      private lazy val tail = choice("tail", "musttail", "notail")
      private lazy val params =
        "(" *>
          parsley.combinator.sepBy(
            choice(
              lexeme("ptr") *>
                alphanumeric.spanned.map(FunctionCallParam.Ptr.apply) <* ws,
              ws *> num.map(atom =>
                FunctionCallParam.Num(atom.value, atom.tpe)
              ) <* ws
            ),
            ","
          ) <* ")"

      lazy val instr =
        (
          "call" <* ws,
          option(tail) <* ws,
          many(parameterAttr) <* ws,
          functionReturnType <* ws,
          llvmIdentifier.spanned <* ws,
          params.named("call params") <* ws,
          option("," *> ws *> debugAttachment)
        ).zipped:
          case (_, tail, attrs, retType, funcName, params, debug) =>
            Instruction.Call(tail, retType, funcName, params.toVector)
    end call
  end instructions

  lazy val funcOp =
    choice(
      instructions.call.instr,
      character.stringOfMany(_ != '\n').map(Instruction.Unknown.apply)
    ).named("operation")

  lazy val funcAssign =
    (
      "%" *> localID.spanned <* ws,
      "=" *> ws *> funcOp
    ).zipped((i, op) => BodyOperation.Assignment(i, op)).named("assignment")

  lazy val functionBody =
    token("{") *>
      character.newline *>
      parsley.combinator.manyTill(
        ws *>
          choice(
            funcAssign,
            funcOp.map(BodyOperation.Instr.apply)
          ) <*
          some(character.newline),
        token("}")
      )

  lazy val functionType =
    (
      functionReturnType,
      functionName,
      token("(") *> functionArguments <* token(")"),
      option(someRandomBullshit),
      atomic(debugAttachment)
    )
      .zipped:
        case (retType, name, args, _, dbg) =>
          Function(name, args.toVector, retType, dbg.toVector)

  lazy val functionDefinition =
    token("define") *> (
      functionAttrs,
      functionType,
      functionBody.map(_.toVector)
    ).zipped(Statement.FunctionDefinition.apply)
      .named("functionDefinition")

  val lineComment = ws *> ";" *> character
    .stringOfSome(_ != '\n')
    .spanned
    .map(Statement.LineComment.apply)
    .named("lineComment")

  lazy val debugAssignment: Parsley[Statement] =
    (
      lexeme(ref).spanned <* token(ASS),
      lexeme(expr).spanned
    ).zipped(Statement.MetadataAssignment.apply)
      .named("debugAssignment")

  lazy val statement: Parsley[Statement] =
    choice(
      debugAssignment,
      functionDefinition,
      lineComment,
      parsley.combinator
        .manyTill(character.item, character.endOfLine)
        .void
        .as(Statement.Unknown(""))
    ).named("statement")

  lazy val statements: Parsley[List[Statement]] =
    parsley.combinator.manyTill(statement <* character.whitespaces, Parsley.eof)

  lazy val program: Parsley[Program] = statements
    .map(_.toVector.filter:
      case Statement.Unknown(raw) if raw.isBlank() => false
      case _                                       => true)
    .map(Program.apply)

  val LP: Parsley[Char]        = '('
  val RP: Parsley[Char]        = ')'
  val LB: Parsley[Char]        = '{'
  val RB: Parsley[Char]        = '}'
  val ASS: Parsley[Char]       = '='
  val SEMICOLON: Parsley[Char] = ';'
  val COLON: Parsley[Char]     = ':'
  val EXCL: Parsley[Char]      = '!'
  val COMMA: Parsley[Char]     = ','

  lazy val integer: Parsley[Int] =
    character.stringOfSome(character.digit).map(_.toInt)

  lazy val alphanumeric: Parsley[String] =
    (
      character.letter,
      character.stringOfMany(character.letterOrDigit <|> '_')
    ).zipped(_ +: _)

  def parse(s: String): Either[ParsingError, Program] =
    parseWithPosition(program, s)

  def parse[A](s: String, parser: Parsley[A]): Either[ParsingError, A] =
    parseWithPosition(parser, s)

  def parseExpr(s: String): Either[ParsingError, Expression[F]] =
    parseWithPosition(expr, s)

  private def parseWithPosition[A](
      p: Parsley[A],
      s: String
  ): Either[ParsingError, A] =
    p.parse(s) match
      case parsley.Success(result) => Right(result)
      case parsley.Failure(err)    =>
        // Extract position from error message - Parsley provides line/col info
        var lineNum   = 0
        var colNum    = 0
        var offsetNum = 0
        // Parse error position from Parsley's error message format
        val linePattern = "line (\\d+)".r
        val colPattern  = "column (\\d+)".r
        linePattern
          .findFirstMatchIn(err.toString)
          .foreach(m => lineNum = m.group(1).toInt - 1)
        colPattern
          .findFirstMatchIn(err.toString)
          .foreach(m => colNum = m.group(1).toInt - 1)
        Left(ParsingError(Caret(lineNum, colNum, offsetNum), err))
end Parsers

object SpannedParsers extends Parsers(SpannedTree):
  extension [A](p: => Parsley[A])
    override def spanned: Parsley[WithSpan[A]] =
      withSpan(p)
end SpannedParsers

object PureParsers extends Parsers(PureTree):
  extension [A](p: => Parsley[A]) override def spanned: Parsley[A] = p
end PureParsers
