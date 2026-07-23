package llvm_lsp

import parsley.Parsley
import parsley.Parsley.atomic
import parsley.Parsley.many
import parsley.Parsley.some
import parsley.character
import parsley.character.satisfy
import parsley.character.string
import parsley.combinator.choice
import parsley.combinator.option
import parsley.combinator.sepBy
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

class ParsingError(caret: Caret, text: String)
    extends Exception(
      text
    )
end ParsingError

private val skipWhitespace = character.spaces.void

private def lexeme[A](p: Parsley[A]): Parsley[A] =
  p <~ skipWhitespace
private def token[A](p: Parsley[A]): Parsley[A]  = lexeme(atomic(p))
private def symbol(str: String): Parsley[String] = atomic(string(str))

@parsley.debuggable
trait Parsers[F[_]](val tree: Metadata[F]):
  extension [A](p: => Parsley[A]) def spanned: Parsley[F[A]]

  import tree.*

  // atoms and atom-like things
  lazy val fieldName: Parsley[FieldName] = alphanumeric.map(FieldName.apply)
  lazy val structName: Parsley[Struct]   = alphanumeric.map(Struct.apply)
  lazy val const: Parsley[Atom.Const]    = alphanumeric.map(Atom.Const.apply)
  lazy val integerID: Parsley[Id]        = integer.map(Id.apply)
  lazy val stringID: Parsley[Id]         = llvmIdentifierNoQuotes.map(Id.apply)
  lazy val quotedID: Parsley[Id]         =
    ('"' *> character.stringOfSome(_ != '"') <* '"').map(Id.apply)
  lazy val ref: Parsley[Atom.Ref] =
    "!" *> choice(integerID.map(Atom.Ref(_)), stringID.map(Atom.Ref(_)))

  lazy val metadataAssignmentLhs: Parsley[Atom.Ref] =
    "!" *> choice(
      integerID.map(Atom.Ref(_)),
      quotedID.map(Atom.Ref(_)),
      stringID.map(Atom.Ref(_))
    )

  lazy val num: Parsley[Atom.Num] =
    val withType =
      ('i' <|> 'u', integer).zipped.map((c, i) => s"$c$i")

    val signedInt: Parsley[Int] =
      (
        option(character.char('-')),
        character.stringOfSome(character.digit)
      ).zipped: (sign, digits) =>
        val abs = digits.toLongOption.map(_.toInt).getOrElse(Int.MaxValue)
        if sign.isDefined then -abs else abs

    val floatTypeName: Parsley[String] =
      choice(
        symbol("float"),
        symbol("double"),
        symbol("half"),
        symbol("bfloat"),
        symbol("fp128"),
        symbol("x86_fp80")
      )

    // Accept float literals like 1.5, -1.5, 2.000000e+00, 0x7FF0000000000000.
    // We currently store them as Atom.Num truncated to Int — this is lossy but
    // matches the existing model. Keeping the type tag lets us round-trip later.
    val floatLiteral: Parsley[String] =
      val hexFloat =
        (atomic(string("0x")), character.stringOfSome(c =>
          (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')
        )).zipped(_ + _)
      val decFloat =
        (
          option(character.char('-')).map(_.fold("")(_.toString)),
          character.stringOfSome(character.digit),
          option(
            (character.char('.'), character.stringOfSome(character.digit))
              .zipped((d, ds) => d.toString + ds)
          ).map(_.getOrElse("")),
          option(
            (
              character.char('e') <|> character.char('E'),
              option(character.char('+') <|> character.char('-'))
                .map(_.fold("")(_.toString)),
              character.stringOfSome(character.digit)
            ).zipped((e, s, ds) => e.toString + s + ds)
          ).map(_.getOrElse(""))
        ).zipped(_ + _ + _ + _)
      atomic(hexFloat) <|> atomic(decFloat.filter(s => s.contains('.') || s.contains('e') || s.contains('E')))

    val typedFloat: Parsley[Atom.Num] =
      (floatTypeName <* ws, floatLiteral).zipped((tpe, _) => Atom.Num(0, tpe))

    // `i1 true` / `i1 false` — booleans, stored as i1 0/1 to fit the model.
    val typedBool: Parsley[Atom.Num] =
      (
        atomic(token("i1")),
        choice(string("true").as(1), string("false").as(0))
      ).zipped((_, v) => Atom.Num(v, "i1"))

    val typed: Parsley[Atom.Num] =
      (withType <* ws, signedInt).zipped.map((sz, i) => Atom.Num(i, sz))

    val untyped: Parsley[Atom.Num] = signedInt.map(Atom.Num(_, "i32"))

    choice(
      atomic(typedBool),
      atomic(typedFloat),
      atomic(typed),
      untyped
    ).named("numeric literal")
  end num

  lazy val stringExpr: Parsley[Atom.Str] =
    val quotedStr: Parsley[Atom.Str] =
      ('"' *> many(satisfy(_ != '"')) <* '"').map(cs => Atom.Str(cs.mkString))

    val withExcl: Parsley[Atom.Str] = atomic(EXCL *> quotedStr)

    withExcl <|> quotedStr
  end stringExpr

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
    (token("distinct") *> namedData.spanned)
      .map(Distinct.apply)

  // expression
  lazy val expr: Parsley[Expression[F]] =
    lexeme(
      choice(
        distinctNamed,
        atomic(namedData),
        atomic(ref),
        atomic(bag),
        num,
        const,
        stringExpr
      )
    )

  end expr

  lazy val debugAssignment: Parsley[Statement] =
    (
      lexeme(metadataAssignmentLhs).spanned <* token(ASS),
      lexeme(expr).spanned
    ).zipped(Statement.MetadataAssignment.apply)
      .named("debugAssignment")

  val nonEmptyString =
    token(character.stringOfSome(c => c != ' ' && c != '(' && c != ')').spanned)
      .named("nonEmptyString")
  val functionName       = llvmIdentifier.spanned
  val functionReturnType = nonEmptyString.named("functionReturnType")

  val localID =
    token(character.stringOfSome(c => c != ' ' && c != '(' && c != ')'))
      .map(LocalID.apply)
      .named("localID")

  object functions:
    // LLVM local identifier char class — same as `llvmRef` below.
    private val localNameChar = (c: Char) =>
      c == '-' || c == '$' || c == '_' || c == '.' ||
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
        (c >= '0' && c <= '9')

    val arg =
      (
        nonEmptyString.map(ArgumentType.apply),
        many(parameterAttr) *>
          "%" *> character.stringOfSome(character.satisfy(localNameChar))
            .spanned
      )
        .zipped(FunctionArgument.apply)
    val arguments = sepBy(ws *> arg, ",")

    // In `declare`, argument names are optional and parameter attributes can
    // appear between type and name. When the name is absent we synthesise an
    // empty spanned string at the position where the name would have appeared.
    val declareArgType =
      token(
        character
          .stringOfSome(c =>
            c != ' ' && c != '(' && c != ')' && c != ',' && c != '\n'
          )
          .spanned
      )
    val declareArg =
      (
        declareArgType.map(ArgumentType.apply),
        many(parameterAttr) *>
          option("%" *> character.stringOfSome(character.digit))
            .map(_.getOrElse(""))
            .spanned
      ).zipped(FunctionArgument.apply)
    val declareArguments = sepBy(ws *> declareArg, ",")

    object call:
      val numeric =
        num.map(atom => // TODO: provide a generic parser for LLVM atoms
          FunctionCallParam.Num(atom.value, atom.tpe)
        )

      val ptr = token("ptr") *> many(parameterAttr) *>
        choice(
          llvmRef.spanned.map(FunctionCallParam.Ptr.apply),
          // Global reference: @foo or @"quoted.name"
          llvmIdentifier.spanned.map(FunctionCallParam.Ptr.apply),
          string("null").as(NULL).map(FunctionCallParam.Ptr.apply)
        )

      val metaRef = token("metadata") *>
        Parsley.lookAhead(string("!")) *> expr.map(
          FunctionCallParam.Metadata.apply
        )

      val scalarType: Parsley[String] =
        val iType =
          atomic(('i' <|> 'u', integer).zipped.map((c, i) => s"$c$i"))
        choice(
          iType,
          symbol("float"),
          symbol("double"),
          symbol("half"),
          symbol("bfloat"),
          symbol("fp128"),
          symbol("x86_fp80"),
          symbol("void")
        )

      val typedRef = atomic(
        (
          token(scalarType) <* many(parameterAttr),
          llvmRef.spanned
        ).zipped
          .map(FunctionCallParam.TypedRef.apply)
      )

      val param =
        atomic(metaRef) <|> // TODO this is inefficient
          (option(token("metadata")) <* ws, choice(ptr, typedRef, numeric))
            .zipped:
              case (None, param) =>
                param
              case (Some(_), param) =>
                FunctionCallParam.Metadata(param)

    end call

  end functions

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

  // Known bare LLVM parameter/function attributes. Kept as an allowlist so
  // that `many(parameterAttr)` running BEFORE `functionReturnType` (in
  // `functionType`) can't accidentally swallow the return type name (e.g.
  // `ptr`, `void`, `i32`, `double`, or a `%struct.T` alias). Extend as new
  // attributes are encountered.
  enum ParameterAttrSimple:
    case noalias, nocapture, noredzone, noinline, nounwind, optnone, optsize,
      readnone, readonly, returns_twice, ssp, sspreq, sspstrong, swiftself,
      swifterror, writeonly, noundef, zeroext, nonnull, signext, inreg,
      immarg, allocptr, writable, alwaysinline, inlinehint, hot, cold, willreturn,
      mustprogress, speculatable, sanitize_address, sanitize_memory,
      sanitize_thread, sanitize_hwaddress, sanitize_memtag, uwtable, naked,
      nofree, nosync, nocallback, nomerge, norecurse, noreturn, disable_sanitizer_instrumentation,
      strictfp, argmemonly, inaccessiblememonly, inaccessiblemem_or_argmemonly,
      builtin, convergent, presplitcoroutine, minsize, safestack, shadowcallstack,
      speculative_load_hardening, nocf_check, notail, jumptable, noprofile,
      nosanitize_bounds, nosanitize_coverage, allocsize, allockind, byval, byref,
      preallocated, sret, elementtype, nofpclass, no_sanitize_address,
      no_sanitize_hwaddress, no_sanitize_memtag, no_sanitize_thread,
      no_sanitize_bounds

  // Any identifier from the LLVM attribute char class. Used only inside
  // `<ident>(<args>)` — never as a bare match, to avoid ambiguity with
  // types and other keywords.
  private val attrIdent: Parsley[String] =
    character.stringOfSome(character.satisfy(c =>
      (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
        (c >= '0' && c <= '9') || c == '_' || c == '.' || c == '-'
    ))

  // Body of a parenthesised attribute argument — allows nested parens, braces,
  // and double-quoted strings. Consumes up to the matching close paren.
  private lazy val parenBalanced: Parsley[String] =
    val chunk: Parsley[String] = choice(
      character.stringOfSome(c =>
        c != '(' && c != ')' && c != '{' && c != '}' && c != '"'
      ).map(identity),
      atomic(
        ('"' *> character.stringOfMany(_ != '"') <* '"').map(s => s"\"$s\"")
      ),
      atomic(('(' *> parenBalanced <* ')').map(s => s"($s)")),
      atomic(('{' *> parenBalancedBraces <* '}').map(s => s"{$s}"))
    )
    many(chunk).map(_.mkString)

  // Same as parenBalanced but named for reading calls that need to distinguish
  // opening braces at the top level. Same grammar body.
  private lazy val parenBalancedBraces: Parsley[String] = parenBalanced

  val parameterAttr = choice(
    // Any `<ident>(<balanced>)` form — covers `dereferenceable(N)`,
    // `dereferenceable_or_null(N)`, `captures(...)`, `align(N)`,
    // `allocsize(0)`, `byval(%struct.T)`, `nofpclass(nan)`, and future ones.
    atomic(
      (attrIdent, character.char('('), parenBalanced, character.char(')'))
        .zipped((n, _, body, _) => s"$n($body)") <* skipWhitespace
    ),
    // `align N` — the one bare-plus-int form without parens.
    atomic((token("align "), token(integer)).zipped(_ + _.toString)),
    // Quoted attribute: `"target-cpu"` or `"target-cpu"="x86-64"` etc.
    atomic(
      (
        '"' *> character.stringOfMany(_ != '"') <* '"',
        option(
          character.char('=') *> '"' *> character.stringOfMany(_ != '"') <* '"'
        )
      ).zipped((k, v) =>
        s""""$k"${v.map(vv => s"=\"$vv\"").getOrElse("")}"""
      ) <* skipWhitespace
    ),
    // Fallback: known bare-identifier attribute.
    ParameterAttrSimple.values.map(_.toString()).map(token).reduce(_ | _)
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

  lazy val llvmRef =
    val first = (c: Char) =>
      c == '-' || (c >= 'a' && c <= 'z') ||
        (c >= 'A' && c <= 'Z') ||
        c == '$' ||
        c == '_' ||
        c == '.'

    "%" *>
      character.stringOfSome(character.satisfy(c => first(c) || c.isDigit))
  end llvmRef

  lazy val (llvmIdentifierBase, llvmIdentifierNoQuotes) =
    val first = (c: Char) =>
      c == '-' || (c >= 'a' && c <= 'z') ||
        (c >= 'A' && c <= 'Z') ||
        c == '$' ||
        c == '_' ||
        c == '.'

    val noQuotes =
      character.stringOfSome(character.satisfy(c => first(c) || c.isDigit))

    (
      choice(
        '"' *> character.stringOfSome(_ != '"') <* '"',
        noQuotes
      ),
      noQuotes
    )
  end val

  lazy val llvmIdentifier =
    "@" *> llvmIdentifierBase
  end llvmIdentifier

  object instructions:
    object call:
      private lazy val tail = choice("tail", "musttail", "notail")
      private lazy val params =
        "(" *>
          parsley.combinator.sepBy(
            functions.call.param,
            token(",")
          ) <* ")"

      // A call's return type is richer than a top-level function's — it can
      // be a struct `{i64, i1}` (from intrinsics like `llvm.smul.with.overflow`)
      // or a function-pointer signature `i32 (ptr, ...)` (for varargs calls).
      // We consume it as a sequence of simple identifiers / brace groups /
      // paren groups, whitespace-separated, terminated when the next thing is
      // a callee sigil (`@`, `%`) or the `null` literal.
      private lazy val callReturnType: Parsley[String] =
        // The return type is EITHER:
        //   - a simple identifier / keyword: `ptr`, `void`, `i32`, `double`,
        //     `%struct.T`... except we reject `null` (callee) and sigils.
        //   - a struct literal `{i64, i1}` (anonymous struct return).
        // Optionally followed by a single parenthesised argument-type list
        // `(ptr, ...)` — the "function pointer" return-type form used for
        // varargs calls like `call i32 (ptr, ...) @printf(...)`.
        val simple = atomic(
          (
            character.satisfy(c =>
              c != ' ' && c != '(' && c != ')' && c != '{' && c != '}' &&
                c != ',' && c != '@' && c != '!' && c != '"'
            ),
            character.stringOfMany(character.satisfy(c =>
              c != ' ' && c != '(' && c != ')' && c != '{' && c != '}' && c != ','
            ))
          ).zipped((h, t) => s"$h$t")
        ).filter(_ != "null")
        val braceGroup: Parsley[String] =
          ('{' *> parenBalancedBraces <* '}').map(s => s"{$s}")
        val baseType = simple <|> braceGroup
        // Optional function-pointer suffix.
        val fnSuffix =
          atomic(character.spaces *> character.char('(')) *>
            parenBalanced <* character.char(')')
        (baseType, option(fnSuffix.map(s => s"($s)")))
          .zipped((b, sfx) => sfx.fold(b)(s => s"$b $s"))

      // Callee can be a direct `@ident`, an indirect `%local`, or the literal
      // `null` (calling a null pointer — appears in unreachable branches).
      // Indirect callees are stored with a leading `%` so downstream code
      // (index, cross-file resolution) can tell them apart from globals.
      private val callee: Parsley[String] =
        llvmIdentifier <|>
          ("%" *> character.stringOfSome(character.satisfy(c =>
            (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
              (c >= '0' && c <= '9') ||
              c == '_' || c == '.' || c == '-' || c == '$'
          ))).map(s => s"%$s") <|>
          atomic(string("null"))

      lazy val instr =
        (
          token("call") <* ws,
          option(token(tail)) <* ws,
          many(parameterAttr) <* ws,
          callReturnType.spanned <* ws,
          callee.spanned <* ws,
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

  // Lenient bare-identifier function attribute, used ONLY in the trailing
  // post-args block. Kept out of `parameterAttr` (which runs before the return
  // type) so it can't accidentally consume `ptr`, `void`, `i32`, `%struct.T`,
  // etc. In the trailing position the only thing that can legitimately follow
  // is `!dbg …` or `{`, both structurally distinct from an identifier.
  private val bareFnAttr: Parsley[String] =
    token(
      (
        character.satisfy(c =>
          (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
        ),
        character.stringOfMany(character.satisfy(c =>
          (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') ||
            (c >= '0' && c <= '9') || c == '_' || c == '.' || c == '-'
        ))
      ).zipped((h, t) => s"$h$t")
    )

  lazy val functionType =
    (
      many(parameterAttr) *> functionReturnType,
      functionName,
      token("(") *> functions.arguments <* token(")"),
      // Post-args stuff: fn attrs like `#0`, per-arg return attrs, `personality
      // ptr @foo`, `gc`, etc. Consume greedily.
      many(
        choice(
          atomic(someRandomBullshit),
          atomic(
            (token("personality"), token("ptr"), token(llvmIdentifier))
              .zipped((_, _, id) => s"personality ptr @$id")
          ),
          atomic(parameterAttr),
          bareFnAttr
        )
      ),
      atomic(debugAttachment)
    )
      .zipped:
        case (retType, name, args, _, dbg) =>
          Function(name, args.toVector, retType, dbg.toVector)

  lazy val declareFunctionType =
    (
      many(parameterAttr) *> functionReturnType,
      functionName,
      token("(") *> functions.declareArguments <* token(")"),
      many(choice(atomic(parameterAttr), bareFnAttr)),
      option(someRandomBullshit),
      atomic(debugAttachment)
    )
      .zipped:
        case (retType, name, args, _, _, dbg) =>
          Function(name, args.toVector, retType, dbg.toVector)

  lazy val functionDefinition =
    token("define") *> (
      functionAttrs,
      functionType,
      functionBody.map(_.toVector)
    ).zipped(Statement.FunctionDefinition.apply)
      .named("functionDefinition")

  lazy val functionDeclaration =
    atomic(
      token("declare") *> (
        functionAttrs,
        declareFunctionType
      ).zipped(Statement.Declare.apply)
    ).named("functionDeclaration")

  val lineComment = ws *> ";" *> character
    .stringOfSome(_ != '\n')
    .spanned
    .map(Statement.LineComment.apply)
    .named("lineComment")

  lazy val statement: Parsley[Statement] =
    choice(
      debugAssignment,
      functionDefinition,
      functionDeclaration,
      lineComment,
      parsley.combinator
        .manyTill(character.item, character.endOfLine <|> Parsley.eof.as('\n'))
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

  inline def char(c: Char) = character.char(c) <~ ws

  val LP: Parsley[Char]        = char('(')
  val RP: Parsley[Char]        = char(')')
  val LB: Parsley[Char]        = char('{')
  val RB: Parsley[Char]        = char('}')
  val ASS: Parsley[Char]       = char('=')
  val SEMICOLON: Parsley[Char] = char(';')
  val COLON: Parsley[Char]     = char(':')
  val EXCL: Parsley[Char]      = char('!')
  val COMMA: Parsley[Char]     = char(',')

  lazy val integer: Parsley[Int] =
    // LLVM permits arbitrary-precision integers (e.g. i64 max, or values that
    // don't fit in Long at all — the parser then just clamps to Int.MaxValue).
    // The model stores `Int`, so this is lossy for values above 2^31-1, but at
    // least it doesn't crash on realistic IR.
    character.stringOfSome(character.digit).map { s =>
      s.toLongOption match
        case Some(l) => l.toInt
        case None    => Int.MaxValue
    }

  lazy val alphanumeric: Parsley[String] =
    (
      character.letter,
      character.stringOfMany(character.letterOrDigit <|> '_')
    ).zipped(_ +: _)

  def parse(s: String): Either[ParsingError, Program] =
    parseWithPosition(program, s)

  def parse[A](s: String, parser: Parsley[A]): Either[ParsingError, A] =
    parseWithPosition(parser, s)

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
