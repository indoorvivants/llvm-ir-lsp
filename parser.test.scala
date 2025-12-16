import weaver.*
import SpannedTree.*

import parsers.{parse, parseExpr}

object ParsingSpec extends FunSuite:

  test("parse const expression") {
    val result = parseExpr("DWARF")
    result match
      case Right(Atom.Const(value)) => expect(value == "DWARF")
      case other => failure(s"Expected Atom.Const, got $other")
  }

  test("parse string expression") {
    val result = parseExpr("\"DWARF\"")
    result match
      case Right(Atom.Str(value)) => expect(value == "DWARF")
      case other => failure(s"Expected Atom.Str, got $other")
  }

  test("parse string expression with exclamation") {
    val result = parseExpr("!\"hello world\"")
    result match
      case Right(Atom.Str(value)) => expect(value == "hello world")
      case other => failure(s"Expected Atom.Str, got $other")
  }

  test("parse integer without type") {
    val result = parseExpr("25")
    result match
      case Right(num: Atom.Num) =>
        expect.all(
          num.value == 25,
          num.tpe == "i32"
        )
      case other => failure(s"Expected Atom.Num, got $other")
  }

  test("parse integer with i8 type") {
    val result = parseExpr("i8 5")
    result match
      case Right(num: Atom.Num) =>
        expect.all(
          num.value == 5,
          num.tpe == "i8"
        )
      case other => failure(s"Expected Atom.Num, got $other")
  }

  test("parse integer with i32 type") {
    val result = parseExpr("i32 42")
    result match
      case Right(num: Atom.Num) =>
        expect.all(
          num.value == 42,
          num.tpe == "i32"
        )
      case other => failure(s"Expected Atom.Num, got $other")
  }

  test("parse integer with u64 type") {
    val result = parseExpr("u64 123")
    result match
      case Right(num: Atom.Num) =>
        expect.all(
          num.value == 123,
          num.tpe == "u64"
        )
      case other => failure(s"Expected Atom.Num, got $other")
  }

  test("parse reference") {
    val result = parseExpr("!25")
    result match
      case Right(ref: Atom.Ref) =>
        expect(ref.id.value == 25)
      case other => failure(s"Expected Atom.Ref, got $other")
  }

  test("parse bag expression") {
    val result = parseExpr("!{ i32 5, \"Dwarf Version\", 3}")
    result match
      case Right(Bag(exprs)) =>
        expect(exprs.map(_.value) == Vector(
          Atom.Num(5, "i32"),
          Atom.Str("Dwarf Version"),
          Atom.Num(3, "i32")
        ))
      case other => failure(s"Expected Bag, got $other")
  }

  test("parse empty bag") {
    val result = parseExpr("!{}")
    result match
      case Right(bag: Bag) =>
        expect(bag.exprs.isEmpty)
      case other => failure(s"Expected Bag, got $other")
  }

  test("parse bag with references") {
    val result = parseExpr("!{!1, !2, !3}")
    result match
      case Right(Bag(exprs)) =>
        expect(exprs.map(_.value) == Vector(
          Atom.Ref(Id(1)),
          Atom.Ref(Id(2)),
          Atom.Ref(Id(3))
        ))
      case other => failure(s"Expected Bag, got $other")
  }

  test("parse named data expression") {
    val result = parseExpr("!DiLocalVariable(a: !{25})")
    result match
      case Right(NamedData(struct, Vector(Field.KeyValue(name, valueSpan)))) =>
        expect.all(
          struct == Struct("DiLocalVariable"),
          name == FieldName("a")
        )
        valueSpan.value match
          case Bag(exprs) =>
            expect(exprs.map(_.value) == Vector(Atom.Num(25, "i32")))
          case other => failure(s"Expected Bag in field, got $other")
      case other => failure(s"Expected NamedData with one field, got $other")
  }

  test("parse named data with multiple fields") {
    val result = parseExpr("!DIFile(filename: \"test.scala\", directory: \"/home\")")
    result match
      case Right(NamedData(struct, Vector(
        Field.KeyValue(name1, value1Span),
        Field.KeyValue(name2, value2Span)
      ))) =>
        expect.all(
          struct == Struct("DIFile"),
          name1 == FieldName("filename"),
          name2 == FieldName("directory"),
          value1Span.value == Atom.Str("test.scala"),
          value2Span.value == Atom.Str("/home")
        )
      case other => failure(s"Expected NamedData with two fields, got $other")
  }

  test("parse named data with empty fields") {
    val result = parseExpr("!DILocation()")
    result match
      case Right(named: NamedData) =>
        expect.all(
          named.struct == Struct("DILocation"),
          named.fields.isEmpty
        )
      case other => failure(s"Expected NamedData, got $other")
  }

  test("parse distinct expression") {
    val result = parseExpr("distinct !DiLocalVariable(a: 5)")
    result match
      case Right(Distinct(exprSpan)) =>
        exprSpan.value match
          case NamedData(struct, fields) =>
            expect.all(
              struct == Struct("DiLocalVariable"),
              fields.size == 1
            )
          case other => failure(s"Expected NamedData inside Distinct, got $other")
      case other => failure(s"Expected Distinct, got $other")
  }

  test("parse full assignment with const") {
    val result = parse("!25 = DWARF")
    result match
      case Right(Program(Vector(stmtSpan))) =>
        stmtSpan.value match
          case Statement.Assignment(idSpan, exprSpan) =>
            expect.all(
              idSpan.value == Atom.Ref(Id(25)),
              exprSpan.value == Atom.Const("DWARF")
            )
      case Left(err) => failure(s"Parse failed: $err")
      case other => failure(s"Expected single assignment, got $other")
  }

  test("parse full assignment with string") {
    val result = parse("!25 = \"DWARF\"")
    result match
      case Right(Program(Vector(stmtSpan))) =>
        stmtSpan.value match
          case Statement.Assignment(idSpan, exprSpan) =>
            expect.all(
              idSpan.value == Atom.Ref(Id(25)),
              exprSpan.value == Atom.Str("DWARF")
            )
      case Left(err) => failure(s"Parse failed: $err")
      case other => failure(s"Expected single assignment, got $other")
  }

  test("parse full assignment with reference") {
    val result = parse("!25 = !25")
    result match
      case Right(Program(Vector(stmtSpan))) =>
        stmtSpan.value match
          case Statement.Assignment(idSpan, exprSpan) =>
            expect.all(
              idSpan.value == Atom.Ref(Id(25)),
              exprSpan.value == Atom.Ref(Id(25))
            )
      case Left(err) => failure(s"Parse failed: $err")
      case other => failure(s"Expected single assignment, got $other")
  }

  test("parse full assignment with typed integer") {
    val result = parse("!25 = i8 5")
    result match
      case Right(Program(Vector(stmtSpan))) =>
        stmtSpan.value match
          case Statement.Assignment(idSpan, exprSpan) =>
            expect.all(
              idSpan.value == Atom.Ref(Id(25)),
              exprSpan.value == Atom.Num(5, "i8")
            )
      case Left(err) => failure(s"Parse failed: $err")
      case other => failure(s"Expected single assignment, got $other")
  }

  test("parse full assignment with bag") {
    val result = parse("!25 = !{ i32 5, \"Dwarf Version\", 3}")
    result match
      case Right(Program(Vector(stmtSpan))) =>
        stmtSpan.value match
          case Statement.Assignment(idSpan, exprSpan) =>
            expect(idSpan.value == Atom.Ref(Id(25)))
            exprSpan.value match
              case Bag(exprs) =>
                expect(exprs.map(_.value) == Vector(
                  Atom.Num(5, "i32"),
                  Atom.Str("Dwarf Version"),
                  Atom.Num(3, "i32")
                ))
              case other => failure(s"Expected Bag, got $other")
      case Left(err) => failure(s"Parse failed: $err")
      case other => failure(s"Expected single assignment, got $other")
  }

  test("parse multiple assignments") {
    val input = """!0 = DWARF
                  |!1 = i32 42
                  |!2 = !0""".stripMargin
    val result = parse(input)
    result match
      case Right(Program(stmts)) =>
        val assignments = stmts.map(_.value).collect {
          case Statement.Assignment(idSpan, valueSpan) =>
            (idSpan.value, valueSpan.value)
        }
        expect(assignments == Vector(
          (Atom.Ref(Id(0)), Atom.Const("DWARF")),
          (Atom.Ref(Id(1)), Atom.Num(42, "i32")),
          (Atom.Ref(Id(2)), Atom.Ref(Id(0)))
        ))
      case Left(err) => failure(s"Parse failed: $err")
  }

  test("ignores things that are not good") {
    val emptyProg = Right(Program(Vector()))

    val examples = List(
      "define i32 @\"_SM11scala.None$D12productArityiEO\"(i8* %_1) personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*) !dbg !69 {",
      "_20000.0:",
      "  ret i32 0",
      "}",
      "",
      "define nonnull dereferenceable(32) i8* @\"_SM11scala.None$D13productPrefixL16java.lang.StringEO\"(i8* %_1) personality i8* bitcast (i32 (...)* @__gxx_personality_v0 to i8*) !dbg !72 {",
      "_20000.0:",
      "  ret i8* bitcast ({ i8*, i8*, i32, i32, i32 }* @\"_SM7__constG3-198\" to i8*)",
      "}"
    )

    forEach(examples)(ex => expect(parse(ex) == emptyProg))
  }

  test("mixed valid and invalid lines") {
    val input = """define i32 @main() {
                  |!0 = DWARF
                  |  ret i32 0
                  |}
                  |!1 = i32 5""".stripMargin
    val result = parse(input)
    result match
      case Right(Program(stmts)) =>
        val ids = stmts.map(_.value).collect {
          case Statement.Assignment(idSpan, _) => idSpan.value
        }
        expect(ids == Vector(Atom.Ref(Id(0)), Atom.Ref(Id(1))))
      case Left(err) => failure(s"Parse failed: $err")
  }

end ParsingSpec
