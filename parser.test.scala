//> using lib "com.disneystreaming::weaver-cats::0.8.0"

import weaver.*
import SpannedTree._

import parsers.parse

object ParsingSpec extends FunSuite:
  test("all is good") {
    expect.all(
      parse("!25 = DWARF").isRight,
      parse("!25 = \"DWARF\"").isRight,
      parse("!25 = 25").isRight,
      parse("!25 = !25").isRight,
      parse("!25 = i8 5").isRight,
      parse("!25 = !DiLocalVariable(a: !{25})").isRight,
      parse("!25 = distinct !DiLocalVariable(a: 5)").isRight,
      parse("!25 = !{ i32 5, \"Dwarf Version\", 3}").isRight
    )
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

    // expect.all(parse("declare i32 @llvm.eh.typeid.for(i8*)") == emptyProg)
  }
