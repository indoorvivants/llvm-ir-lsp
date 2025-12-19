package llvm_lsp

import parsley.debug.RemoteView
import weaver.*

import PureParsers.*
import PureTree.*

object ParsingSpec extends FunSuite:

  private def parseExpr(s: String) = parse(s, Metadata.expr)

  test("parse const expression") {
    expect.same(parseExpr("DWARF"), Right(Atom.Const("DWARF")))
  }

  test("parse string expression") {
    expect.same(
      parseExpr("\"DWARF\""),
      Right(Atom.Str("DWARF"))
    )
  }

  test("parse string expression with exclamation") {
    expect.same(
      parseExpr("!\"hello world\""),
      Right(Atom.Str("hello world"))
    )
  }

  test("parse integer without type") {
    expect.same(
      parseExpr("25"),
      Right(Atom.Num(25, "i32"))
    )
  }

  test("parse integer with i8 type") {
    expect.same(
      parseExpr("i8 5"),
      Right(Atom.Num(5, "i8"))
    )
  }

  test("parse integer with i32 type") {
    expect.same(
      parseExpr("i32 42"),
      Right(Atom.Num(42, "i32"))
    )
  }

  test("parse integer with u64 type") {
    expect.same(
      parseExpr("u64 123"),
      Right(Atom.Num(123, "u64"))
    )
  }

  test("parse reference") {
    expect.same(
      parseExpr("!25"),
      Right(Atom.Ref(Id(25)))
    )
  }

  test("parse bag expression") {
    expect.same(
      parseExpr("!{ i32 5, \"Dwarf Version\", 3}"),
      Right(
        Bag(
          Vector(
            Atom.Num(5, "i32"),
            Atom.Str("Dwarf Version"),
            Atom.Num(3, "i32")
          )
        )
      )
    )
  }

  test("parse empty bag") {
    expect.same(
      parseExpr("!{}"),
      Right(Bag(Vector.empty))
    )
  }

  test("parse bag with references") {
    expect.same(
      parseExpr("!{!1, !2, !3}"),
      Right(
        Bag(
          Vector(
            Atom.Ref(Id(1)),
            Atom.Ref(Id(2)),
            Atom.Ref(Id(3))
          )
        )
      )
    )
  }

  test("parse named data expression") {
    expect.same(
      parseExpr("!DiLocalVariable(a: !{25})"),
      Right(
        NamedData(
          Struct("DiLocalVariable"),
          Vector(
            Field.KeyValue(FieldName("a"), Bag(Vector(Atom.Num(25, "i32"))))
          )
        )
      )
    )
  }

  test("parse named data with multiple fields") {
    expect.same(
      parseExpr(
        "!DIFile(filename: \"test.scala\", directory: \"/home\")"
      ),
      Right(
        NamedData(
          Struct("DIFile"),
          Vector(
            Field.KeyValue(FieldName("filename"), Atom.Str("test.scala")),
            Field.KeyValue(FieldName("directory"), Atom.Str("/home"))
          )
        )
      )
    )
  }

  test("parse named data with empty fields") {
    expect.same(
      parseExpr("!DILocation()"),
      Right(
        NamedData(Struct("DILocation"), Vector.empty)
      )
    )
  }

  test("parse distinct expression") {
    expect.same(
      parseExpr("distinct !DiLocalVariable(a: 5)"),
      Right(
        Distinct(
          NamedData(
            Struct("DiLocalVariable"),
            Vector(Field.KeyValue(FieldName("a"), Atom.Num(5, "i32")))
          )
        )
      )
    )
  }

  test("parse full MetadataAssignment with const") {
    val result = parse("!25 = DWARF")

    expect.same(
      result,
      Right(
        Program(
          Vector(
            Statement.MetadataAssignment(Atom.Ref(Id(25)), Atom.Const("DWARF"))
          )
        )
      )
    )

  }

  test("parse full MetadataAssignment with string") {
    expect.same(
      parse("!25 = \"DWARF\""),
      Right(
        Program(
          Vector(
            Statement.MetadataAssignment(Atom.Ref(Id(25)), Atom.Str("DWARF"))
          )
        )
      )
    )
  }

  test("parse full MetadataAssignment with reference") {
    expect.same(
      parse("!25 = !{!24, !24}"),
      Right(
        Program(
          Vector(
            Statement.MetadataAssignment(
              Atom.Ref(Id(25)),
              Bag(Vector(Atom.Ref(Id(24)), Atom.Ref(Id(24))))
            )
          )
        )
      )
    )
  }

  test("parse full MetadataAssignment with typed integer") {
    expect.same(
      parse("!25 = i8 5"),
      Right(
        Program(
          Vector(
            Statement.MetadataAssignment(Atom.Ref(Id(25)), Atom.Num(5, "i8"))
          )
        )
      )
    )
  }

  test("parse full MetadataAssignment with bag") {
    expect(
      parse("!25 = !{ i32 5, \"Dwarf Version\", 3}") == Right(
        Program(
          Vector(
            Statement.MetadataAssignment(
              Atom.Ref(Id(25)),
              Bag(
                Vector(
                  Atom.Num(5, "i32"),
                  Atom.Str("Dwarf Version"),
                  Atom.Num(3, "i32")
                )
              )
            )
          )
        )
      )
    )
  }

  test("parse multiple MetadataAssignments") {
    val input = """
    |!0 = DWARF
    |!1 = i32 42
    |!2 = !0""".stripMargin.trim

    expect.same(
      parse(input),
      Right(
        Program(
          Vector(
            Statement.MetadataAssignment(Atom.Ref(Id(0)), Atom.Const("DWARF")),
            Statement.MetadataAssignment(Atom.Ref(Id(1)), Atom.Num(42, "i32")),
            Statement.MetadataAssignment(Atom.Ref(Id(2)), Atom.Ref(Id(0)))
          )
        )
      )
    )
  }

  test("parses LLVM identifiers correctly") {
    val samples =
      List(
        """@"_SM32scala.scalanative.runtime.Boxes$D10boxToUSizewL32scala.scalanative.unsigned.USizeEO"""" -> "_SM32scala.scalanative.runtime.Boxes$D10boxToUSizewL32scala.scalanative.unsigned.USizeEO",
        "@llvm.dbg.declare" -> "llvm.dbg.declare"
      )
    expect.same(
      samples.map(_._1).map(parse(_, llvmIdentifier)),
      samples.map(_._2).map(Right(_))
    )
  }

  test("parses function definition") {
    val input =
      """
; ModuleID = '/app/example.cpp'
source_filename = "/app/example.cpp"
target datalayout = "e-m:e-p270:32:32-p271:32:32-p272:64:64-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-unknown-linux-gnu"

; Declare the string constant as a global constant.
@.str = private unnamed_addr constant [13 x i8] c"hello world\0A\00"

; External declaration of the puts function
declare i32 @puts(ptr captures(none)) nounwind


define dso_local noundef float @_Z1tf(float noundef %0) #0 !dbg !10 {
  %2 = alloca float, align 4
  store float %0, ptr %2, align 4
  %_2000009 = call dereferenceable_or_null(16) ptr @"_SM32scala.scalanative.runtime.Boxes$D10boxToUSizewL32scala.scalanative.unsigned.USizeEO"(ptr null, i64 256), !dbg !797
  %3 = load float, ptr %2, align 4, !dbg !18
  %4 = fdiv float %3, 2.000000e+00, !dbg !19
  ret float %4, !dbg !20
}


!1 = !{!2, i32 5}
"""
    import parsley.debug.combinator.*
    expect.same(
      parse(input, program.attach(RemoteView.dill)),
      Right(
        value = Program(
          asses = Vector(
            Statement.LineComment(
              content = " ModuleID = '/app/example.cpp'"
            ),
            Statement.LineComment(
              content = " Declare the string constant as a global constant."
            ),
            Statement.LineComment(
              content = " External declaration of the puts function"
            ),
            Statement.FunctionDefinition(
              (None, Some("dso_local"), None, Some("noundef")),
              Function(
                "_Z1tf",
                Vector(FunctionArgument(ArgumentType("float"), "0")),
                "float",
                Vector(Atom.Ref(Id(10)))
              ),
              Vector(
                BodyOperation
                  .Assignment(
                    LocalID("2"),
                    Instruction.Unknown("alloca float, align 4")
                  ),
                BodyOperation.Instr(
                  Instruction.Unknown("store float %0, ptr %2, align 4")
                ),
                BodyOperation.Assignment(
                  LocalID("_2000009"),
                  Instruction.Call(
                    tail = None,
                    tpe = "ptr",
                    funcName =
                      "_SM32scala.scalanative.runtime.Boxes$D10boxToUSizewL32scala.scalanative.unsigned.USizeEO",
                    params = Vector(
                      FunctionCallParam.Ptr("null"),
                      FunctionCallParam.Num(256, "i64")
                    )
                  )
                ),
                BodyOperation.Assignment(
                  LocalID("3"),
                  Instruction.Unknown("load float, ptr %2, align 4, !dbg !18")
                ),
                BodyOperation.Assignment(
                  LocalID("4"),
                  Instruction.Unknown("fdiv float %3, 2.000000e+00, !dbg !19")
                ),
                BodyOperation.Instr(
                  Instruction.Unknown("ret float %4, !dbg !20")
                )
              )
            ),
            Statement.MetadataAssignment(
              id = Atom.Ref(Id(1)),
              value = Bag(
                exprs = Vector(
                  Atom.Ref(Id(2)),
                  Atom.Num(
                    value = 5,
                    tpe = "i32"
                  )
                )
              )
            )
          )
        )
      )
    )
  }
end ParsingSpec
