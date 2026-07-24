package llvm_lsp

import PureParsers.*, tree.*
import scala.jdk.CollectionConverters.*

class ParsingSpec extends munit.FunSuite:

  private def parseExpr(s: String) = parse(s, expr)

  test("parse const expression") {
    assertEquals(parseExpr("DWARF"), Right(Atom.Const("DWARF")))
  }

  test("parse string expression") {
    assertEquals(
      parseExpr("\"DWARF\""),
      Right(Atom.Str("DWARF"))
    )
  }

  test("parse string expression with exclamation") {
    assertEquals(
      parseExpr("!\"hello world\""),
      Right(Atom.Str("hello world"))
    )
  }

  test("parse integer without type") {
    assertEquals(
      parseExpr("25"),
      Right(Atom.Num(25, "i32"))
    )
  }

  test("parse integer with i8 type") {
    assertEquals(
      parseExpr("i8 5"),
      Right(Atom.Num(5, "i8"))
    )
  }

  test("parse integer with i32 type") {
    assertEquals(
      parseExpr("i32 42"),
      Right(Atom.Num(42, "i32"))
    )
  }

  test("parse integer with u64 type") {
    assertEquals(
      parseExpr("u64 123"),
      Right(Atom.Num(123, "u64"))
    )
  }

  test("parse reference") {
    assertEquals(
      parseExpr("!25"),
      Right(Atom.Ref(Id(25)))
    )
  }

  test("parse bag expression") {
    assertEquals(
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
    assertEquals(
      parseExpr("!{}"),
      Right(Bag(Vector.empty))
    )
  }

  test("parse bag with references") {
    assertEquals(
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
    assertEquals(
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
    assertEquals(
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
    assertEquals(
      parseExpr("!DILocation()"),
      Right(
        NamedData(Struct("DILocation"), Vector.empty)
      )
    )
  }

  test("parse distinct expression") {
    assertEquals(
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

    assertEquals(
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

  test("parses call instructions") {
    assertEquals(
      parse(
        "call noundef void @llvm.dbg.declare(metadata ptr %2, metadata !16, metadata !DIExpression()), !dbg !17",
        instructions.call.instr//.attach(RemoteView.dill)
      ),
      Right(
        value = Instruction.Call(
          tail = None,
          tpe = "void",
          funcName = "llvm.dbg.declare",
          params = Vector(
            FunctionCallParam.Metadata(
              arg = FunctionCallParam.Ptr(
                "2"
              )
            ),
            FunctionCallParam.Metadata(
              arg = Atom.Ref(Id(16))
            ),
            FunctionCallParam.Metadata(
              arg = NamedData(
                struct = Struct("DIExpression"),
                fields = Vector.empty
              )
            )
          )
        )
      )
    )

  }

  test("parse full MetadataAssignment with string") {
    assertEquals(
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
    assertEquals(
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
    assertEquals(
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
    assertEquals(
      parse(
        "!25 = !{ i32 5, \"Dwarf Version\", 3}",
        program//.attach(RemoteView.dill)
      ),
      Right(
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

    assertEquals(
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
    assertEquals(
      samples.map(_._1).map(parse(_, llvmIdentifier)),
      samples.map(_._2).map(Right(_))
    )
  }

  test("parses single function definition") {
    val input =
      """define dso_local noundef float @_Z1tf(float noundef %0) #0 !dbg !10 {
  %2 = alloca float, align 4
  store float %0, ptr %2, align 4
  %_2000009 = call dereferenceable_or_null(16) ptr @"_SM32scala.scalanative.runtime.Boxes$D10boxToUSizewL32scala.scalanative.unsigned.USizeEO"(ptr null, i64 256), !dbg !797
  %3 = load float, ptr %2, align 4, !dbg !18
  %4 = fdiv float %3, 2.000000e+00, !dbg !19
  ret float %4, !dbg !20
}

!1 = !{!2, i32 5}
"""
    assertEquals(
      parse(input, program),
      Right(
        Program(
          Vector(
            Statement.FunctionDefinition(
              (None, Some("dso_local"), None, Some("noundef")),
              Function(
                "_Z1tf",
                Vector(FunctionArgument(ArgumentType("float"), "0")),
                "float",
                Vector(Atom.Ref(Id(10)))
              ),
              Vector(
                BodyOperation.Assignment(
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
                      FunctionCallParam.Ptr(NULL),
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
              Atom.Ref(Id(1)),
              Bag(Vector(Atom.Ref(Id(2)), Atom.Num(5, "i32")))
            )
          )
        )
      )
    )
  }

  // Regression tests for bugs found while auditing the parser (task step 1).

  test("call with typed scalar reference (non-float)") {
    // Before fix: typedRef only accepted `float`, so `i32 %x`, `i64 %x`,
    // `double %x` would fail.
    assertEquals(
      parse(
        "call void @f(i32 %x, i64 %y, double %z)",
        instructions.call.instr
      ),
      Right(
        Instruction.Call(
          tail = None,
          tpe = "void",
          funcName = "f",
          params = Vector(
            FunctionCallParam.TypedRef("i32", "x"),
            FunctionCallParam.TypedRef("i64", "y"),
            FunctionCallParam.TypedRef("double", "z")
          )
        )
      )
    )
  }

  test("call with typed float literal") {
    // Before fix: `float 2.0e0` was rejected — `num` did not know floats.
    assertEquals(
      parse("call void @f(float 2.000000e+00)", instructions.call.instr),
      Right(
        Instruction.Call(
          tail = None,
          tpe = "void",
          funcName = "f",
          params = Vector(FunctionCallParam.Num(0, "float"))
        )
      )
    )
  }

  test("negative integer literal") {
    // Before fix: `-` was not accepted.
    assertEquals(parseExpr("i32 -5"), Right(Atom.Num(-5, "i32")))
  }

  test("float literal at expression position") {
    // Just make sure it parses; value is currently lossy (stored as 0).
    val Right(v) = parseExpr("double 1.5e0"): @unchecked
    assertEquals(v.asInstanceOf[Atom.Num].tpe, "double")
  }

  test("hex float literal in call") {
    val Right(Instruction.Call(_, _, _, params)) =
      parse("call void @f(double 0x7FF0000000000000)", instructions.call.instr): @unchecked
    assertEquals(params, Vector(FunctionCallParam.Num(0, "double")))
  }

  test("unknown top-level statement without trailing newline") {
    // Before fix: `manyTill(item, endOfLine)` needed a newline; a final
    // orphan line at EOF would fail the whole program parse.
    assertEquals(
      parse("target triple = \"x86_64-unknown-linux-gnu\""),
      Right(Program(Vector.empty))
    )
  }

  test("named metadata assignment (dotted string id)") {
    assertEquals(
      parse("!llvm.dbg.cu = !{!0}"),
      Right(
        Program(
          Vector(
            Statement.MetadataAssignment(
              Atom.Ref(Id("llvm.dbg.cu")),
              Bag(Vector(Atom.Ref(Id(0))))
            )
          )
        )
      )
    )
  }

  test("quoted metadata id on LHS of assignment") {
    // Before fix: `!"foo" = ...` was rejected because the LHS ref parser
    // did not accept quoted identifiers.
    assertEquals(
      parse("!\"foo bar\" = !{!0}"),
      Right(
        Program(
          Vector(
            Statement.MetadataAssignment(
              Atom.Ref(Id("foo bar")),
              Bag(Vector(Atom.Ref(Id(0))))
            )
          )
        )
      )
    )
  }

  test("parses resources/basic.ll end-to-end") {
    val src = scala.io.Source.fromFile("resources/basic.ll").mkString
    val Right(Program(stmts)) = parse(src): @unchecked
    val funcDefs = stmts.collect { case f: Statement.FunctionDefinition => f }
    val declares = stmts.collect { case d: Statement.Declare => d }
    assertEquals(funcDefs.size, 2)
    assertEquals(declares.size, 2)
  }

  test("parses resources/basic2.ll end-to-end") {
    val src = scala.io.Source.fromFile("resources/basic2.ll").mkString
    val Right(Program(stmts)) = parse(src): @unchecked
    val funcDefs = stmts.collect { case f: Statement.FunctionDefinition => f }
    val declares = stmts.collect { case d: Statement.Declare => d }
    assertEquals(funcDefs.size, 2)
    assertEquals(declares.size, 1)
  }

  test("parses declare with named ptr argument (captures attr)") {
    val Right(Program(Vector(d: Statement.Declare))) =
      parse("declare i32 @puts(ptr captures(none)) nounwind\n"): @unchecked
    assertEquals(d.func.name, "puts")
    assertEquals(d.func.resultType, "i32")
  }

  test("call with nonnull + dereferenceable(N) attrs on ptr args") {
    // Regression: reported by user against a real Scala Native fixture.
    val Right(v) = parse(
      "call void @f(ptr nonnull dereferenceable(32) %_31000001, ptr dereferenceable_or_null(48) %_10000001)",
      instructions.call.instr
    ): @unchecked
    assertEquals(v.asInstanceOf[Instruction.Call].funcName, "f")
    assertEquals(v.asInstanceOf[Instruction.Call].params.size, 2)
  }

  test("call params: i1 true / i1 false boolean literals") {
    val Right(v) = parse(
      "call void @f(i1 true, i1 false, i32 5)",
      instructions.call.instr
    ): @unchecked
    val params = v.asInstanceOf[Instruction.Call].params
    assertEquals(params.size, 3)
    assertEquals(params(0), FunctionCallParam.Num(1, "i1"))
    assertEquals(params(1), FunctionCallParam.Num(0, "i1"))
  }

  test("function definitions accept any bare-identifier attribute after args") {
    // Lenient trailing-block parser: no allowlist needed for `inlinehint`,
    // future-proofing against new LLVM function attributes.
    val src =
      """define ptr @foo(ptr %_1) inlinehint completely_made_up_attr xyz42 personality ptr @scalanative_personality {
_entry:
  ret ptr null
}
"""
    val Right(Program(Vector(f: Statement.FunctionDefinition))) =
      parse(src): @unchecked
    assertEquals(f.func.name, "foo")
  }

  test("call params: ptr can point to a global @-reference") {
    // Regression: user reported that `ptr @"_SM...G8instance"` in a call
    // param was rejected because `ptr` only accepted `%local` or `null`.
    val Right(v) = parse(
      """call void @f(ptr nonnull dereferenceable(16) @"_SM30java.nio.channels.FileChannel$G8instance", ptr @"_SM16java.lang.StringG4type", ptr @foo)""",
      instructions.call.instr
    ): @unchecked
    assertEquals(v.asInstanceOf[Instruction.Call].params.size, 3)
  }

  test("lenient parameterAttr: byval(%struct.T), quoted attrs, alwaysinline") {
    // The lenient rule accepts any `ident(<balanced>)` form and quoted attrs.
    val Right(_) = parse(
      "call void @f(ptr byval(%struct.T) %x, ptr byref(%foo) %y)",
      instructions.call.instr
    ): @unchecked
    val defineSrc =
      """define dereferenceable_or_null(40) ptr @"foo"(ptr %_1) alwaysinline "target-cpu"="x86-64" personality ptr @scalanative_personality {
_2000000.0:
  ret ptr null
}
"""
    val Right(Program(Vector(f: Statement.FunctionDefinition))) =
      parse(defineSrc): @unchecked
    assertEquals(f.func.name, "foo")
    assertEquals(f.func.resultType, "ptr")
  }

  test("call with align N and align(N) attrs on ptr args") {
    val Right(v) = parse(
      "call void @f(ptr align 8 %x, ptr align(16) %y)",
      instructions.call.instr
    ): @unchecked
    assertEquals(v.asInstanceOf[Instruction.Call].params.size, 2)
  }

  test("parses every .ll file in resources/ without errors") {
    // Corpus test — locks in that the full set of Scala Native fixtures
    // parse. If a real-world IR construct breaks this, add a smaller focused
    // fixture and fix the parser before shipping.
    val dir   = java.nio.file.Paths.get("resources")
    val files = java.nio.file.Files
      .list(dir)
      .iterator
      .asScala
      .toVector
      .filter(_.toString.endsWith(".ll"))
    val failures = files.flatMap { p =>
      val src = new String(java.nio.file.Files.readAllBytes(p))
      parse(src) match
        case Right(_)  => None
        case Left(err) => Some(p.getFileName.toString -> err.toString.take(400))
    }
    if failures.nonEmpty then
      fail(
        s"${failures.size}/${files.size} fixture(s) failed to parse:\n" +
          failures.take(3).map((n, e) => s"  $n: $e").mkString("\n")
      )
    else
      assertEquals(files.size, files.size) // ensure the loop actually ran
  }

  test("parses declare with type-only arguments") {
    val Right(Program(Vector(d: Statement.Declare))) =
      parse("declare void @llvm.dbg.declare(metadata, metadata, metadata) #1\n"): @unchecked
    assertEquals(d.func.name, "llvm.dbg.declare")
    assertEquals(d.func.arguments.size, 3)
  }
end ParsingSpec
