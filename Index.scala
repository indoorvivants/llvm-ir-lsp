package llvm_lsp

case class Index(
    tree: Metadata[WithSpan],
    definitions: Map[tree.Atom.Ref, Span],
    detectReferences: IntervalTree[tree.Atom.Ref],
    functionDefinitions: Map[String, Span],
    functionReferences: IntervalTree[String],
    functionNameSpans: Vector[Span],
    text: TextIndex
)
object Index:
  def create(
      tree: Metadata[WithSpan],
      text: String,
      p: tree.Program
  ): Index =
    import tree.*
    val defns = p.asses.collect {
      case Statement.MetadataAssignment(spannedId, spannedExpr) =>
        spannedId.value -> spannedExpr.span
    }

    def extractReferences(
        expression: WithSpan[Expression[WithSpan]]
    ): Vector[WithSpan[Atom.Ref]] =
      def go(e: WithSpan[Expression[WithSpan]]): Vector[WithSpan[Atom.Ref]] =
        e.value match
          case NamedData(struct, fields) =>
            fields.collect { case Field.KeyValue(name, value) =>
              go(value)
            }.flatten
          case Bag(exprs)     => exprs.flatMap(go)
          case Distinct(expr) => go(expr)
          case rf: Atom.Ref   => Vector(e.copy(value = rf))
          case _              => Vector.empty
        end match
      end go

      go(expression)
    end extractReferences

    val occurrences = p.asses
      .collect { case Statement.MetadataAssignment(id, value) =>
        extractReferences(value)
      }
      .flatten
      .map { ws => ws.span -> ws.value }

    // Function definitions: pull the spanned name out of every `define` and
    // `declare`. First occurrence wins on duplicates (LLVM allows only one
    // define per symbol; declares are re-declarable but the span is best-effort
    // for goto-def anyway).
    val functionDefns = collection.mutable.LinkedHashMap.empty[String, Span]
    p.asses.foreach {
      case Statement.FunctionDefinition(_, func, _) =>
        functionDefns.getOrElseUpdate(func.name.value, func.name.span)
      case Statement.Declare(_, func) =>
        functionDefns.getOrElseUpdate(func.name.value, func.name.span)
      case _ => ()
    }

    // Function references: every call site's spanned funcName. We walk both
    // direct instructions and assignment RHS, in both function bodies and
    // (defensively) at the top level. Only `Instruction.Call` carries a name;
    // `Instruction.Unknown` is opaque, so `invoke`, GEP-of-function, etc.
    // are not indexed yet.
    def collectCalls(instr: Instruction): Vector[WithSpan[String]] =
      instr match
        case Instruction.Call(_, _, funcName, _) => Vector(funcName)
        case _                                   => Vector.empty

    val functionRefsWithSpan = p.asses.flatMap {
      case Statement.FunctionDefinition(_, _, body) =>
        body.flatMap {
          case BodyOperation.Instr(i)          => collectCalls(i)
          case BodyOperation.Assignment(_, i)  => collectCalls(i)
          case _: BodyOperation.SetLabel       => Vector.empty
        }
      case _ => Vector.empty
    }
    val functionRefs = functionRefsWithSpan.map(ws => ws.span -> ws.value)

    // All function-name occurrences (defs + calls) — used to emit semantic
    // tokens of kind `function`. Sorted by offset so the LSP delta-encoder
    // can walk them in document order.
    val functionNameSpans =
      (functionDefns.values.toVector ++ functionRefsWithSpan.map(_.span))
        .sortBy(_.from.offset)

    Index(
      tree,
      defns.toMap,
      IntervalTree.construct(occurrences.toMap),
      functionDefns.toMap,
      IntervalTree.construct(functionRefs.toMap),
      functionNameSpans,
      TextIndex.construct(text)
    )
  end create
end Index
