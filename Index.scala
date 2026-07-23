package llvm_lsp

enum FunctionDefKind:
  case Define, Declare

case class FunctionDefEntry(
    nameSpan: Span,
    kind: FunctionDefKind,
    /** For `define`: line of the closing `}`. For `declare`: same as
      * `nameSpan.from.line` (single-line). Used to bound hover snippets.
      */
    endLine: Int
)

case class Index(
    tree: Metadata[WithSpan],
    definitions: Map[tree.Atom.Ref, Span],
    detectReferences: IntervalTree[tree.Atom.Ref],
    functionDefinitions: Map[String, FunctionDefEntry],
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
    // `declare`. When a symbol has both a `declare` and a `define`, prefer
    // the `define` — that's what hover / goto-def should land on.
    val srcLines      = text.linesIterator.toVector
    val functionDefns = collection.mutable.LinkedHashMap.empty[String, FunctionDefEntry]
    def upsert(entry: FunctionDefEntry, name: String): Unit =
      functionDefns.get(name) match
        case Some(existing) if existing.kind == FunctionDefKind.Define => ()
        case _                                                          =>
          functionDefns.update(name, entry)
    // Scan forward from `startLine` in `srcLines` to find the closing `}` of
    // a `define`. Falls back to `startLine` if no closer is found within a
    // reasonable window (keeps hover snippets bounded on malformed input).
    def findCloseBrace(startLine: Int, maxLookahead: Int = 500): Int =
      val end = math.min(srcLines.size, startLine + maxLookahead)
      var i   = startLine
      while i < end && !srcLines(i).trim.startsWith("}") do i += 1
      if i < end then i else startLine
    p.asses.foreach {
      case Statement.FunctionDefinition(_, func, _) =>
        val startLn = func.name.span.from.line
        upsert(
          FunctionDefEntry(
            func.name.span,
            FunctionDefKind.Define,
            findCloseBrace(startLn)
          ),
          func.name.value
        )
      case Statement.Declare(_, func) =>
        val startLn = func.name.span.from.line
        upsert(
          FunctionDefEntry(func.name.span, FunctionDefKind.Declare, startLn),
          func.name.value
        )
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
      (functionDefns.values.map(_.nameSpan).toVector ++
        functionRefsWithSpan.map(_.span))
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
