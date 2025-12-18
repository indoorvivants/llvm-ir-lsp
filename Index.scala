case class Index(
    tree: Metadata[WithSpan],
    definitions: Map[tree.Atom.Ref, Span],
    detectReferences: IntervalTree[tree.Atom.Ref],
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
      .flatMap { case Statement.MetadataAssignment(id, value) =>
        extractReferences(value)
      }
      .map { ws => ws.span -> ws.value }

    Index(
      tree,
      defns.toMap,
      IntervalTree.construct(occurrences.toMap),
      TextIndex.construct(text)
    )
  end create
end Index
