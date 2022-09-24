import SpannedTree._

import cats.effect.*
import cats.effect.std.*
import jsonrpclib.fs2.*
import langoustine.lsp.*
import langoustine.lsp.all.*
import langoustine.lsp.app.*

case class Index(
    definitions: Map[Atom.Ref, Span],
    detectReferences: IntervalTree[Atom.Ref],
    text: TextIndex
)
object Index:
    def create(text: String, p: Program): Index =
      val defns = p.asses.collect {
        case WithSpan(span, Statement.Assignment(spannedId, spannedExpr)) =>
          spannedId.value -> span
      }

      def extractReferences(
          expression: WithSpan[Expression[WithSpan]]
      ): Vector[WithSpan[SpannedTree.Atom.Ref]] =
        def go(e: WithSpan[Expression[WithSpan]]): Vector[WithSpan[Atom.Ref]] =
          import SpannedTree.*

          e.value match
            case NamedData(struct, fields) =>
              fields.collect { case Field.KeyValue(name, value) =>
                go(value)
              }.flatten
            case Bag(exprs)     => exprs.flatMap(go)
            case Distinct(expr) => go(expr)
            case rf: Atom.Ref   => Vector(e.copy(value = rf))
            case _              => Vector.empty

        go(expression)

      val occurrences = p.asses
        .flatMap { case WithSpan(_, value) =>
          value match
            case Statement.Assignment(id, value) => extractReferences(value)
        }
        .map { ws => ws.span -> ws.value }

      Index(
        defns.toMap,
        IntervalTree.construct(occurrences.toMap),
        TextIndex.construct(text)
      )
