opaque type Id = Int
object Id:
  inline def apply(i: Int): Id             = i
  extension (id: Id) inline def value: Int = id

opaque type FieldName = String
object FieldName:
  inline def apply(i: String): FieldName = i

opaque type Struct = String
object Struct:
  inline def apply(i: String): Struct = i

trait Metadata[F[_]]:
  sealed trait Expression[X[_]]

  enum Atom extends Expression[F]:
    case Str(value: String)
    case Ref(id: Id)
    case Num(value: Int, tpe: String)
    case Const(value: String)

  case class AtomExpression(atom: F[Atom])        extends Expression[F]
  case class Bag(exprs: Vector[F[Expression[F]]]) extends Expression[F]

  case class Distinct(expr: F[Expression[F]]) extends Expression[F]

  enum Field:
    case KeyValue(name: FieldName, value: F[Expression[F]])
    case Value(value: Expression[F])

  case class NamedData(struct: Struct, fields: Vector[Field])
      extends Expression[F]

  type FunctionBodyLine = (Int, String) | String

  enum Statement extends Expression[F]:
    case LineComment(content: F[String])
    case MetadataAssignment(id: F[Atom.Ref], value: F[Expression[F]])
    case FunctionDefinition(
        attrs: (Option[String], Option[String], Option[String], Option[String]),
        func: Function,
        body: Vector[FunctionBodyLine]
    )

  case class Program(asses: Vector[Statement])

  case class ArgumentType(name: F[String]) extends Expression[F]

  case class FunctionArgument(
      tpe: ArgumentType,
      name: F[String]
  ) extends Expression[F]

  case class Function(
      name: F[String],
      arguments: Vector[FunctionArgument],
      resultType: F[String],
      metadataAttachments: Vector[F[Atom.Ref]]
  ) extends Expression[F]

end Metadata

object SpannedTree extends Metadata[WithSpan]
object PureTree    extends Metadata[cats.Id]
