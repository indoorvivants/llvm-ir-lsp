opaque type Id = Int
object Id:
  inline def apply(i: Int): Id = i

opaque type FieldName = String
object FieldName:
  inline def apply(i: String): FieldName = i

opaque type Struct = String
object Struct:
  inline def apply(i: String): Struct = i

trait Module[F[_]]:
  sealed trait Expression[X[_]]

  enum Atom extends Expression[F]:
    case Str(value: String)
    case Ref(id: Id)
    case Num(value: Int, tpe: String)
    case Const(value: String)

  case class AtomExpression(atom: F[Atom]) extends Expression[F]

  case class Bag(exprs: Vector[F[Expression[F]]]) extends Expression[F]

  case class Distinct(expr: F[Expression[F]]) extends Expression[F]

  enum Field:
    case KeyValue(name: FieldName, value: F[Expression[F]])
    case Value(value: Expression[F])

  case class NamedData(struct: Struct, fields: Vector[Field])
      extends Expression[F]

  enum Statement:
    case Assignment(id: F[Atom.Ref], value: F[Expression[F]])

  case class Program(asses: Vector[F[Statement]])
