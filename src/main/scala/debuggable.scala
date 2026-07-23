package parsley

import parsley.*

import scala.annotation.{experimental, unused, MacroAnnotation}
import scala.quoted.*
import scala.collection.immutable.Queue

type Registration =
  Expr[(Map[Parsley[?], String], String, List[(Int, Int)])]

/** This annotation can be applied to an object or class to record their names
  * for the debugging/diagnostic combinators.
  *
  * @note
  *   Currently, macro-annotations in Scala 3 are experimental, which means the @experimental
  *   annotation will need to be used (or the global -experimental flag on 3.4+)
  *   to use this functionality.
  *
  * @since 5.0.0
  */
class debuggable2 extends MacroAnnotation:
  // this is required for Scala 3.5+
  def transform(using
      Quotes
  )(
      tree: quotes.reflect.Definition,
      @unused companion: Option[quotes.reflect.Definition]
  ): List[quotes.reflect.Definition] = transform(tree)
  def transform(using
      Quotes
  )(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
    import quotes.reflect.*
    tree match
      case cls @ ClassDef(clsName, constr, parents, selfOpt, body) =>
        // val fields  = cls.symbol.fieldMembers.view.map(_.termRef)
        // val parsers = fields.filter(isParsley).toList
        // // the idea is we inject a call to Collector.registerNames with a constructed
        // // map from these identifiers to their compile-time names
        // val listOfParsers = Expr.ofList {
        //   parsers.map(tr =>
        //     Expr.ofTuple(
        //       (Ident(tr).asExprOf[parsley.Parsley[?]], Expr(tr.name))
        //     )
        //   )
        // }

        // val filePath = Expr(cls.pos.sourceFile.path)
        // val positionInfo = Expr.ofList(
        //   for
        //     termRef <- parsers
        //     pos     <- termRef.termSymbol.pos
        //   yield Expr.ofTuple(Expr(pos.start), Expr(pos.end))
        // )
        // // val parserInfo = '{ Some(($filePath, $positionInfo)) }

        // // val registration = '{
        // //   parsley.debug.util.Collector
        // //     .registerNames($listOfParsers.toMap, $filePath, $positionInfo)
        // // }.asTerm

        // // add the registration as the last statement in the object
        // // TODO: in future, we want to modify all `def`s with a top level `opaque` combinator
        // // that will require a bit more modification of the overall `body`, unfortunately
        //
        val all = extractAllParsers(cls, Queue.empty).map { reg =>
          '{
            val r = $reg
            println(s"Registering $r")
            parsley.debug.util.Collector
              .registerNames($reg._1, Some(($reg._2, $reg._3)))
          }
        }

        List(
          ClassDef
            .copy(tree)(
              clsName,
              constr,
              parents,
              selfOpt,
              body :+ Expr.ofList(all).asTerm
            )
        )
      case _ =>
        report.error(
          "only classes/objects containing parsers can be annotated for debugging"
        )
        List(tree)
    end match
  end transform

  // this can see through type aliases
  private def isParsley(using Quotes)(tyRepr: quotes.reflect.TypeRepr) =
    tyRepr.asType match
      case '[Parsley[?]] => true
      case _             => false

  private def extractAllParsers(using
      Quotes
  )(
      tree: quotes.reflect.Definition,
      nesting: Queue[String]
  ): List[Registration] =
    import quotes.reflect.*
    tree match
      case cls @ ClassDef(clsName, constr, parents, selfOpt, body) =>
        report.info(s"Extracting parsers from ${clsName} -- $nesting")
        val fields  = cls.symbol.fieldMembers.view.map(_.termRef)
        val parsers = fields.filter(isParsley).toList
        // the idea is we inject a call to Collector.registerNames with a constructed
        // map from these identifiers to their compile-time names
        val listOfParsers = Expr.ofList {
          parsers.map(tr =>
            Expr.ofTuple(
              (
                Ident(tr).asExprOf[parsley.Parsley[?]],
                Expr((nesting :+ tr.name).mkString("."))
              )
            )
          )
        }

        val filePath = Expr(cls.pos.sourceFile.path)
        val positionInfo = Expr.ofList(
          for
            termRef <- parsers
            pos     <- termRef.termSymbol.pos
          yield Expr.ofTuple(Expr(pos.start), Expr(pos.end))
        )
        val thisClass = '{ ($listOfParsers.toMap, $filePath, $positionInfo) }

        // val nestedClasses = fields
        //   .collect { case cls: ClassDef =>
        //     extractAllParsers(cls, nesting.appended(clsName))
        //   }
        //   .flatten
        //   .toList

        thisClass +: Nil

      case _ => Nil
    end match
  end extractAllParsers

end debuggable2
