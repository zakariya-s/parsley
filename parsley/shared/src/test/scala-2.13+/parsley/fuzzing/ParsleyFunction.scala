package parsley.fuzzing

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

import parsley.fuzzing.PrettyPrint._

// An inspectable, shrinkable, and comparable function type suited for fuzzing
// ScalaCheck should really incorporate something like this, since QuickCheck does
sealed trait ParsleyFunction[-T, +R] extends (T => R) {
    import ParsleyFunction._

    def andThen[A: PrettyPrint](that: ParsleyFunction[R, A]): ParsleyFunction[T, A] = (this, that) match {
        case (f1: UnwrappedFunction[T, R], f2: UnwrappedFunction[R, A]) => f1 andThenUnwrapped f2
        case (f1, f2) => CombinedFunction(f1, f2)
    }

    def prettyString(ctx: List[String], depth: Int = 0): (String, List[String])

    override def toString = this.prettyToString
}

object ParsleyFunction {
    val Indent: String = "    "

    // An arbitrary ParsleyFunction will always be an UnwrappedFunction
    // The other ParsleyFunctions are only created internally when performing optimizations
    implicit def arbFunc[T: Arbitrary: PrettyPrint, R: Arbitrary: PrettyPrint]: Arbitrary[ParsleyFunction[T, R]] = Arbitrary(UnwrappedFunction.genUnwrappedFunc[T, R])

    implicit def prettyPrintParsleyFunction[T, R]: PrettyPrint[ParsleyFunction[T, R]] = new PrettyPrint[ParsleyFunction[T, R]] {
        override def prettyPrint(obj: ParsleyFunction[T, R], ctx: List[String], depth: Int): (String, List[String]) = obj.prettyString(ctx, depth)
    }

    // Represents `f1 andThen f2` but in a form which can be printed and debugged
    case class CombinedFunction[-T, S, +R](f1: ParsleyFunction[T, S], f2: ParsleyFunction[S, R]) extends ParsleyFunction[T, R] {
        override def apply(x: T): R = f2(f1(x))

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (out1, ctx1) = f1.prettyString(ctx, depth + 2)
            val (out2, ctx2) = f2.prettyString(ctx1, depth + 2)
            (s"""${indent}andThen
               |${indentInner}- leftFunc:
               |$out1
               |${indentInner}- rightFunc:
               |$out2""".stripMargin, ctx2)
        }
    }

    case class UnwrappedFunction[T: PrettyPrint, R: PrettyPrint](mapping: Map[T, R], default: R) extends ParsleyFunction[T, R] {
        override def apply(x: T): R = mapping.getOrElse(x, default)

        def andThenUnwrapped[A: PrettyPrint](that: UnwrappedFunction[R, A]): UnwrappedFunction[T, A] = {
            val newMapping = mapping.collect {
                case (k, v) if that.mapping.contains(v) => (k, that.mapping(v))
                // Corner case if newDefault is not the same as that.default
                case (k, _) if that.mapping.contains(default) => (k, that.default)
            }
            val newDefault = that.mapping.getOrElse(default, that.default)
            UnwrappedFunction(newMapping, newDefault)
        }

        def map[A: PrettyPrint](func: R => A) = UnwrappedFunction(mapping.map { case (k, v) => (k, func(v)) }, func(default))

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val (mappingStr, ctx1) = mapping.foldLeft(("", ctx)) { case ((str, c), (k, v)) =>
                val (kStr, kCtx) = k.prettyPrint(c)
                val (vStr, vCtx) = v.prettyPrint(kCtx)
                (s"$str${Indent}case $kStr =>\n${Indent * 2}$vStr\n", vCtx)
            }
            val (defaultStr, ctx2) = default.prettyPrint(ctx1)
            val funcId = ctx2.length
            // Inspiration for the notation taken from Scala
            val str = s"""<func$funcId>: {
               |${mappingStr.stripLineEnd}
               |${Indent}case _ =>
               |${Indent * 2}$defaultStr
               |}""".stripMargin

            val indent = Indent * depth
            (s"$indent<func$funcId>", ctx2 :+ str)
        }
    }

    object UnwrappedFunction {
        implicit def genUnwrappedFunc[T: Arbitrary: PrettyPrint, R: Arbitrary: PrettyPrint]: Gen[UnwrappedFunction[T, R]] = for {
            // Avoid exploding the output with a massive map (completely bricked
            // my shell and everything was in runes for some reason)
            elems <- Gen.choose(1, 10)
            mapping <- Gen.mapOfN(elems, Gen.zip(Arbitrary.arbitrary[T], Arbitrary.arbitrary[R]))
            default <- Arbitrary.arbitrary[R]
        } yield UnwrappedFunction(mapping, default)

        implicit def arbUnwrappedFunc[T: Arbitrary: PrettyPrint, R: Arbitrary: PrettyPrint]: Arbitrary[UnwrappedFunction[T, R]] = Arbitrary(genUnwrappedFunc[T, R])
    }

    case class ApplyFunction[T: PrettyPrint, U](x: T) extends ParsleyFunction[ParsleyFunction[T, U], U] {
        override def apply(f: ParsleyFunction[T, U]): U = f(x)

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val (out, ctx1) = x.prettyPrint(ctx)
            (s"${indent}f => f($out)", ctx1)
        }
    }

    case class LeftFunction[T, +U]() extends ParsleyFunction[T, Either[T, U]] {
        override def apply(x: T): Either[T, U] = Left(x)

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            (s"${indent}x => Left(x)", ctx)
        }
    }

    case class RightFunction[T, U]() extends ParsleyFunction[U, Either[T, U]] {
        override def apply(x: U): Either[T, U] = Right(x)

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            (s"${indent}x => Right(x)", ctx)
        }
    }

    case class SomeFunction[T]() extends ParsleyFunction[T, Option[T]] {
        override def apply(x: T): Option[T] = Some(x)

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            (s"${indent}x => Some(x)", ctx)
        }
    }

    case class AtomExpression[T]() extends ParsleyFunction[T, HomogeneousExpr] {
        override def apply(x: T): HomogeneousExpr = HomogeneousExpr.AtomExpr(x)

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            (s"${indent}x => AtomExpr(x)", ctx)
        }
    }
}
