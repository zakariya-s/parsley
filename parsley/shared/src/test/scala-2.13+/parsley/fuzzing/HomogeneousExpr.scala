package parsley.fuzzing

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Shrink

sealed abstract class HomogeneousExpr(val binding: Int)
object HomogeneousExpr {
    case class LeftBinaryExpr(b: Int, leftExpr: HomogeneousExpr, rightExpr: HomogeneousExpr) extends HomogeneousExpr(b) {
        require(binding > 0, "Binding is zero in non-atomic expression")
        require(leftExpr.binding <= binding, "Invalid binding of left expression in left-associative expression")
        require(rightExpr.binding < binding, "Invalid binding of right expression in left-associative expression")
    }
    case class NonBinaryExpr(b: Int, leftExpr: HomogeneousExpr, rightExpr: HomogeneousExpr) extends HomogeneousExpr(b) {
        require(binding > 0, "Binding is zero in non-atomic expression")
        require(leftExpr.binding < binding, "Invalid binding of left expression in non-associative expression")
        require(rightExpr.binding < binding, "Invalid binding of right expression in non-associative expression")
    }
    case class RightBinaryExpr(b: Int, leftExpr: HomogeneousExpr, rightExpr: HomogeneousExpr) extends HomogeneousExpr(b) {
        require(binding > 0, "Binding is zero in non-atomic expression")
        require(leftExpr.binding < binding, "Invalid binding of left expression in right-associative expression")
        require(rightExpr.binding <= binding, "Invalid binding of right expression in right-associative expression")
    }
    case class PrefixUnaryExpr(b: Int, expr: HomogeneousExpr) extends HomogeneousExpr(b) {
        require(binding > 0, "Binding is zero in non-atomic expression")
        require(expr.binding <= binding, "Invalid binding of inner expression in prefix expression")
    }
    case class PostfixUnaryExpr(b: Int, expr: HomogeneousExpr) extends HomogeneousExpr(b) {
        require(binding > 0, "Binding is zero in non-atomic expression")
        require(expr.binding <= binding, "Invalid binding of inner expression in postfix expression")
    }
    case class AtomExpr(value: Any) extends HomogeneousExpr(0)

    def generateHomogeneousExpr(maxBinding: Int): Gen[HomogeneousExpr] = Gen.frequency(
        ((1 -> Gen.oneOf[Any](
            Arbitrary.arbitrary[Int],
            Arbitrary.arbitrary[Boolean],
            Gen.asciiPrintableChar,
            Gen.asciiPrintableStr
        ).map(AtomExpr)) +:
        (if (maxBinding != 0) List(
            2 -> (for {
                binding <- Gen.choose(1, maxBinding)
                leftExpr <- generateHomogeneousExpr(binding)
                rightExpr <- generateHomogeneousExpr(binding - 1)
            } yield LeftBinaryExpr(binding, leftExpr, rightExpr)),
            2 -> (for {
                binding <- Gen.choose(1, maxBinding)
                leftExpr <- generateHomogeneousExpr(binding - 1)
                rightExpr <- generateHomogeneousExpr(binding - 1)
            } yield NonBinaryExpr(binding, leftExpr, rightExpr)),
            2 -> (for {
                binding <- Gen.choose(1, maxBinding)
                leftExpr <- generateHomogeneousExpr(binding - 1)
                rightExpr <- generateHomogeneousExpr(binding)
            } yield RightBinaryExpr(binding, leftExpr, rightExpr)),
            2 -> (for {
                binding <- Gen.choose(1, maxBinding)
                expr <- generateHomogeneousExpr(binding)
            } yield PrefixUnaryExpr(binding, expr)),
            2 -> (for {
                binding <- Gen.choose(1, maxBinding)
                expr <- generateHomogeneousExpr(binding)
            } yield PostfixUnaryExpr(binding, expr))
        ) else List())): _*
    )

    implicit val genHomogeneousExpr: Gen[HomogeneousExpr] = Gen.choose(0, 20).flatMap(generateHomogeneousExpr)

    implicit val arbHomogeneousExpr: Arbitrary[HomogeneousExpr] = Arbitrary(genHomogeneousExpr)

    implicit val shrinkHomogeneousExpr: Shrink[HomogeneousExpr] = Shrink.shrinkAny
}
