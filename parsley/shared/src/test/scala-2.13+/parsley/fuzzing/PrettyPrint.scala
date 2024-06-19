package parsley.fuzzing

import parsley.expr.Fixity

// A type class for pretty printing objects with additional context
trait PrettyPrint[T] {
    // Pretty prints an object of type T as a String, and also returns an
    // additional context to be printed at the end of the output.
    // This effectively replicates a Writer/State monad.
    def prettyPrint(obj: T, ctx: List[String], depth: Int = 0): (String, List[String])
}

object PrettyPrint {
    val Indent: String = "    "

    def apply[T: PrettyPrint]: PrettyPrint[T] = implicitly

    implicit class PrettyPrintExtension[T: PrettyPrint](obj: T) {
        def prettyPrint(ctx: List[String], depth: Int = 0): (String, List[String]) = PrettyPrint[T].prettyPrint(obj, ctx, depth)

        def prettyToString: String = {
            val (out, ctx) = PrettyPrint[T].prettyPrint(obj, List())
            s"$out\n\n${ctx.mkString("\n")}"
        }
    }

    implicit def prettyPrintChar: PrettyPrint[Char] = new PrettyPrint[Char] {
        override def prettyPrint(obj: Char, ctx: List[String], depth: Int): (String, List[String]) = (s"${Indent * depth}'$obj'", ctx)
    }

    implicit def prettyPrintString: PrettyPrint[String] = new PrettyPrint[String] {
        override def prettyPrint(obj: String, ctx: List[String], depth: Int): (String, List[String]) = (s"${Indent * depth}\"$obj\"", ctx)
    }

    // Default PrettyPrint instance for types is the simple #toString with no
    // additional context
    // implicit def prettyPrintAny[T]: PrettyPrint[T] = new PrettyPrint[T] {
    //     override def prettyPrint(obj: T, ctx: List[String], _depth: Int): (String, List[String]) = (obj.toString, ctx)
    // }

    implicit def prettyPrintInt: PrettyPrint[Int] = new PrettyPrint[Int] {
        override def prettyPrint(obj: Int, ctx: List[String], depth: Int): (String, List[String]) = (s"${Indent * depth}$obj", ctx)
    }

    implicit def prettyPrintUnit: PrettyPrint[Unit] = new PrettyPrint[Unit] {
        override def prettyPrint(obj: Unit, ctx: List[String], depth: Int): (String, List[String]) = (s"${Indent * depth}()", ctx)
    }

    implicit def prettyPrintBoolean: PrettyPrint[Boolean] = new PrettyPrint[Boolean] {
        override def prettyPrint(obj: Boolean, ctx: List[String], depth: Int): (String, List[String]) = (s"${Indent * depth}$obj", ctx)
    }

    implicit def prettyPrintNothing: PrettyPrint[Nothing] = new PrettyPrint[Nothing] {
        override def prettyPrint(obj: Nothing, ctx: List[String], depth: Int): (String, List[String]) = (s"${Indent * depth}<Nothing>", ctx)
    }

    implicit def prettyPrintEither[T: PrettyPrint, U: PrettyPrint]: PrettyPrint[Either[T, U]] = new PrettyPrint[Either[T, U]] {
        override def prettyPrint(obj: Either[T, U], ctx: List[String], depth: Int): (String, List[String]) = obj match {
            case Left(value) => {
                val (out, ctx1) = value.prettyPrint(ctx)
                (s"${Indent * depth}Left($out)", ctx1)
            }
            case Right(value) => {
                val (out, ctx1) = value.prettyPrint(ctx)
                (s"${Indent * depth}Right($out)", ctx1)
            }
        }
    }

    implicit def prettyPrintOption[T: PrettyPrint]: PrettyPrint[Option[T]] = new PrettyPrint[Option[T]] {
        override def prettyPrint(obj: Option[T], ctx: List[String], depth: Int): (String, List[String]) = obj match {
            case Some(value) => {
                val (out, ctx1) = value.prettyPrint(ctx)
                (s"${Indent * depth}Some($out)", ctx1)
            }
            case None => (s"${Indent * depth}None", ctx)
        }
    }

    implicit def prettyPrintList[T: PrettyPrint]: PrettyPrint[List[T]] = new PrettyPrint[List[T]] {
        override def prettyPrint(obj: List[T], ctx: List[String], depth: Int): (String, List[String]) = {
            val (strList, c) = obj.foldLeft((List[String](), ctx)) { case ((strList, c), o) =>
                val (oStr, oCtx) = o.prettyPrint(c)
                (strList :+ oStr, oCtx)
            }
            (s"${Indent * depth}[${strList.mkString(", ")}]", c)
        }
    }

    implicit def prettyPrintTuple[T: PrettyPrint, U: PrettyPrint]: PrettyPrint[(T, U)] = new PrettyPrint[(T, U)] {
        override def prettyPrint(obj: (T, U), ctx: List[String], depth: Int): (String, List[String]) = {
            val (leftValue, rightValue) = obj
            val (leftOut, leftCtx) = leftValue.prettyPrint(ctx)
            val (rightOut, rightCtx) = rightValue.prettyPrint(leftCtx)
            (s"${Indent * depth}($leftOut, $rightOut)", rightCtx)
        }
    }

    implicit def prettyPrintHomogeneousExpr: PrettyPrint[HomogeneousExpr] = new PrettyPrint[HomogeneousExpr] {
        override def prettyPrint(obj: HomogeneousExpr, ctx: List[String], depth: Int): (String, List[String]) = (s"${Indent * depth}${obj.toString}", ctx)
    }

    implicit def prettyPrintFixity: PrettyPrint[Fixity] = new PrettyPrint[Fixity] {
        override def prettyPrint(obj: Fixity, ctx: List[String], depth: Int): (String, List[String]) = (s"${Indent * depth}${obj.toString}", ctx)
    }
}
