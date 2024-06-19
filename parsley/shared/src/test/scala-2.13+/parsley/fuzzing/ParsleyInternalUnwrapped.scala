package parsley.fuzzing

import parsley.Parsley
import parsley.combinator.{ifS, optional, option}
import parsley.character.{char, string}
import parsley.expr.{precedence, Fixity, Ops, InfixL, InfixN, InfixR, Postfix, Prefix}

import parsley.fuzzing.PrettyPrint._
import java.io.{StringWriter, PrintWriter}

// Second-stage ADT
sealed abstract class ParsleyInternalUnwrapped[+T: PrettyPrint](val input: String, val output: Option[T]) {
    def generate: Parsley[T]

    private def getStackTrace(e: Throwable): String = {
        val sw = new StringWriter
        val pw = new PrintWriter(sw)
        e.printStackTrace(pw)
        pw.close()
        sw.close()
        sw.toString()
    }

    protected def prettyPrintParsleyOutput(ctx: List[String]): (Option[T], String, List[String]) = {
        val result = try {
            Right(generate.parse(input))
        } catch {
            case e: Throwable => Left(e)
        }
        result.fold(err => {
            val exceptionId = ctx.length
            val stackTrace = getStackTrace(err)
            (None, s"<exception$exceptionId>", ctx :+ s"<exception$exceptionId>: $stackTrace")
        }, _.fold(err => {
            val errId = ctx.length
            (None, s"<error$errId>", ctx :+ s"<error$errId>: $err")
        }, o => {
            val (outStr, ctx1) = o.prettyPrint(ctx)
            (Some(o), outStr, ctx1)
        }))
    }

    def prettyString(ctx: List[String], depth: Int = 0): (String, List[String])

    def optimize: ParsleyInternalUnwrapped[T]

    def disableOptimizations: ParsleyInternalUnwrapped[T]
    
    override def toString = this.prettyToString

    final def evPrettyPrint: PrettyPrint[Any] = implicitly[PrettyPrint[T]].asInstanceOf[PrettyPrint[Any]]
}

object ParsleyInternalUnwrapped {
    final val Indent: String = "    "

    implicit def prettyPrintParsleyInternalUnwrapped[T]: PrettyPrint[ParsleyInternalUnwrapped[T]] = new PrettyPrint[ParsleyInternalUnwrapped[T]] {
        override def prettyPrint(obj: ParsleyInternalUnwrapped[T], ctx: List[String], depth: Int): (String, List[String]) = obj.prettyString(ctx, depth)
    }

    // The output of Empty is Nothing, but it cannot be instantiated so we have
    // to use an Option for the output type
    final case class Empty() extends ParsleyInternalUnwrapped[Nothing]("", None) {
        override def generate: Parsley[Nothing] = Parsley.empty

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            (s"${indent}empty", ctx)
        }

        override def optimize: ParsleyInternalUnwrapped[Nothing] = this

        override def disableOptimizations: ParsleyInternalUnwrapped[Nothing] = this
    }

    final case class PChar(c: Char, i: String) extends ParsleyInternalUnwrapped[Char](i, if (i.length == 1 && c == i.charAt(0)) Some(c) else None) {
        def this(c: Char) = this(c, c.toString)

        override def generate: Parsley[Char] = char(c)

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            (s"${indent}char('$c')", ctx)
        }

        override def optimize: ParsleyInternalUnwrapped[Char] = this

        override def disableOptimizations: ParsleyInternalUnwrapped[Char] = Impure(this)
    }

    final case class PString(s: String, i: String) extends ParsleyInternalUnwrapped[String](i, if (s == i) Some(s) else None) {
        def this(s: String) = this(s, s)

        override def generate: Parsley[String] = string(s)

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            (s"""${indent}string("$s")""", ctx)
        }

        override def optimize: ParsleyInternalUnwrapped[String] = this

        override def disableOptimizations: ParsleyInternalUnwrapped[String] = Impure(this)
    }

    final case class Pure[T: PrettyPrint](value: T) extends ParsleyInternalUnwrapped[T]("", Some(value)) {
        override def generate: Parsley[T] = Parsley.pure(value)

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val (valueStr, valueCtx) = value.prettyPrint(ctx)
            (s"${indent}pure($valueStr)", valueCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[T] = this

        override def disableOptimizations: ParsleyInternalUnwrapped[T] = Impure(this)
    }

    final case class Impure[T: PrettyPrint](node: ParsleyInternalUnwrapped[T]) extends ParsleyInternalUnwrapped[T](node.input, node.output) {
        override def generate: Parsley[T] = node.generate.impure

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (nodeStr, nodeCtx) = node.prettyString(outCtx, depth + 2)
            
            (s"""${indent}impure${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- node:
               |$nodeStr""".stripMargin, nodeCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[T] = node.optimize

        override def disableOptimizations: ParsleyInternalUnwrapped[T] = Impure(node.disableOptimizations)
    }

    final case class Atomic[T: PrettyPrint](node: ParsleyInternalUnwrapped[T]) extends ParsleyInternalUnwrapped[T](node.input, node.output) {
        override def generate: Parsley[T] = Parsley.atomic(node.generate)

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (nodeStr, nodeCtx) = node.prettyString(outCtx, depth + 2)

            (s"""${indent}atomic${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- node:
               |$nodeStr""".stripMargin, nodeCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[T] = Atomic(node.optimize)

        override def disableOptimizations: ParsleyInternalUnwrapped[T] = Impure(Atomic(node.disableOptimizations))
    }

    final case class LookAhead[T: PrettyPrint](node: ParsleyInternalUnwrapped[T]) extends ParsleyInternalUnwrapped[T](node.input, node.output) {
        override def generate: Parsley[T] = {
            val parsleyNode = node.generate
            Parsley.lookAhead(parsleyNode) *> parsleyNode
        }

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (nodeStr, nodeCtx) = node.prettyString(outCtx, depth + 2)

            (s"""${indent}lookAhead${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- node:
               |$nodeStr""".stripMargin, nodeCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[T] = LookAhead(node.optimize)

        override def disableOptimizations: ParsleyInternalUnwrapped[T] = Impure(LookAhead(node.disableOptimizations))
    }

    final case class NotFollowedBy[T](node: ParsleyInternalUnwrapped[T], i: String) extends ParsleyInternalUnwrapped[Unit](i, if (i.isEmpty) Some(()) else None) {
        def this(node: ParsleyInternalUnwrapped[T]) = this(node, "")
        
        override def generate: Parsley[Unit] = Parsley.notFollowedBy(node.generate)

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (nodeStr, nodeCtx) = node.prettyString(outCtx, depth + 2)

            (s"""${indent}notFollowedBy${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- node:
               |$nodeStr""".stripMargin, nodeCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[Unit] = NotFollowedBy(node.optimize, i)

        override def disableOptimizations: ParsleyInternalUnwrapped[Unit] = Impure(NotFollowedBy(node.disableOptimizations, i))
    }

    final case class POptional[T](node: ParsleyInternalUnwrapped[T], taken: Boolean) extends ParsleyInternalUnwrapped[Unit](if (taken) node.input else "", if (node.output.isDefined) Some(()) else None) {
        override def generate: Parsley[Unit] = optional(node.generate)

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (nodeStr, nodeCtx) = node.prettyString(outCtx, depth + 2)

            (s"""${indent}optional${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- node:
               |$nodeStr""".stripMargin, nodeCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[Unit] = POptional(node.optimize, taken)

        override def disableOptimizations: ParsleyInternalUnwrapped[Unit] = Impure(POptional(node.disableOptimizations, taken))
    }

    final case class POption[T: PrettyPrint](node: ParsleyInternalUnwrapped[T], taken: Boolean) extends ParsleyInternalUnwrapped[Option[T]](if (taken) node.input else "", if (node.output.isDefined) { if (taken) Some(node.output) else Some(None) } else None) {
        override def generate: Parsley[Option[T]] = option(node.generate)

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (nodeStr, nodeCtx) = node.prettyString(outCtx, depth + 2)

            (s"""${indent}option${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- node:
               |$nodeStr""".stripMargin, nodeCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[Option[T]] = POption(node.optimize, taken)

        override def disableOptimizations: ParsleyInternalUnwrapped[Option[T]] = Impure(POption(node.disableOptimizations, taken))
    }

    final case class Then[T: PrettyPrint, U](left: ParsleyInternalUnwrapped[U], right: ParsleyInternalUnwrapped[T]) extends ParsleyInternalUnwrapped[T](left.input + right.input, right.output) {
        override def generate: Parsley[T] = left.generate *> right.generate

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (leftStr, leftCtx) = left.prettyString(outCtx, depth + 2)
            val (rightStr, rightCtx) = right.prettyString(leftCtx, depth + 2)

            (s"""${indent}*>${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- left:
               |$leftStr
               |${indentInner}- right:
               |$rightStr""".stripMargin, rightCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[T] = Then(left.optimize, right.optimize)

        override def disableOptimizations: ParsleyInternalUnwrapped[T] = Impure(Then(left.disableOptimizations, right.disableOptimizations))
    }

    final case class ThenDiscard[T: PrettyPrint, U](left: ParsleyInternalUnwrapped[T], right: ParsleyInternalUnwrapped[U]) extends ParsleyInternalUnwrapped[T](left.input + right.input, left.output) {
        override def generate: Parsley[T] = left.generate <* right.generate

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (leftStr, leftCtx) = left.prettyString(outCtx, depth + 2)
            val (rightStr, rightCtx) = right.prettyString(leftCtx, depth + 2)

            (s"""${indent}<*${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- left:
               |$leftStr
               |${indentInner}- right:
               |$rightStr""".stripMargin, rightCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[T] = ThenDiscard(left.optimize, right.optimize)

        override def disableOptimizations: ParsleyInternalUnwrapped[T] = Impure(ThenDiscard(left.disableOptimizations, right.disableOptimizations))
    }

    final case class Or[T: PrettyPrint](left: ParsleyInternalUnwrapped[T], right: ParsleyInternalUnwrapped[T], leftParser: Boolean) extends ParsleyInternalUnwrapped[T](if (leftParser) left.input else right.input, if (leftParser) left.output else right.output) {
        override def generate: Parsley[T] = left.generate <|> right.generate

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (leftStr, leftCtx) = left.prettyString(outCtx, depth + 2)
            val (rightStr, rightCtx) = right.prettyString(leftCtx, depth + 2)

            (s"""${indent}<|>${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- left${if (leftParser) " (taken)" else ""}:
               |$leftStr
               |${indentInner}- right${if (!leftParser) " (taken)" else ""}:
               |$rightStr""".stripMargin, rightCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[T] = (left.optimize, right.optimize) match {
            case (Or(ll, lr, lp), r) => Or(ll, Or(lr, r, leftParser && !lp), leftParser && lp)
            case (l, Or(rl, rr, lp)) => Or(Or(l, rl, leftParser), rr, leftParser || lp)
            case (Empty(), r) => r
            case (l, Empty()) => l
            case (l, r) => Or(l, r, leftParser)
        }

        override def disableOptimizations: ParsleyInternalUnwrapped[T] = Impure(Or(left.disableOptimizations, right.disableOptimizations, leftParser))
    }

    final case class Sum[T: PrettyPrint, U: PrettyPrint](left: ParsleyInternalUnwrapped[T], right: ParsleyInternalUnwrapped[U], leftParser: Boolean) extends ParsleyInternalUnwrapped[Either[T, U]](if (leftParser) left.input else right.input, if (leftParser && left.output.isDefined) Some(Left(left.output.get)) else if (right.output.isDefined) Some(Right(right.output.get)) else None) {
        override def generate: Parsley[Either[T, U]] = left.generate <+> right.generate

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (leftStr, leftCtx) = left.prettyString(outCtx, depth + 2)
            val (rightStr, rightCtx) = right.prettyString(leftCtx, depth + 2)

            (s"""${indent}<+>${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- left${if (leftParser) " (taken)" else ""}:
               |$leftStr
               |${indentInner}- right${if (!leftParser) " (taken)" else ""}:
               |$rightStr""".stripMargin, rightCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[Either[T, U]] = Sum(left.optimize, right.optimize, leftParser)

        override def disableOptimizations: ParsleyInternalUnwrapped[Either[T, U]] = Impure(Sum(left.disableOptimizations, right.disableOptimizations, leftParser))
    }

    final case class IfS[T: PrettyPrint](condP: ParsleyInternalUnwrapped[Boolean], thenP: ParsleyInternalUnwrapped[T], elseP: ParsleyInternalUnwrapped[T]) extends ParsleyInternalUnwrapped[T](condP.input + (if (condP.output.isDefined && condP.output.get) thenP.input else elseP.input), if (condP.output.isDefined) { if (condP.output.get) thenP.output else elseP.output } else None) {
        override def generate: Parsley[T] = ifS(condP.generate, thenP.generate, elseP.generate)

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (condStr, condCtx) = condP.prettyString(outCtx, depth + 2)
            val (thenStr, thenCtx) = thenP.prettyString(condCtx, depth + 2)
            val (elseStr, elseCtx) = elseP.prettyString(thenCtx, depth + 2)

            (s"""${indent}ifP${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- if:
               |$condStr
               |${indentInner}- then${if (condP.output.isDefined && condP.output.get) " (taken)" else ""}:
               |$thenStr
               |${indentInner}- else${if (condP.output.isDefined && !condP.output.get) " (taken)" else ""}:
               |$elseStr""".stripMargin, elseCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[T] = IfS(condP.optimize, thenP.optimize, elseP.optimize)

        override def disableOptimizations: ParsleyInternalUnwrapped[T] = Impure(IfS(condP.disableOptimizations, thenP.disableOptimizations, elseP.disableOptimizations))
    }

    final case class Map[T, U: PrettyPrint](node: ParsleyInternalUnwrapped[T], f: ParsleyFunction[T, U]) extends ParsleyInternalUnwrapped[U](node.input, if (node.output.isDefined) Some(f(node.output.get)) else None) {
        override def generate: Parsley[U] = node.generate.map(f)

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (nodeStr, nodeCtx) = node.prettyString(outCtx, depth + 2)
            val (fStr, fCtx) = f.prettyString(nodeCtx, depth + 2)

            (s"""${indent}map${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- node:
               |$nodeStr
               |${indentInner}- func:
               |$fStr""".stripMargin, fCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[U] = node.optimize match {
            case Map(n, g) => Map(n, g andThen f)
            case n => Map(n, f)
        }

        override def disableOptimizations: ParsleyInternalUnwrapped[U] = Impure(Map(node.disableOptimizations, f))
    }

    abstract class Repeat[T: PrettyPrint](nodes: List[ParsleyInternalUnwrapped[T]], repeatName: String) extends ParsleyInternalUnwrapped[List[T]](nodes.map(_.input).mkString, if (nodes.forall(_.output.isDefined)) Some(nodes.map(_.output.get)) else None) {
        require(!nodes.isEmpty, "Repetitive combinators must have at least one outcome")

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (nodeHeadStr, nodeHeadCtx) = nodes.head.prettyString(outCtx, depth + 2)

            (s"""${indent}$repeatName${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- iterations: ${nodes.length}
               |${indentInner}- fstNode:
               |$nodeHeadStr""".stripMargin, nodeHeadCtx)
        }
    }

    final case class Many[T: PrettyPrint](nodes: List[ParsleyInternalUnwrapped[T]]) extends Repeat[T](nodes, "many") {
        // TODO: See how this can be made to work with even 0 (since this is too restrictive)
        require(!nodes.isEmpty, "Many combinator must have at least one outcome")

        override def generate: Parsley[List[T]] = Parsley.many(nodes.head.generate)

        override def optimize: ParsleyInternalUnwrapped[List[T]] = Many(nodes.map(_.optimize))

        override def disableOptimizations: ParsleyInternalUnwrapped[List[T]] = Impure(Many(nodes.map(_.disableOptimizations)))
    }

    final case class PSome[T: PrettyPrint](nodes: List[ParsleyInternalUnwrapped[T]]) extends Repeat[T](nodes, "some") {
        require(!nodes.isEmpty, "Some combinator must have at least one outcome")

        override def generate: Parsley[List[T]] = Parsley.some(nodes.head.generate)

        override def optimize: ParsleyInternalUnwrapped[List[T]] = PSome(nodes.map(_.optimize))

        override def disableOptimizations: ParsleyInternalUnwrapped[List[T]] = Impure(PSome(nodes.map(_.disableOptimizations)))
    }

    final case class Filter[T: PrettyPrint](node: ParsleyInternalUnwrapped[T], outputs: Set[T]) extends ParsleyInternalUnwrapped[T](node.input, if (!outputs.isEmpty) node.output else None) {
        // Following constraint only true when generating correct parsers
        // require(outputs.contains(node.output), "Filter should always succeed")

        override def generate: Parsley[T] = node.generate.filter(outputs.contains(_))

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val indentInnerInner = Indent * (depth + 2)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (outputsStr, outputsCtx) = outputs.foldLeft(("", outCtx)) { case ((str, c), o) =>
                val (oStr, oCtx) = o.prettyPrint(c)
                (s"$str${indentInnerInner}- $oStr\n", oCtx)
            }
            val (nodeStr, nodeCtx) = node.prettyString(outputsCtx, depth + 2)

            (s"""${indent}filter${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- possibleOutputs:
               |${outputsStr.stripLineEnd}
               |${indentInner}- node:
               |$nodeStr""".stripMargin, nodeCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[T] = Filter(node.optimize, outputs)

        override def disableOptimizations: ParsleyInternalUnwrapped[T] = Impure(Filter(node.disableOptimizations, outputs))
    }

    final case class Ap[T: PrettyPrint, U: PrettyPrint](funcNode: ParsleyInternalUnwrapped[ParsleyFunction[T, U]], argNode: ParsleyInternalUnwrapped[T]) extends ParsleyInternalUnwrapped[U](funcNode.input + argNode.input, if (funcNode.output.isDefined && argNode.output.isDefined) Some(funcNode.output.get(argNode.output.get)) else None) {
        override def generate: Parsley[U] = funcNode.generate <*> argNode.generate

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (funcStr, funcCtx) = funcNode.prettyString(outCtx, depth + 2)
            val (argStr, argCtx) = argNode.prettyString(funcCtx, depth + 2)

            (s"""${indent}<*>${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- funcNode:
               |$funcStr
               |${indentInner}- argNode:
               |$argStr""".stripMargin, argCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[U] = (funcNode.optimize, argNode.optimize) match {
            case (Pure(f), Pure(x)) => Pure(f(x))
            case (u, Pure(x)) => Ap(Pure(ParsleyFunction.ApplyFunction[T, U](x): ParsleyFunction[ParsleyFunction[T, U], U]), u)
            case (Empty(), _) => Empty()
            case (f, x) => Ap(f, x)
        }

        override def disableOptimizations: ParsleyInternalUnwrapped[U] = Impure(Ap(funcNode.disableOptimizations, argNode.disableOptimizations))
    }

    final case class ReverseAp[T: PrettyPrint, U: PrettyPrint](argNode: ParsleyInternalUnwrapped[T], funcNode: ParsleyInternalUnwrapped[ParsleyFunction[T, U]]) extends ParsleyInternalUnwrapped[U](argNode.input + funcNode.input, if (argNode.output.isDefined && funcNode.output.isDefined) Some(funcNode.output.get(argNode.output.get)) else None) {
        override def generate: Parsley[U] = argNode.generate <**> funcNode.generate

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (argStr, argCtx) = argNode.prettyString(outCtx, depth + 2)
            val (funcStr, funcCtx) = funcNode.prettyString(argCtx, depth + 2)

            (s"""${indent}<**>${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- argNode:
               |$argStr
               |${indentInner}- funcNode:
               |$funcStr""".stripMargin, funcCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[U] = (argNode.optimize, funcNode.optimize) match {
            case (Pure(x), Pure(f)) => Pure(f(x))
            case (Pure(x), u) => ReverseAp(u, Pure(ParsleyFunction.ApplyFunction[T, U](x): ParsleyFunction[ParsleyFunction[T, U], U]))
            case (x, f) => ReverseAp(x, f)
        }

        override def disableOptimizations: ParsleyInternalUnwrapped[U] = Impure(ReverseAp(argNode.disableOptimizations, funcNode.disableOptimizations))
    }

    final case class Cons[T: PrettyPrint](elemNode: ParsleyInternalUnwrapped[T], listNode: ParsleyInternalUnwrapped[List[T]]) extends ParsleyInternalUnwrapped[List[T]](elemNode.input + listNode.input, if (elemNode.output.isDefined && listNode.output.isDefined) Some(elemNode.output.get :: listNode.output.get) else None) {
        override def generate: Parsley[List[T]] = elemNode.generate <::> listNode.generate

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (elemStr, elemCtx) = elemNode.prettyString(outCtx, depth + 2)
            val (listStr, listCtx) = listNode.prettyString(elemCtx, depth + 2)

            (s"""${indent}::${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- head:
               |$elemStr
               |${indentInner}- tail:
               |$listStr""".stripMargin, listCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[List[T]] = (elemNode.optimize, listNode.optimize) match {
            case (Pure(x), Pure(xs)) => Pure(x :: xs)
            case (x, xs) => Cons(x, xs)
        }

        override def disableOptimizations: ParsleyInternalUnwrapped[List[T]] = Impure(Cons(elemNode.disableOptimizations, listNode.disableOptimizations))
    }

    final case class Zip[T: PrettyPrint, U: PrettyPrint](leftNode: ParsleyInternalUnwrapped[T], rightNode: ParsleyInternalUnwrapped[U]) extends ParsleyInternalUnwrapped[(T, U)](leftNode.input + rightNode.input, if (leftNode.output.isDefined && rightNode.output.isDefined) Some((leftNode.output.get, rightNode.output.get)) else None) {
        override def generate: Parsley[(T, U)] = leftNode.generate <~> rightNode.generate

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (leftStr, leftCtx) = leftNode.prettyString(outCtx, depth + 2)
            val (rightStr, rightCtx) = rightNode.prettyString(leftCtx, depth + 2)

            (s"""${indent}<~>${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- left:
               |$leftStr
               |${indentInner}- right:
               |$rightStr""".stripMargin, rightCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[(T, U)] = (leftNode.optimize, rightNode.optimize) match {
            case (Pure(l), Pure(r)) => Pure((l, r))
            case (l, r) => Zip(l, r)
        }

        override def disableOptimizations: ParsleyInternalUnwrapped[(T, U)] = Impure(Zip(leftNode.disableOptimizations, rightNode.disableOptimizations))
    }

    final case class Branch[T: PrettyPrint, U: PrettyPrint, V: PrettyPrint](eitherNode: ParsleyInternalUnwrapped[Either[T, U]], leftNode: ParsleyInternalUnwrapped[ParsleyFunction[T, V]], rightNode: ParsleyInternalUnwrapped[ParsleyFunction[U, V]]) extends ParsleyInternalUnwrapped[V](eitherNode.input + (if (eitherNode.output.isDefined && eitherNode.output.get.isLeft) leftNode.input else rightNode.input), if (eitherNode.output.isDefined && leftNode.output.isDefined && rightNode.output.isDefined) Some(eitherNode.output.get match { case Left(out) => leftNode.output.get(out); case Right(out) => rightNode.output.get(out) }) else None) {
        override def generate: Parsley[V] = Parsley.branch(eitherNode.generate, leftNode.generate, rightNode.generate)

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (eitherStr, eitherCtx) = eitherNode.prettyString(outCtx, depth + 2)
            val (leftStr, leftCtx) = leftNode.prettyString(eitherCtx, depth + 2)
            val (rightStr, rightCtx) = rightNode.prettyString(leftCtx, depth + 2)

            (s"""${indent}branch${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- ifEither:
               |$eitherStr
               |${indentInner}- leftFunc${if (eitherNode.output.isDefined && eitherNode.output.get.isLeft) " (taken)" else ""}:
               |$leftStr
               |${indentInner}- rightFunc${if (eitherNode.output.isDefined && !eitherNode.output.get.isLeft) " (taken)" else ""}:
               |$rightStr""".stripMargin, rightCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[V] = Branch(eitherNode.optimize, leftNode.optimize, rightNode.optimize)

        override def disableOptimizations: ParsleyInternalUnwrapped[V] = Impure(Branch(eitherNode.disableOptimizations, leftNode.disableOptimizations, rightNode.disableOptimizations))
    }

    final case class FlatMap[T, U: PrettyPrint](node: ParsleyInternalUnwrapped[T], func: ParsleyFunction.UnwrappedFunction[T, ParsleyInternalUnwrapped[U]]) extends ParsleyInternalUnwrapped[U](node.input + (if (node.output.isDefined) func(node.output.get).input else ""), if (node.output.isDefined) func(node.output.get).output else None) {
        override def generate: Parsley[U] = node.generate.flatMap(func(_).generate)

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (nodeStr, nodeCtx) = node.prettyString(outCtx, depth + 2)
            val (funcStr, funcCtx) = func.prettyString(nodeCtx, depth + 2)

            (s"""${indent}flatMap${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- node:
               |$nodeStr
               |${indentInner}- func:
               |$funcStr""".stripMargin, funcCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[U] = FlatMap(node.optimize, func.map(_.optimize))

        override def disableOptimizations: ParsleyInternalUnwrapped[U] = Impure(FlatMap(node.disableOptimizations, func.map(_.disableOptimizations)))
    }

    final case class Flatten[T: PrettyPrint](node: ParsleyInternalUnwrapped[ParsleyInternalUnwrapped[T]]) extends ParsleyInternalUnwrapped[T](node.input + (if (node.output.isDefined) node.output.get.input else ""), if (node.output.isDefined) node.output.get.output else None) {
        override def generate: Parsley[T] = node.generate.map(_.generate).flatten

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (nodeStr, nodeCtx) = node.prettyString(outCtx, depth + 2)

            (s"""${indent}flatMap${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- node:
               |$nodeStr""".stripMargin, nodeCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[T] = Flatten(node.optimize)

        override def disableOptimizations: ParsleyInternalUnwrapped[T] = Impure(Flatten(node.disableOptimizations))
    }

    final case class Fresh[T: PrettyPrint](value: T) extends ParsleyInternalUnwrapped[T]("", Some(value)) {
        override def generate: Parsley[T] = Parsley.fresh(value)

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val (valueStr, valueCtx) = value.prettyPrint(ctx)
            (s"${indent}fresh($valueStr)", valueCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[T] = this

        override def disableOptimizations: ParsleyInternalUnwrapped[T] = Impure(this)
    }

    final case class Select[T: PrettyPrint, U: PrettyPrint](eitherNode: ParsleyInternalUnwrapped[Either[T, U]], leftFuncNode: ParsleyInternalUnwrapped[ParsleyFunction[T, U]]) extends ParsleyInternalUnwrapped[U](eitherNode.input + (if (eitherNode.output.isDefined && eitherNode.output.get.isLeft) leftFuncNode.input else ""), (eitherNode.output, leftFuncNode.output) match { case (Some(Left(out)), Some(leftOut)) => Some(leftOut(out)); case (Some(Right(out)), _) => Some(out); case _ => None }) {
        override def generate: Parsley[U] = Parsley.select(eitherNode.generate, leftFuncNode.generate)

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (eitherStr, eitherCtx) = eitherNode.prettyString(outCtx, depth + 2)
            val (leftStr, leftCtx) = leftFuncNode.prettyString(eitherCtx, depth + 2)

            (s"""${indent}select${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- ifEither:
               |$eitherStr
               |${indentInner}- leftFunc${if (eitherNode.output.isDefined && eitherNode.output.get.isLeft) " (taken)" else ""}:
               |$leftStr""".stripMargin, leftCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[U] = Select(eitherNode.optimize, leftFuncNode.optimize)

        override def disableOptimizations: ParsleyInternalUnwrapped[U] = Impure(Select(eitherNode.disableOptimizations, leftFuncNode.disableOptimizations))
    }

    final case class Collect[T: PrettyPrint, U: PrettyPrint](node: ParsleyInternalUnwrapped[T], outputs: Set[T], res: U) extends ParsleyInternalUnwrapped[U](node.input, if (node.output.isDefined) Some(res) else None) {
        // require(outputs.contains(node.output), "Collect should always succeed")

        override def generate: Parsley[U] = node.generate.collect(outputs.map(_ -> res).toMap)

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val indentInnerInner = Indent * (depth + 2)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (outputsStr, outputsCtx) = outputs.foldLeft(("", outCtx)) { case ((str, c), o) =>
                val (oStr, oCtx) = o.prettyPrint(c)
                (s"$str${indentInnerInner}- $oStr\n", oCtx)
            }
            val (nodeStr, nodeCtx) = node.prettyString(outputsCtx, depth + 2)

            (s"""${indent}collect${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- possibleFilteredOutputs:
               |${outputsStr.stripLineEnd}
               |${indentInner}- node:
               |$nodeStr""".stripMargin, nodeCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[U] = Collect(node.optimize, outputs, res)

        override def disableOptimizations: ParsleyInternalUnwrapped[U] = Impure(Collect(node.disableOptimizations, outputs, res))
    }

    final case class MapFilter[T: PrettyPrint, U: PrettyPrint](node: ParsleyInternalUnwrapped[T], outputs: Set[T], res: U) extends ParsleyInternalUnwrapped[U](node.input, if (node.output.isDefined) Some(res) else None) {
        // require(outputs.contains(node.output), "MapFilter should always succeed")

        override def generate: Parsley[U] = node.generate.mapFilter(ParsleyFunction.UnwrappedFunction(outputs.map(_ -> Some(res)).toMap, None))

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val indentInnerInner = Indent * (depth + 2)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (outputsStr, outputsCtx) = outputs.foldLeft(("", outCtx)) { case ((str, c), o) =>
                val (oStr, oCtx) = o.prettyPrint(c)
                (s"$str${indentInnerInner}- $oStr\n", oCtx)
            }
            val (nodeStr, nodeCtx) = node.prettyString(outputsCtx, depth + 2)

            (s"""${indent}mapFilter${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- possibleFilteredOutputs:
               |${outputsStr.stripLineEnd}
               |${indentInner}- node:
               |$nodeStr""".stripMargin, nodeCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[U] = MapFilter(node.optimize, outputs, res)

        override def disableOptimizations: ParsleyInternalUnwrapped[U] = Impure(MapFilter(node.disableOptimizations, outputs, res))
    }

    abstract class PFold[T, U: PrettyPrint](
        nodes: List[ParsleyInternalUnwrapped[T]],
        initialValue: U,
        funcNode: Either[ParsleyFunction[(U, T), U], ParsleyFunction[(T, U), U]],
        foldName: String
    ) extends ParsleyInternalUnwrapped[U](
        nodes.map(_.input).mkString,
        if (nodes.forall(_.output.isDefined)) Some ({
            val outputs = nodes.map(_.output.get)
            funcNode match {
                case Left(f) => {
                    val func: ((U, T) => U) = (u, t) => f((u, t))
                    outputs.foldLeft(initialValue)(func)
                }
                case Right(f) => {
                    val func: ((T, U) => U) = (t, u) => f((t, u))
                    outputs.foldRight(initialValue)(func)
                }
            }
        }) else None
    ) {
        // TODO: See how this can be made to work with even 0 (since this is too restrictive)
        require(!nodes.isEmpty, "Fold combinators must have at least one outcome")

        override def generate: Parsley[U] = funcNode match {
            case Left(f) => {
                val func: ((U, T) => U) = (u, t) => f((u, t))
                nodes.head.generate.foldLeft(initialValue)(func)
            }
            case Right(f) => {
                val func: ((T, U) => U) = (t, u) => f((t, u))
                nodes.head.generate.foldRight(initialValue)(func)
            }
        }

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (initialStr, initialCtx) = initialValue.prettyPrint(outCtx)
            val (funcStr, funcCtx) = funcNode match {
                case Left(func) => func.prettyString(initialCtx, depth + 2)
                case Right(func) => func.prettyString(initialCtx, depth + 2)
            }
            val (nodeHeadStr, nodeHeadCtx) = nodes.head.prettyString(funcCtx, depth + 2)

            (s"""${indent}$foldName${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- iterations: ${nodes.length}
               |${indentInner}- initialValue: $initialStr
               |${indentInner}- funcNode:
               |$funcStr
               |${indentInner}- fstNode:
               |$nodeHeadStr""".stripMargin, nodeHeadCtx)
        }
    }

    abstract class PReduce[T, U >: T: PrettyPrint](
        nodes: List[ParsleyInternalUnwrapped[T]],
        funcNode: Either[ParsleyFunction[(U, T), U], ParsleyFunction[(T, U), U]],
        reduceName: String
    ) extends ParsleyInternalUnwrapped[U](
        nodes.map(_.input).mkString,
        if (nodes.forall(_.output.isDefined)) Some ({
            val outputs = nodes.map(_.output.get)
            funcNode match {
                case Left(f) => {
                    val func: ((U, T) => U) = (u, t) => f((u, t))
                    outputs.reduceLeft(func)
                }
                case Right(f) => {
                    val func: ((T, U) => U) = (t, u) => f((t, u))
                    outputs.reduceRight(func)
                }
            }
        }) else None
    ) {
        require(!nodes.isEmpty, "Reduce combinators must have at least one outcome")

        override def generate: Parsley[U] = funcNode match {
            case Left(f) => {
                val func: ((U, T) => U) = (u, t) => f((u, t))
                nodes.head.generate.reduceLeft(func)
            }
            case Right(f) => {
                val func: ((T, U) => U) = (t, u) => f((t, u))
                nodes.head.generate.reduceRight(func)
            }
        }

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (funcStr, funcCtx) = funcNode match {
                case Left(func) => func.prettyString(outCtx, depth + 2)
                case Right(func) => func.prettyString(outCtx, depth + 2)
            }
            val (nodeHeadStr, nodeHeadCtx) = nodes.head.prettyString(funcCtx, depth + 2)

            (s"""${indent}$reduceName${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- iterations: ${nodes.length}
               |${indentInner}- funcNode:
               |$funcStr
               |${indentInner}- fstNode:
               |$nodeHeadStr""".stripMargin, nodeHeadCtx)
        }
    }

    abstract class PReduceOption[T, U >: T: PrettyPrint](
        nodes: List[ParsleyInternalUnwrapped[T]],
        funcNode: Either[ParsleyFunction[(U, T), U], ParsleyFunction[(T, U), U]],
        reduceName: String
    ) extends ParsleyInternalUnwrapped[Option[U]](
        nodes.map(_.input).mkString,
        if (nodes.forall(_.output.isDefined)) Some ({
            val outputs = nodes.map(_.output.get)
            funcNode match {
                case Left(f) => {
                    val func: ((U, T) => U) = (u, t) => f((u, t))
                    outputs.reduceLeftOption(func)
                }
                case Right(f) => {
                    val func: ((T, U) => U) = (t, u) => f((t, u))
                    outputs.reduceRightOption(func)
                }
            }
        }) else None
    ) {
        // TODO: See how this can be made to work with even 0 (since this is too restrictive)
        require(!nodes.isEmpty, "Reduce option combinators must have at least one outcome")

        override def generate: Parsley[Option[U]] = funcNode match {
            case Left(f) => {
                val func: ((U, T) => U) = (u, t) => f((u, t))
                nodes.head.generate.reduceLeftOption(func)
            }
            case Right(f) => {
                val func: ((T, U) => U) = (t, u) => f((t, u))
                nodes.head.generate.reduceRightOption(func)
            }
        }

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (funcStr, funcCtx) = funcNode match {
                case Left(func) => func.prettyString(outCtx, depth + 2)
                case Right(func) => func.prettyString(outCtx, depth + 2)
            }
            val (nodeHeadStr, nodeHeadCtx) = nodes.head.prettyString(funcCtx, depth + 2)

            (s"""${indent}$reduceName${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- iterations: ${nodes.length}
               |${indentInner}- funcNode:
               |$funcStr
               |${indentInner}- fstNode:
               |$nodeHeadStr""".stripMargin, nodeHeadCtx)
        }
    }

    final case class FoldLeft[T, U: PrettyPrint](nodes: List[ParsleyInternalUnwrapped[T]], initialValue: U, funcNode: ParsleyFunction[(U, T), U]) extends PFold[T, U](nodes, initialValue, Left(funcNode), "foldLeft") {
        override def generate: Parsley[U] = {
            val func: ((U, T) => U) = (u, t) => funcNode((u, t))
            nodes.head.generate.foldLeft(initialValue)(func)
        }

        override def optimize: ParsleyInternalUnwrapped[U] = FoldLeft(nodes.map(_.optimize), initialValue, funcNode)

        override def disableOptimizations: ParsleyInternalUnwrapped[U] = Impure(FoldLeft(nodes.map(_.disableOptimizations), initialValue, funcNode))
    }

    final case class FoldLeft1[T, U: PrettyPrint](nodes: List[ParsleyInternalUnwrapped[T]], initialValue: U, funcNode: ParsleyFunction[(U, T), U]) extends PFold[T, U](nodes, initialValue, Left(funcNode), "foldLeft1") {
        override def generate: Parsley[U] = {
            val func: ((U, T) => U) = (u, t) => funcNode((u, t))
            nodes.head.generate.foldLeft1(initialValue)(func)
        }

        override def optimize: ParsleyInternalUnwrapped[U] = FoldLeft1(nodes.map(_.optimize), initialValue, funcNode)

        override def disableOptimizations: ParsleyInternalUnwrapped[U] = Impure(FoldLeft1(nodes.map(_.disableOptimizations), initialValue, funcNode))
    }

    final case class FoldRight[T, U: PrettyPrint](nodes: List[ParsleyInternalUnwrapped[T]], initialValue: U, funcNode: ParsleyFunction[(T, U), U]) extends PFold[T, U](nodes, initialValue, Right(funcNode), "foldRight") {
        override def generate: Parsley[U] = {
            val func: ((T, U) => U) = (t, u) => funcNode((t, u))
            nodes.head.generate.foldRight(initialValue)(func)
        }

        override def optimize: ParsleyInternalUnwrapped[U] = FoldRight(nodes.map(_.optimize), initialValue, funcNode)

        override def disableOptimizations: ParsleyInternalUnwrapped[U] = Impure(FoldRight(nodes.map(_.disableOptimizations), initialValue, funcNode))
    }

    final case class FoldRight1[T, U: PrettyPrint](nodes: List[ParsleyInternalUnwrapped[T]], initialValue: U, funcNode: ParsleyFunction[(T, U), U]) extends PFold[T, U](nodes, initialValue, Right(funcNode), "foldRight1") {
        override def generate: Parsley[U] = {
            val func: ((T, U) => U) = (t, u) => funcNode((t, u))
            nodes.head.generate.foldRight1(initialValue)(func)
        }

        override def optimize: ParsleyInternalUnwrapped[U] = FoldRight1(nodes.map(_.optimize), initialValue, funcNode)

        override def disableOptimizations: ParsleyInternalUnwrapped[U] = Impure(FoldRight1(nodes.map(_.disableOptimizations), initialValue, funcNode))
    }

    final case class ReduceLeft[T, U >: T: PrettyPrint](nodes: List[ParsleyInternalUnwrapped[T]], funcNode: ParsleyFunction[(U, T), U]) extends PReduce[T, U](nodes, Left(funcNode), "reduceLeft") {
        override def generate: Parsley[U] = {
            val func: ((U, T) => U) = (u, t) => funcNode((u, t))
            nodes.head.generate.reduceLeft(func)
        }

        override def optimize: ParsleyInternalUnwrapped[U] = ReduceLeft(nodes.map(_.optimize), funcNode)

        override def disableOptimizations: ParsleyInternalUnwrapped[U] = Impure(ReduceLeft(nodes.map(_.disableOptimizations), funcNode))
    }

    final case class ReduceRight[T, U >: T: PrettyPrint](nodes: List[ParsleyInternalUnwrapped[T]], funcNode: ParsleyFunction[(T, U), U]) extends PReduce[T, U](nodes, Right(funcNode), "reduceRight") {
        override def generate: Parsley[U] = {
            val func: ((T, U) => U) = (t, u) => funcNode((t, u))
            nodes.head.generate.reduceRight(func)
        }

        override def optimize: ParsleyInternalUnwrapped[U] = ReduceRight(nodes.map(_.optimize), funcNode)

        override def disableOptimizations: ParsleyInternalUnwrapped[U] = Impure(ReduceRight(nodes.map(_.disableOptimizations), funcNode))
    }

    final case class ReduceOptionLeft[T, U >: T: PrettyPrint](nodes: List[ParsleyInternalUnwrapped[T]], funcNode: ParsleyFunction[(U, T), U]) extends PReduceOption[T, U](nodes, Left(funcNode), "reduceLeftOption") {
        override def generate: Parsley[Option[U]] = {
            val func: ((U, T) => U) = (u, t) => funcNode((u, t))
            nodes.head.generate.reduceLeftOption(func)
        }

        override def optimize: ParsleyInternalUnwrapped[Option[U]] = ReduceOptionLeft(nodes.map(_.optimize), funcNode)

        override def disableOptimizations: ParsleyInternalUnwrapped[Option[U]] = Impure(ReduceOptionLeft(nodes.map(_.disableOptimizations), funcNode))
    }

    final case class ReduceOptionRight[T, U >: T: PrettyPrint](nodes: List[ParsleyInternalUnwrapped[T]], funcNode: ParsleyFunction[(T, U), U]) extends PReduceOption[T, U](nodes, Right(funcNode), "reduceRightOption") {
        override def generate: Parsley[Option[U]] = {
            val func: ((T, U) => U) = (t, u) => funcNode((t, u))
            nodes.head.generate.reduceRightOption(func)
        }

        override def optimize: ParsleyInternalUnwrapped[Option[U]] = ReduceOptionRight(nodes.map(_.optimize), funcNode)

        override def disableOptimizations: ParsleyInternalUnwrapped[Option[U]] = Impure(ReduceOptionRight(nodes.map(_.disableOptimizations), funcNode))
    }

    abstract class Separated[T: PrettyPrint](
        nodes: List[(ParsleyInternalUnwrapped[T], ParsleyInternalUnwrapped[?])],
        endingNode: Option[ParsleyInternalUnwrapped[T]],
        sepName: String)
    extends ParsleyInternalUnwrapped[List[T]](
        nodes.map { case (n, s) => n.input + s.input }.mkString + endingNode.map(_.input).getOrElse(""),
        if (nodes.forall { case (n, s) => n.output.isDefined && s.output.isDefined } && endingNode.fold(true)(_.output.isDefined)) Some(nodes.map { case (n, _) => n.output.get } ++ endingNode.map(e => List(e.output.get)).getOrElse(List())) else None
    ) {
        // TODO: See how this can be made to work with even 0 (since this is too restrictive)
        require(!nodes.isEmpty, "Separated combinators must have at least one outcome")

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (nodeHeadStr, nodeHeadCtx) = nodes.head._1.prettyString(outCtx, depth + 2)
            val (sepHeadStr, sepHeadCtx) = nodes.head._2.prettyString(nodeHeadCtx, depth + 2)

            (s"""${indent}$sepName${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- iterations: ${nodes.length + (if (endingNode.isDefined) 1 else 0)}
               |${indentInner}- fstNode:
               |$nodeHeadStr
               |${indentInner}- fstSeparator:
               |$sepHeadStr""".stripMargin, sepHeadCtx)
        }
    }

    final case class EndBy[T: PrettyPrint](nodes: List[(ParsleyInternalUnwrapped[T], ParsleyInternalUnwrapped[?])]) extends Separated[T](nodes, None, "endBy") {
        override def generate: Parsley[List[T]] = {
            val (n, s) = nodes.head
            parsley.combinator.endBy(n.generate, s.generate)
        }

        override def optimize: ParsleyInternalUnwrapped[List[T]] = EndBy(nodes.map { case (p, s) => (p.optimize, s.optimize) })

        override def disableOptimizations: ParsleyInternalUnwrapped[List[T]] = Impure(EndBy(nodes.map { case (p, s) => (p.disableOptimizations, s.disableOptimizations) }))
    }

    final case class EndBy1[T: PrettyPrint](nodes: List[(ParsleyInternalUnwrapped[T], ParsleyInternalUnwrapped[?])]) extends Separated[T](nodes, None, "endBy1") {
        override def generate: Parsley[List[T]] = {
            val (n, s) = nodes.head
            parsley.combinator.endBy1(n.generate, s.generate)
        }

        override def optimize: ParsleyInternalUnwrapped[List[T]] = EndBy1(nodes.map { case (p, s) => (p.optimize, s.optimize) })

        override def disableOptimizations: ParsleyInternalUnwrapped[List[T]] = Impure(EndBy1(nodes.map { case (p, s) => (p.disableOptimizations, s.disableOptimizations) }))
    }

    final case class SepBy[T: PrettyPrint](nodes: List[(ParsleyInternalUnwrapped[T], ParsleyInternalUnwrapped[?])], endingNode: ParsleyInternalUnwrapped[T]) extends Separated[T](nodes, Some(endingNode), "sepBy") {
        override def generate: Parsley[List[T]] = {
            val (n, s) = nodes.head
            parsley.combinator.sepBy(n.generate, s.generate)
        }

        override def optimize: ParsleyInternalUnwrapped[List[T]] = SepBy(nodes.map { case (p, s) => (p.optimize, s.optimize) }, endingNode.optimize)

        override def disableOptimizations: ParsleyInternalUnwrapped[List[T]] = Impure(SepBy(nodes.map { case (p, s) => (p.disableOptimizations, s.disableOptimizations) }, endingNode.disableOptimizations))
    }

    final case class SepBy1[T: PrettyPrint](nodes: List[(ParsleyInternalUnwrapped[T], ParsleyInternalUnwrapped[?])], endingNode: ParsleyInternalUnwrapped[T]) extends Separated[T](nodes, Some(endingNode), "sepBy1") {
        override def generate: Parsley[List[T]] = {
            val (n, s) = nodes.head
            parsley.combinator.sepBy1(n.generate, s.generate)
        }

        override def optimize: ParsleyInternalUnwrapped[List[T]] = SepBy1(nodes.map { case (p, s) => (p.optimize, s.optimize) }, endingNode.optimize)

        override def disableOptimizations: ParsleyInternalUnwrapped[List[T]] = Impure(SepBy1(nodes.map { case (p, s) => (p.disableOptimizations, s.disableOptimizations) }, endingNode.disableOptimizations))
    }

    final case class SepEndBy[T: PrettyPrint](nodes: List[(ParsleyInternalUnwrapped[T], ParsleyInternalUnwrapped[?])], endingNode: Option[ParsleyInternalUnwrapped[T]]) extends Separated[T](nodes, endingNode, "sepEndBy") {
        override def generate: Parsley[List[T]] = {
            val (n, s) = nodes.head
            parsley.combinator.sepEndBy(n.generate, s.generate)
        }

        override def optimize: ParsleyInternalUnwrapped[List[T]] = SepEndBy(nodes.map { case (p, s) => (p.optimize, s.optimize) }, endingNode.map(_.optimize))

        override def disableOptimizations: ParsleyInternalUnwrapped[List[T]] = Impure(SepEndBy(nodes.map { case (p, s) => (p.disableOptimizations, s.disableOptimizations) }, endingNode.map(_.disableOptimizations)))
    }

    final case class SepEndBy1[T: PrettyPrint](nodes: List[(ParsleyInternalUnwrapped[T], ParsleyInternalUnwrapped[?])], endingNode: Option[ParsleyInternalUnwrapped[T]]) extends Separated[T](nodes, endingNode, "sepEndBy1") {
        override def generate: Parsley[List[T]] = {
            val (n, s) = nodes.head
            parsley.combinator.sepEndBy1(n.generate, s.generate)
        }

        override def optimize: ParsleyInternalUnwrapped[List[T]] = SepEndBy1(nodes.map { case (p, s) => (p.optimize, s.optimize) }, endingNode.map(_.optimize))

        override def disableOptimizations: ParsleyInternalUnwrapped[List[T]] = Impure(SepEndBy1(nodes.map { case (p, s) => (p.disableOptimizations, s.disableOptimizations) }, endingNode.map(_.disableOptimizations)))
    }

    abstract class Count(nodes: List[ParsleyInternalUnwrapped[?]], countName: String) extends ParsleyInternalUnwrapped[Int](nodes.map(_.input).mkString, if (nodes.forall(_.output.isDefined)) Some(nodes.length) else None) {
        require(!nodes.isEmpty, "Count combinators must have at least one outcome")

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (nodeHeadStr, nodeHeadCtx) = nodes.head.prettyString(outCtx, depth + 2)

            (s"""${indent}$countName${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- iterations: ${nodes.length}
               |${indentInner}- fstNode:
               |$nodeHeadStr""".stripMargin, nodeHeadCtx)
        }
    }

    final case class CountMany(nodes: List[ParsleyInternalUnwrapped[?]]) extends Count(nodes, "countMany") {
        // TODO: See how this can be made to work with even 0 (since this is too restrictive)
        require(!nodes.isEmpty, "CountMany combinator must have at least one outcome")

        override def generate: Parsley[Int] = parsley.combinator.countMany(nodes.head.generate)

        override def optimize: ParsleyInternalUnwrapped[Int] = CountMany(nodes.map(_.optimize))

        override def disableOptimizations: ParsleyInternalUnwrapped[Int] = Impure(CountMany(nodes.map(_.disableOptimizations)))
    }

    final case class CountSome(nodes: List[ParsleyInternalUnwrapped[?]]) extends Count(nodes, "countSome") {
        require(!nodes.isEmpty, "CountSome combinator must have at least one outcome")

        override def generate: Parsley[Int] = parsley.combinator.countSome(nodes.head.generate)

        override def optimize: ParsleyInternalUnwrapped[Int] = CountSome(nodes.map(_.optimize))

        override def disableOptimizations: ParsleyInternalUnwrapped[Int] = Impure(CountSome(nodes.map(_.disableOptimizations)))
    }

    final case class HomogeneousPrecedence(atoms: List[ParsleyInternalUnwrapped[HomogeneousExpr]], ops: List[(Fixity, List[ParsleyInternalUnwrapped[Any]])], i: String, o: Option[HomogeneousExpr]) extends ParsleyInternalUnwrapped(i, o) {
        require(!atoms.isEmpty, "Must provide at least one atom for homogeneous precedence parsing in unwrapped ADT")
        require(!ops.isEmpty, "Must provide at least one operation for homogeneous precedence parsing in unwrapped ADT")

        def generateOps(binding: Int, fixity: Fixity, ops: List[ParsleyInternalUnwrapped[Any]]): Ops[HomogeneousExpr, HomogeneousExpr] = fixity match {
            case InfixL => Ops(InfixL)(ops.head.generate.as(HomogeneousExpr.LeftBinaryExpr(binding, _, _)), ops.tail.map(_.generate.as(HomogeneousExpr.LeftBinaryExpr(binding, _, _))): _*)
            case InfixN => Ops(InfixN)(ops.head.generate.as(HomogeneousExpr.NonBinaryExpr(binding, _, _)), ops.tail.map(_.generate.as(HomogeneousExpr.NonBinaryExpr(binding, _, _))): _*)
            case InfixR => Ops(InfixR)(ops.head.generate.as(HomogeneousExpr.RightBinaryExpr(binding, _, _)), ops.tail.map(_.generate.as(HomogeneousExpr.RightBinaryExpr(binding, _, _))): _*)
            case Prefix => Ops(Prefix)(ops.head.generate.as(HomogeneousExpr.PrefixUnaryExpr(binding, _)), ops.tail.map(_.generate.as(HomogeneousExpr.PrefixUnaryExpr(binding, _))): _*)
            case Postfix => Ops(Postfix)(ops.head.generate.as(HomogeneousExpr.PostfixUnaryExpr(binding, _)), ops.tail.map(_.generate.as(HomogeneousExpr.PostfixUnaryExpr(binding, _))): _*)
        }

        override def generate: Parsley[HomogeneousExpr] = {
            val (f, ps) = ops.head
            precedence(atoms.head.generate, atoms.tail.map(_.generate): _*)(generateOps(1, f, ps), ops.tail.zipWithIndex.map { case ((f, ps), i) => generateOps(i + 2, f, ps) }: _*)
        }

        override def prettyString(ctx: List[String], depth: Int): (String, List[String]) = {
            val indent = Indent * depth
            val indentInner = Indent * (depth + 1)
            val (parsleyOutput, actualOutput, actualOutputCtx) = prettyPrintParsleyOutput(ctx)
            val (outStr, outCtx) = output.fold(("N/A", actualOutputCtx))(_.prettyPrint(actualOutputCtx))
            val (atomsStr, atomsCtx) = atoms.prettyPrint(outCtx, depth + 1)
            val (opsStr, opsCtx) = ops.prettyPrint(atomsCtx, depth + 2)

            (s"""${indent}homogeneous${if (output != parsleyOutput) " (deviates)" else ""}
               |${indentInner}- input: "$input"
               |${indentInner}- output: $outStr
               |${indentInner}- parsleyOutput: $actualOutput
               |${indentInner}- atoms:
               |$atomsStr
               |${indentInner}- ops:
               |$opsStr""".stripMargin, opsCtx)
        }

        override def optimize: ParsleyInternalUnwrapped[HomogeneousExpr] = HomogeneousPrecedence(atoms.map(_.optimize), ops.map { case (f, p) => (f, p.map(_.optimize)) }, i, o)

        override def disableOptimizations: ParsleyInternalUnwrapped[HomogeneousExpr] = Impure(HomogeneousPrecedence(atoms.map(_.disableOptimizations), ops.map { case (f, p) => (f, p.map(_.disableOptimizations)) }, i, o))
    }
}
