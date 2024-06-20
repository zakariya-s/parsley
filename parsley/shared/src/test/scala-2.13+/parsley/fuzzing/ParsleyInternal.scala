package parsley.fuzzing

import parsley.expr.{Fixity, InfixL, InfixN, InfixR, Postfix, Prefix}

import org.scalacheck.Gen
import org.scalacheck.Shrink

import scala.annotation.nowarn

// First-stage ADT
sealed abstract class ParsleyInternal[+T: PrettyPrint] {
    def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[T]]]

    // Generates an invalid input/output for the provided parser
    def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[T]], Boolean)]

    final def generate: Gen[ParsleyInternalUnwrapped[T]] = generate(1).map(_.head)

    final def generateInvalid(lastFailed: Boolean): Gen[ParsleyInternalUnwrapped[T]] = generateInvalid(1, lastFailed).suchThat(_._2).map(_._1.headOption.getOrElse(throw new Exception("Expected one invalid parser but got none")))

    // Use Stream even though it is deprecated due to ScalaCheck requiring it
    @nowarn("cat=deprecation") def shrink: Stream[ParsleyInternal[T]]

    @nowarn("cat=deprecation") def shrinkAny: Stream[ParsleyInternal[Any]]

    // (satisfied so far, has consumed input?)
    def satisfies(input: String): (Option[T], String)

    // Returns either a list of possible characters that can follow, or the value
    // of the expression if no input is required (i.e. stateless/inputless)
    // This may also be an over-approximation, which is okay
    def possibleNext: Either[List[Char], T]

    def correct(forbidden: Set[Char]): Either[Unit, Set[Char]]

    def correctInvalid(lastFailed: Boolean): Boolean

    // Returns true if the given input satisfies this parser, or if input is consumed
    final def valid(input: String): Boolean = {
        val (satisfy, str) = satisfies(input)
        // Catch the empty string case too
        satisfies("")._1.isDefined || satisfy.isDefined || input != str
    }

    // FIXME?: I cannot get `ParsleyInternal[T]`, which is covariant in its type parameter T to play
    // nicely with `PrettyPrint[T]` which is invariant in T (or could be contravariant
    // as is the case for `Show` in the `cats` library, but never covariant due to the
    // presence of the type parameter in the arguments of the virtual method)
    // and I need to use it for `shrinkAny` so that there is a known `PrettyPrint` instance,
    // yet because `shrinkAny` returns a `ParsleyInternal[Any]` I cannot retrieve a `PrettyPrint[Any]`
    // safely, even though I know that it will in fact not be used for `Any` but for the original type
    // parameter. I have given up trying to resolve this, so `asInstanceOf` will do.
    final def evPrettyPrint: PrettyPrint[Any] = implicitly[PrettyPrint[T]].asInstanceOf[PrettyPrint[Any]]
}

// @nowarn("cat=deprecation")
object ParsleyInternal {
    // Shrink instance for parser with the guarantee that the parsers should be
    // semantically valid upon shrinking
    implicit def shrinkParsleyInternal: Shrink[ParsleyInternal[Any]] = Shrink[ParsleyInternal[Any]](_.shrinkAny).suchThat(_.correct(Set()).isRight)

    implicit def prettyPrintParsleyInternal[T]: PrettyPrint[ParsleyInternal[T]] = new PrettyPrint[ParsleyInternal[T]] {
        override def prettyPrint(obj: ParsleyInternal[T], ctx: List[String], depth: Int): (String, List[String]) = (obj.toString, ctx)
    }

    final case class Empty() extends ParsleyInternal[Nothing] {
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[Nothing]]] = for {
            internal <- Gen.listOfN(n, Gen.const(ParsleyInternalUnwrapped.Empty()))
        } yield internal

        def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[Nothing]], Boolean)] = generate(n).map((_, true))
        
        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[Nothing]] = Stream.empty

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = shrink
        
        override def satisfies(input: String): (Option[Nothing], String) = (None, input)
        
        override def possibleNext: Either[List[Char], Nothing] = Left(List())

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = Right(forbidden)

        override def correctInvalid(lastFailed: Boolean): Boolean = true
    }

    final case class PChar(c: Char) extends ParsleyInternal[Char] {
        require(c != 0, "Cannot parse NUL-character from the input")
        
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[Char]]] = for {
            internal <- Gen.listOfN(n, Gen.const(new ParsleyInternalUnwrapped.PChar(c)))
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[Char]], Boolean)] = Gen.oneOf(
            Gen.listOfN(n, Gen.asciiPrintableChar.suchThat(_ != c).map(genC => ParsleyInternalUnwrapped.PChar(c, genC.toString))).map((_, true)),
            generate(n).map((_, lastFailed))
        )
        
        // FIXME: This is preventing shrinking -- ensure only printable ASCII
        // characters  are used
        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[Char]] = Stream.empty

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = shrink
        
        override def satisfies(input: String): (Option[Char], String) = if (input.startsWith(c.toString)) (Some(c), input.substring(1)) else (None, input)
        
        override def possibleNext: Either[List[Char], Char] = Left(List(c))

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = if (forbidden.contains(c)) Left(()) else Right(Set())

        override def correctInvalid(lastFailed: Boolean): Boolean = true
    }

    final case class PString(s: String) extends ParsleyInternal[String] {
        require(!s.isEmpty, "Cannot parse an empty string from the input")
        
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[String]]] = for {
            internal <- Gen.listOfN(n, Gen.const(new ParsleyInternalUnwrapped.PString(s)))
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[String]], Boolean)] = Gen.oneOf(
            Gen.listOfN(n, Gen.asciiPrintableStr.suchThat(_ != s).map(genS => ParsleyInternalUnwrapped.PString(s, genS))).map((_, true)),
            generate(n).map((_, lastFailed))
        )
        
        //override def shrink: Stream[ParsleyInternal[String]] = for (shrinkStr <- Shrink.shrink(s).filter(!_.isEmpty)) yield PString(shrinkStr)
        // FIXME: This is preventing shrinking -- ensure String cannot be empty
        // and only uses printable ASCII characters
        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[String]] = Stream.empty

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = shrink
        
        override def satisfies(input: String): (Option[String], String) = if (input.startsWith(s.toString)) (Some(s), input.substring(s.length)) else (None, input.substring(commonPrefixLength(s, input)))
        
        override def possibleNext: Either[List[Char], String] = Left(List(s(0)))

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = if (forbidden.contains(s(0))) Left(()) else Right(Set())

        override def correctInvalid(lastFailed: Boolean): Boolean = true

        // Source: https://stackoverflow.com/questions/8104479/how-to-find-the-longest-common-prefix-of-two-strings-in-scala
        private def commonPrefixLength(a: String, b: String): Int = a.zip(b).takeWhile(Function.tupled(_ == _)).map(_._1).length
    }
    
    final case class Pure[T: PrettyPrint](value: T) extends ParsleyInternal[T] {
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[T]]] = for {
            internal <- Gen.listOfN(n, Gen.const(ParsleyInternalUnwrapped.Pure(value)))
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[T]], Boolean)] = generate(n).map((_, lastFailed))
        
        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[T]] = for (shrinkVal <- Shrink.shrink(value)) yield Pure(shrinkVal)

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = shrink
        
        override def satisfies(input: String): (Option[T], String) = (Some(value), input)
        
        override def possibleNext: Either[List[Char], T] = Right(value)

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = Right(forbidden)

        override def correctInvalid(lastFailed: Boolean): Boolean = lastFailed
    }

    final case class Impure[T: PrettyPrint](node: ParsleyInternal[T]) extends ParsleyInternal[T] {
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[T]]] = node.generate(n).map(_.map(ParsleyInternalUnwrapped.Impure(_)))

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[T]], Boolean)] = node.generateInvalid(n, lastFailed).map { case (p, f) => (p.map(ParsleyInternalUnwrapped.Impure(_)), f) }
        
        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[T]] = {
            val s1 = for (shrinkNode <- node.shrink) yield Impure(shrinkNode)
            val s2 = node.shrink

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = for (shrinkNode <- node.shrinkAny) yield Impure(shrinkNode)(shrinkNode.evPrettyPrint)
            val s2 = node.shrinkAny
            
            shrink ++ s1 ++ s2
        }
        
        override def satisfies(input: String): (Option[T], String) = node.satisfies(input)
        
        override def possibleNext: Either[List[Char], T] = node.possibleNext

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = node.correct(forbidden)

        override def correctInvalid(lastFailed: Boolean): Boolean = node.correctInvalid(lastFailed)
    }

    final case class Atomic[T: PrettyPrint](node: ParsleyInternal[T]) extends ParsleyInternal[T] {
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[T]]] = node.generate(n).map(_.map(ParsleyInternalUnwrapped.Atomic(_)))

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[T]], Boolean)] = node.generateInvalid(n, lastFailed).map { case (p, f) => (p.map(ParsleyInternalUnwrapped.Atomic(_)), f) }
        
        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[T]] = {
            val s1 = for (shrinkNode <- node.shrink) yield Atomic(shrinkNode)
            val s2 = node.shrink

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = for (shrinkNode <- node.shrinkAny) yield Atomic(shrinkNode)(shrinkNode.evPrettyPrint)
            val s2 = node.shrinkAny
            
            shrink ++ s1 ++ s2
        }
        
        override def satisfies(input: String): (Option[T], String) = node.satisfies(input) match {
            case x @ (Some(_), _) => x
            case (None, _) => (None, input)
        }
        
        override def possibleNext: Either[List[Char], T] = node.possibleNext

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = node.correct(forbidden)

        override def correctInvalid(lastFailed: Boolean): Boolean = node.correctInvalid(lastFailed)
    }

    final case class LookAhead[T: PrettyPrint](node: ParsleyInternal[T]) extends ParsleyInternal[T] {
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[T]]] = node.generate(n).map(_.map(ParsleyInternalUnwrapped.LookAhead(_)))

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[T]], Boolean)] = node.generateInvalid(n, lastFailed).map { case (p, f) => (p.map(ParsleyInternalUnwrapped.LookAhead(_)), f) }
        
        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[T]] = {
            val s1 = for (shrinkNode <- node.shrink) yield LookAhead(shrinkNode)
            val s2 = node.shrink

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = for (shrinkNode <- node.shrinkAny) yield LookAhead(shrinkNode)(shrinkNode.evPrettyPrint)
            val s2 = node.shrinkAny

            shrink ++ s1 ++ s2
        }
        
        override def satisfies(input: String): (Option[T], String) = node.satisfies(input) match {
            case (Some(value), _) => (Some(value), input)
            case x => x
        }
        
        override def possibleNext: Either[List[Char], T] = node.possibleNext

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = node.correct(forbidden)

        // TODO: Change lookAhead to be failable
        override def correctInvalid(lastFailed: Boolean): Boolean = node.correctInvalid(lastFailed)
    }

    final case class NotFollowedBy[T](node: ParsleyInternal[T]) extends ParsleyInternal[Unit] {
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[Unit]]] = node.generate(n).map(_.map(new ParsleyInternalUnwrapped.NotFollowedBy(_)))

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[Unit]], Boolean)] = Gen.oneOf(
            // It is indeed generate and not generateInvalid as notFollowedBy will fail if it exactly matches
            node.generate(n).map(_.map(genNode => ParsleyInternalUnwrapped.NotFollowedBy(genNode, genNode.input))).map((_, lastFailed)),
            generate(n).map((_, lastFailed))
        )
        
        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[Unit]] = {
            val s1 = for (shrinkNode <- node.shrinkAny) yield NotFollowedBy(shrinkNode)
            val s2 = Stream(Pure(()))

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = shrink
        
        override def satisfies(input: String): (Option[Unit], String) = node.satisfies(input) match {
            case (Some(_), _) => (None, input)
            case (None, _) => (Some(()), input)
        }
        
        // TODO: Think about this...
        //override def possibleNext: Either[List[Char], Unit] = node.possibleNext.map(Function.const(()))
        override def possibleNext: Either[List[Char], Unit] = Right(())

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = node.correct(forbidden).map(Function.const(forbidden ++ node.possibleNext.swap.getOrElse(List())))

        override def correctInvalid(lastFailed: Boolean): Boolean = lastFailed
    }

    final case class POptional[T](node: ParsleyInternal[T]) extends ParsleyInternal[Unit] {
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[Unit]]] = node.generate(n).map(_.map(ParsleyInternalUnwrapped.POptional(_, true)))

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[Unit]], Boolean)] = node.generateInvalid(n, lastFailed).map { case (p, f) => (p.map(ParsleyInternalUnwrapped.POptional(_, true)), f) }
        
        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[Unit]] = {
            val s1 = for (shrinkNode <- node.shrinkAny) yield POptional(shrinkNode)
            val s2 = Stream(Pure(()))

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = node.shrinkAny
            
            shrink ++ s1
        }
        
        override def satisfies(input: String): (Option[Unit], String) = node.satisfies(input) match {
            case (Some(_), str) => (Some(()), str)
            case (None, str) if input == str => (Some(()), str)
            case (None, str) => (None, str)
        }
        
        override def possibleNext: Either[List[Char], Unit] = node.possibleNext.map(Function.const(()))

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = node.correct(forbidden).map(forbidden ++ node.possibleNext.swap.getOrElse(List()) ++ _)

        override def correctInvalid(lastFailed: Boolean): Boolean = node.correctInvalid(lastFailed)
    }

    final case class POption[T: PrettyPrint](node: ParsleyInternal[T]) extends ParsleyInternal[Option[T]] {
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[Option[T]]]] = node.generate(n).map(_.map(ParsleyInternalUnwrapped.POption(_, true)))

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[Option[T]]], Boolean)] = node.generateInvalid(n, lastFailed).map { case (p, f) => (p.map(ParsleyInternalUnwrapped.POption(_, true)), f) }
        
        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[Option[T]]] = {
            val s1 = for (shrinkNode <- node.shrink) yield POption(shrinkNode)
            val s2 = Stream(Pure(Option.empty[T]))

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = node.shrinkAny
            
            shrink ++ s1
        }
        
        override def satisfies(input: String): (Option[Option[T]], String) = node.satisfies(input) match {
            case (Some(x), str) => (Some(Some(x)), str)
            case (None, str) if input == str => (Some(None), str)
            case (None, str) => (None, str)
        }
        
        override def possibleNext: Either[List[Char], Option[T]] = node.possibleNext.map(Some(_))

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = node.correct(forbidden).map(forbidden ++ node.possibleNext.swap.getOrElse(List()) ++ _)

        override def correctInvalid(lastFailed: Boolean): Boolean = node.correctInvalid(lastFailed)
    }

    // `~>` / `*>`
    final case class Then[T: PrettyPrint, U](left: ParsleyInternal[U], right: ParsleyInternal[T]) extends ParsleyInternal[T] {
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[T]]] = for {
            leftInternals <- left.generate(n)
            rightInternals <- right.generate(n)
            internal = leftInternals.zip(rightInternals).map { case (left, right) => ParsleyInternalUnwrapped.Then(left, right) }
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[T]], Boolean)] = for {
            (leftInternals, leftFailed) <- left.generateInvalid(n, lastFailed)
            (rightInternals, rightFailed) <- right.generateInvalid(n, leftFailed)
            internal = leftInternals.zip(rightInternals).map { case (left, right) => ParsleyInternalUnwrapped.Then(left, right) }
        } yield (internal, rightFailed)
        
        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[T]] = {
            val s1 = for (shrinkLeft <- left.shrinkAny) yield Then(shrinkLeft, right)
            val s2 = for (shrinkRight <- right.shrink) yield Then(left, shrinkRight)
            val s3 = right.shrink

            s1 ++ s2 ++ s3
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = for (shrinkRight <- right.shrinkAny) yield Then(left, shrinkRight)(shrinkRight.evPrettyPrint)
            val s2 = right.shrinkAny
            val s3 = left.shrinkAny
            
            shrink ++ s1 ++ s2 ++ s3
        }
        
        override def satisfies(input: String): (Option[T], String) = {
            val (value, str) = left.satisfies(input)
            if (value.isDefined) right.satisfies(str) else (None, str)
        }
        
        // No Haskell monadic `>>` operator :(
        override def possibleNext: Either[List[Char], T] = left.possibleNext.flatMap(Function.const(right.possibleNext))

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = for {
            lf <- left.correct(forbidden)
            rf <- right.correct(lf)
        } yield rf

        override def correctInvalid(lastFailed: Boolean): Boolean = right.correctInvalid(left.correctInvalid(lastFailed))
    }

    // `<~` / `<*`
    final case class ThenDiscard[T: PrettyPrint, U](left: ParsleyInternal[T], right: ParsleyInternal[U]) extends ParsleyInternal[T] {
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[T]]] = for {
            leftInternals <- left.generate(n)
            rightInternals <- right.generate(n)
            internal = leftInternals.zip(rightInternals).map { case (left, right) => ParsleyInternalUnwrapped.ThenDiscard(left, right) }
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[T]], Boolean)] = for {
            (leftInternals, leftFailed) <- left.generateInvalid(n, lastFailed)
            (rightInternals, rightFailed) <- right.generateInvalid(n, leftFailed)
            internal = leftInternals.zip(rightInternals).map { case (left, right) => ParsleyInternalUnwrapped.ThenDiscard(left, right) }
        } yield (internal, rightFailed)
        
        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[T]] = {
            val s1 = for (shrinkLeft <- left.shrink) yield ThenDiscard(shrinkLeft, right)
            val s2 = for (shrinkRight <- right.shrinkAny) yield ThenDiscard(left, shrinkRight)
            val s3 = left.shrink

            s1 ++ s2 ++ s3
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = for (shrinkRight <- right.shrinkAny) yield Then(left, shrinkRight)(shrinkRight.evPrettyPrint)
            val s2 = left.shrinkAny
            val s3 = right.shrinkAny
            
            shrink ++ s1 ++ s2 ++ s3
        }
        
        override def satisfies(input: String): (Option[T], String) = {
            val (value, str) = left.satisfies(input)
            if (value.isDefined) (value, right.satisfies(str)._2) else (None, str)
        }
        
        override def possibleNext: Either[List[Char], T] = for {
            x <- left.possibleNext
            _ <- right.possibleNext
        } yield x

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = for {
            lf <- left.correct(forbidden)
            rf <- right.correct(lf)
        } yield rf

        override def correctInvalid(lastFailed: Boolean): Boolean = right.correctInvalid(left.correctInvalid(lastFailed))
    }

    // `<|>`, `|`
    final case class Or[T: PrettyPrint](left: ParsleyInternal[T], right: ParsleyInternal[T]) extends ParsleyInternal[T] {
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[T]]] = for {
            leftInternals <- left.generate(n)
            rightInternals <- right.generate(n)
            internal <- Gen.sequence[List[ParsleyInternalUnwrapped[T]], ParsleyInternalUnwrapped[T]](leftInternals.zip(rightInternals).map { case (leftInternal, rightInternal) =>
                (if (left.valid(rightInternal.input)) Gen.const(true) else Gen.oneOf(true, false)).map(ParsleyInternalUnwrapped.Or(leftInternal, rightInternal, _))
            })
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[T]], Boolean)] = for {
            (leftInternals, leftFailed) <- left.generateInvalid(n, lastFailed)
            (rightInternals, rightFailed) <- right.generateInvalid(n, lastFailed)
            internal <- Gen.sequence[List[ParsleyInternalUnwrapped[T]], ParsleyInternalUnwrapped[T]](leftInternals.zip(rightInternals).map { case (leftInternal, rightInternal) =>
                Gen.oneOf(true, false).map(ParsleyInternalUnwrapped.Or(leftInternal, rightInternal, _))
            })
        } yield (internal, leftFailed && rightFailed)
        
        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[T]] = {
            val s1 = for (shrinkLeft <- left.shrink) yield Or(shrinkLeft, right)
            val s2 = for (shrinkRight <- right.shrink) yield Or(left, shrinkRight)
            val s3 = left.shrink
            val s4 = right.shrink

            s1 ++ s2 ++ s3 ++ s4
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = left.shrinkAny
            val s2 = right.shrinkAny
            
            shrink ++ s1 ++ s2
        }
        
        override def satisfies(input: String): (Option[T], String) = {
            val x @ (value, str) = left.satisfies(input)
            if (value.isDefined) x else if (input == str) right.satisfies(input) else (None, str)
        }
        
        override def possibleNext: Either[List[Char], T] = left.possibleNext.left.flatMap(list => right.possibleNext.left.map(list ++ _))

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = left.correct(forbidden).flatMap(list => right.correct(forbidden).map(list ++ _ ++ left.possibleNext.swap.getOrElse(List())))

        override def correctInvalid(lastFailed: Boolean): Boolean = left.correctInvalid(lastFailed) && right.correctInvalid(lastFailed)
    }

    // `<+>`
    final case class Sum[T: PrettyPrint, U: PrettyPrint](left: ParsleyInternal[T], right: ParsleyInternal[U]) extends ParsleyInternal[Either[T, U]] {
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[Either[T, U]]]] = for {
            leftInternals <- left.generate(n)
            rightInternals <- right.generate(n)
            internal <- Gen.sequence[List[ParsleyInternalUnwrapped[Either[T, U]]], ParsleyInternalUnwrapped[Either[T, U]]](leftInternals.zip(rightInternals).map { case (leftInternal, rightInternal) =>
                (if (left.valid(rightInternal.input)) Gen.const(true) else Gen.oneOf(true, false)).map(ParsleyInternalUnwrapped.Sum(leftInternal, rightInternal, _))
            })
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[Either[T, U]]], Boolean)] = for {
            (leftInternals, leftFailed) <- left.generateInvalid(n, lastFailed)
            (rightInternals, rightFailed) <- right.generateInvalid(n, lastFailed)
            internal <- Gen.sequence[List[ParsleyInternalUnwrapped[Either[T, U]]], ParsleyInternalUnwrapped[Either[T, U]]](leftInternals.zip(rightInternals).map { case (leftInternal, rightInternal) =>
                Gen.oneOf(true, false).map(ParsleyInternalUnwrapped.Sum(leftInternal, rightInternal, _))
            })
        } yield (internal, leftFailed && rightFailed)
        
        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[Either[T, U]]] = {
            val s1 = for (shrinkLeft <- left.shrink) yield Sum(shrinkLeft, right)
            val s2 = for (shrinkRight <- right.shrink) yield Sum(left, shrinkRight)

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = for (shrinkLeft <- left.shrinkAny) yield Sum(shrinkLeft, right)(shrinkLeft.evPrettyPrint, PrettyPrint[U])
            val s2 = for (shrinkRight <- right.shrinkAny) yield Sum(left, shrinkRight)(PrettyPrint[T], shrinkRight.evPrettyPrint)
            val s3 = left.shrinkAny
            val s4 = right.shrinkAny
            
            shrink ++ s1 ++ s2 ++ s3 ++ s4
        }
        
        override def satisfies(input: String): (Option[Either[T, U]], String) = {
            val (value, str) = left.satisfies(input)
            if (value.isDefined) (value.map(Left(_)), str)
            else if (input == str) {
                val (rightValue, rightStr) = right.satisfies(input)
                (rightValue.map(Right(_)), rightStr)
            }
            else (None, str)
        }
        
        override def possibleNext: Either[List[Char], Either[T, U]] = left.possibleNext.map(Left(_)).left.flatMap(list => right.possibleNext.map(Right(_)).left.map(list ++ _))

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = left.correct(forbidden).flatMap(list => right.correct(forbidden).map(list ++ _ ++ left.possibleNext.swap.getOrElse(List())))

        override def correctInvalid(lastFailed: Boolean): Boolean = left.correctInvalid(lastFailed) && right.correctInvalid(lastFailed)
    }

    final case class IfS[T: PrettyPrint](condP: ParsleyInternal[Boolean], thenP: ParsleyInternal[T], elseP: ParsleyInternal[T]) extends ParsleyInternal[T] {
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[T]]] = for {
            condInternals <- condP.generate(n)
            thenInternals <- thenP.generate(n)
            elseInternals <- elseP.generate(n)
            internal = condInternals.zip(thenInternals.zip(elseInternals)).map { case (condI, (thenI, elseI)) => ParsleyInternalUnwrapped.IfS(condI, thenI, elseI) }
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[T]], Boolean)] = for {
            (condInternals, condFailed) <- condP.generateInvalid(n, lastFailed)
            (thenInternals, thenFailed) <- thenP.generateInvalid(n, condFailed)
            (elseInternals, elseFailed) <- elseP.generateInvalid(n, condFailed)
            internal = condInternals.zip(thenInternals.zip(elseInternals)).map { case (condI, (thenI, elseI)) => ParsleyInternalUnwrapped.IfS(condI, thenI, elseI) }
        } yield (internal, thenFailed && elseFailed)
        
        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[T]] = {
            val s1 = for (shrinkCond <- condP.shrink) yield IfS(shrinkCond, thenP, elseP)
            val s2 = for (shrinkThen <- thenP.shrink) yield IfS(condP, shrinkThen, elseP)
            val s3 = for (shrinkElse <- elseP.shrink) yield IfS(condP, thenP, shrinkElse)
            val s4 = thenP.shrink
            val s5 = elseP.shrink

            s1 ++ s2 ++ s3 ++ s4 ++ s5
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = condP.shrinkAny
            
            shrink ++ s1
        }
        
        override def satisfies(input: String): (Option[T], String) = condP.satisfies(input) match {
            case (Some(true), str) => thenP.satisfies(str)
            case (Some(false), str) => elseP.satisfies(str)
            case (None, str) => (None, str)
        }
        
        override def possibleNext: Either[List[Char], T] = condP.possibleNext.flatMap(if (_) thenP.possibleNext else elseP.possibleNext)

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = for {
            cf <- condP.correct(forbidden)
            tf <- thenP.correct(cf)
            ef <- elseP.correct(cf)
        } yield tf ++ ef

        override def correctInvalid(lastFailed: Boolean): Boolean = {
            val condLastFailed = condP.correctInvalid(lastFailed)
            thenP.correctInvalid(condLastFailed) && elseP.correctInvalid(condLastFailed)
        }
    }

    final case class Map[T, U: PrettyPrint](node: ParsleyInternal[T], f: ParsleyFunction[T, U]) extends ParsleyInternal[U] {
        // TODO: This code cannot be salvaged currently since we need to know the function `f` beforehand
        // override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[U]]] = for {
        //     nodeInternal <- node.generate(n)
        //     outputs = nodeInternal.map(_.output)
        //     values <- Gen.listOfN(n, Arbitrary.arbitrary[U])
        //     mapping = outputs.zip(values).toMap
        //     default <- Arbitrary.arbitrary[U]
        //     // Create the mapping function dynamically using the known/expected outputs of `node`
        //     f = UnwrappedFunction(mapping, default)
        //     internal = nodeInternal.map(ParsleyInternalUnwrapped.Map(_, f))
        // } yield internal

        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[U]]] = for {
            nodeInternal <- node.generate(n)
            internal = nodeInternal.map(ParsleyInternalUnwrapped.Map(_, f))
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[U]], Boolean)] = for {
            (nodeInternal, nodeFailed) <- node.generateInvalid(n, lastFailed)
            internal = nodeInternal.map(ParsleyInternalUnwrapped.Map(_, f))
        } yield (internal, nodeFailed)
        
        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[U]] = for (shrinkNode <- node.shrink) yield Map(shrinkNode, f)

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = node.shrinkAny
            
            shrink ++ s1
        }

        override def satisfies(input: String): (Option[U], String) = node.satisfies(input) match {
            case (Some(value), str) => (Some(f(value)), str)
            case (None, str) => (None, str)
        }

        override def possibleNext: Either[List[Char], U] = node.possibleNext.map(f)

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = node.correct(forbidden)

        override def correctInvalid(lastFailed: Boolean): Boolean = node.correctInvalid(lastFailed)
    }

    abstract class Repeat[T: PrettyPrint](node: ParsleyInternal[T], atLeastOne: Boolean) extends ParsleyInternal[List[T]] {
        // TODO: Should not be asserted if intended to fail
        // require(node.possibleNext.isLeft, "Repeat combinators require input")

        def generateNode(internalNodes: List[ParsleyInternalUnwrapped[T]]): ParsleyInternalUnwrapped[List[T]]

        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[List[T]]]] = for {
            rand <- Gen.choose(1, 25)
            manyInternals <- node.generate(rand)
            internal <- Gen.listOfN(n, Gen.atLeastOne(manyInternals).map(_.toList).map(generateNode(_)))
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[List[T]]], Boolean)] = for {
            rand <- Gen.choose(1, 25)
            (manyInternals, manyFailed) <- node.generateInvalid(rand, lastFailed)
            internal <- Gen.listOfN(n, Gen.atLeastOne(manyInternals).map(_.toList).map(generateNode(_)))
        } yield (internal, manyFailed)

        override def satisfies(input: String): (Option[List[T]], String) = {
            var (l, i): (List[T], String) = if (atLeastOne) {
                val (value, str) = node.satisfies(input)
                value match {
                    case Some(value) => (List(value), str)
                    case None => return (None, str)
                }
            } else (List(), input)
            var cont = true

            while (cont) {
                val (value, str) = node.satisfies(i)
                value match {
                    case Some(value) => l = l :+ value
                    case None if i == str => cont = false
                    case None => return (None, str)
                }
                i = str
            }

            (Some(l), i)
        }

        // TODO: This could either result in a pure value (no iterations) or not
        // Switch to Ior from cats to enable zero iterations
        override def possibleNext: Either[List[Char], List[T]] = Left(node.possibleNext.swap.getOrElse(List()))

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] =
            if (node.possibleNext.isLeft) node.correct(forbidden).map((if (atLeastOne) Set() else forbidden) ++ _ ++ node.possibleNext.swap.getOrElse(List()))
            else Left(())

        override def correctInvalid(lastFailed: Boolean): Boolean = node.correctInvalid(lastFailed)
    }

    final case class Many[T: PrettyPrint](node: ParsleyInternal[T]) extends Repeat[T](node, false) {
        override def generateNode(internalNodes: List[ParsleyInternalUnwrapped[T]]): ParsleyInternalUnwrapped[List[T]] = ParsleyInternalUnwrapped.Many(internalNodes)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[List[T]]] = for (shrinkNode <- node.shrink) yield Many(shrinkNode)

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = node.shrinkAny
            
            shrink ++ s1
        }
    }

    final case class PSome[T: PrettyPrint](node: ParsleyInternal[T]) extends Repeat[T](node, true) {
        override def generateNode(internalNodes: List[ParsleyInternalUnwrapped[T]]): ParsleyInternalUnwrapped[List[T]] = ParsleyInternalUnwrapped.PSome(internalNodes)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[List[T]]] = for (shrinkNode <- node.shrink) yield PSome(shrinkNode)

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = node.shrinkAny
            
            shrink ++ s1
        }
    }

    final case class Filter[T: PrettyPrint](node: ParsleyInternal[T]) extends ParsleyInternal[T] {
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[T]]] = for {
            nodeInternal <- node.generate(n)
            // Collect the possible outputs to filter on - this should NEVER fail
            outputs = nodeInternal.map(_.output).collect { case Some(x) => x }.toSet
            internal = nodeInternal.map(ParsleyInternalUnwrapped.Filter(_, outputs))
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[T]], Boolean)] = for {
            (nodeInternal, nodeFailed) <- node.generateInvalid(n, lastFailed)
            // If the node has failed, there is no guarantee on the sanity of the output
            // type, i.e. Nothing if an empty combinator was produced
            outputs = if (nodeFailed) Set[T]() else nodeInternal.map(_.output.getOrElse(throw new Exception("Filter should get all outputs but did not"))).toSet
            internal = nodeInternal.map(ParsleyInternalUnwrapped.Filter(_, outputs))
        } yield (internal, nodeFailed)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[T]] = {
            val s1 = for (shrinkNode <- node.shrink) yield Filter(shrinkNode)
            val s2 = node.shrink

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = for (shrinkNode <- node.shrinkAny) yield Filter(shrinkNode)(shrinkNode.evPrettyPrint)
            val s2 = node.shrinkAny
            
            shrink ++ s1 ++ s2
        }

        override def satisfies(input: String): (Option[T], String) = node.satisfies(input)

        override def possibleNext: Either[List[Char], T] = node.possibleNext

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = node.correct(forbidden)

        override def correctInvalid(lastFailed: Boolean): Boolean = node.correctInvalid(lastFailed)
    }

    // <*>
    final case class Ap[T: PrettyPrint, U: PrettyPrint](funcNode: ParsleyInternal[ParsleyFunction[T, U]], argNode: ParsleyInternal[T]) extends ParsleyInternal[U] {
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[U]]] = for {
            funcInternals <- funcNode.generate(n)
            argInternals <- argNode.generate(n)
            internal = funcInternals.zip(argInternals).map { case (funcI, argI) => ParsleyInternalUnwrapped.Ap(funcI, argI) }
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[U]], Boolean)] = for {
            (funcInternals, funcFailed) <- funcNode.generateInvalid(n, lastFailed)
            (argInternals, argFailed) <- argNode.generateInvalid(n, funcFailed)
            internal = funcInternals.zip(argInternals).map { case (funcI, argI) => ParsleyInternalUnwrapped.Ap(funcI, argI) }
        } yield (internal, argFailed)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[U]] = {
            val s1 = for (shrinkFuncNode <- funcNode.shrink) yield Ap(shrinkFuncNode, argNode)
            val s2 = for (shrinkArgNode <- argNode.shrink) yield Ap(funcNode, shrinkArgNode)

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = funcNode.shrinkAny
            val s2 = argNode.shrinkAny
            
            shrink ++ s1 ++ s2
        }

        override def satisfies(input: String): (Option[U], String) = funcNode.satisfies(input) match {
            case (Some(func), funcStr) => argNode.satisfies(funcStr) match {
                case (Some(arg), argStr) => (Some(func(arg)), argStr)
                case (None, argStr) => (None, argStr)
            }
            case (None, funcStr) => (None, funcStr)
        }

        override def possibleNext: Either[List[Char], U] = for {
            func <- funcNode.possibleNext
            arg <- argNode.possibleNext
        } yield func(arg)

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = for {
            ff <- funcNode.correct(forbidden)
            af <- argNode.correct(ff)
        } yield af

        override def correctInvalid(lastFailed: Boolean): Boolean = argNode.correctInvalid(funcNode.correctInvalid(lastFailed))
    }

    // <**>
    final case class ReverseAp[T: PrettyPrint, U: PrettyPrint](argNode: ParsleyInternal[T], funcNode: ParsleyInternal[ParsleyFunction[T, U]]) extends ParsleyInternal[U] {
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[U]]] = for {
            argInternals <- argNode.generate(n)
            funcInternals <- funcNode.generate(n)
            internal = argInternals.zip(funcInternals).map { case (argI, funcI) => ParsleyInternalUnwrapped.ReverseAp(argI, funcI) }
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[U]], Boolean)] = for {
            (argInternals, argFailed) <- argNode.generateInvalid(n, lastFailed)
            (funcInternals, funcFailed) <- funcNode.generateInvalid(n, argFailed)
            internal = argInternals.zip(funcInternals).map { case (argI, funcI) => ParsleyInternalUnwrapped.ReverseAp(argI, funcI) }
        } yield (internal, funcFailed)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[U]] = {
            val s1 = for (shrinkArgNode <- argNode.shrink) yield ReverseAp(shrinkArgNode, funcNode)
            val s2 = for (shrinkFuncNode <- funcNode.shrink) yield ReverseAp(argNode, shrinkFuncNode)

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = argNode.shrinkAny
            val s2 = funcNode.shrinkAny
            
            shrink ++ s1 ++ s2
        }

        override def satisfies(input: String): (Option[U], String) = argNode.satisfies(input) match {
            case (Some(arg), argStr) => funcNode.satisfies(argStr) match {
                case (Some(func), funcStr) => (Some(func(arg)), funcStr)
                case (None, funcStr) => (None, funcStr)
            }
            case (None, argStr) => (None, argStr)
        }

        override def possibleNext: Either[List[Char], U] = for {
            arg <- argNode.possibleNext
            func <- funcNode.possibleNext
        } yield func(arg)

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = for {
            af <- argNode.correct(forbidden)
            ff <- funcNode.correct(af)
        } yield ff

        override def correctInvalid(lastFailed: Boolean): Boolean = funcNode.correctInvalid(argNode.correctInvalid(lastFailed))
    }

    final case class Cons[T: PrettyPrint](elemNode: ParsleyInternal[T], listNode: ParsleyInternal[List[T]]) extends ParsleyInternal[List[T]] {
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[List[T]]]] = for {
            elemInternals <- elemNode.generate(n)
            listInternals <- listNode.generate(n)
            internal = elemInternals.zip(listInternals).map { case (elemI, listI) => ParsleyInternalUnwrapped.Cons(elemI, listI) }
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[List[T]]], Boolean)] = for {
            (elemInternals, elemFailed) <- elemNode.generateInvalid(n, lastFailed)
            (listInternals, listFailed) <- listNode.generateInvalid(n, elemFailed)
            internal = elemInternals.zip(listInternals).map { case (elemI, listI) => ParsleyInternalUnwrapped.Cons(elemI, listI) }
        } yield (internal, listFailed)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[List[T]]] = {
            val s1 = for (shrinkElemNode <- elemNode.shrink) yield Cons(shrinkElemNode, listNode)
            val s2 = for (shrinkListNode <- listNode.shrink) yield Cons(elemNode, shrinkListNode)
            val s3 = listNode.shrink

            s1 ++ s2 ++ s3
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = elemNode.shrinkAny
            val s2 = listNode.shrinkAny
            
            shrink ++ s1 ++ s2
        }

        override def satisfies(input: String): (Option[List[T]], String) = elemNode.satisfies(input) match {
            case (Some(elem), elemStr) => listNode.satisfies(elemStr) match {
                case (Some(list), listStr) => (Some(elem :: list), listStr)
                case (None, listStr) => (None, listStr)
            }
            case (None, elemStr) => (None, elemStr)
        }

        override def possibleNext: Either[List[Char], List[T]] = for {
            elem <- elemNode.possibleNext
            list <- listNode.possibleNext
        } yield elem :: list

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = for {
            ef <- elemNode.correct(forbidden)
            lf <- listNode.correct(ef)
        } yield lf

        override def correctInvalid(lastFailed: Boolean): Boolean = listNode.correctInvalid(elemNode.correctInvalid(lastFailed))
    }

    // <~>
    final case class Zip[T: PrettyPrint, U: PrettyPrint](leftNode: ParsleyInternal[T], rightNode: ParsleyInternal[U]) extends ParsleyInternal[(T, U)] {
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[(T, U)]]] = for {
            leftInternals <- leftNode.generate(n)
            rightInternals <- rightNode.generate(n)
            internal = leftInternals.zip(rightInternals).map { case (leftI, rightI) => ParsleyInternalUnwrapped.Zip(leftI, rightI) }
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[(T, U)]], Boolean)] = for {
            (leftInternals, leftFailed) <- leftNode.generateInvalid(n, lastFailed)
            (rightInternals, rightFailed) <- rightNode.generateInvalid(n, leftFailed)
            internal = leftInternals.zip(rightInternals).map { case (leftI, rightI) => ParsleyInternalUnwrapped.Zip(leftI, rightI) }
        } yield (internal, rightFailed)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[(T, U)]] = {
            val s1 = for (shrinkLeftNode <- leftNode.shrink) yield Zip(shrinkLeftNode, rightNode)
            val s2 = for (shrinkRightNode <- rightNode.shrink) yield Zip(leftNode, shrinkRightNode)

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = for (shrinkLeftNode <- leftNode.shrinkAny) yield Zip(shrinkLeftNode, rightNode)(shrinkLeftNode.evPrettyPrint, PrettyPrint[U])
            val s2 = for (shrinkRightNode <- rightNode.shrinkAny) yield Zip(leftNode, shrinkRightNode)(PrettyPrint[T], shrinkRightNode.evPrettyPrint)
            val s3 = leftNode.shrinkAny
            val s4 = rightNode.shrinkAny
            
            shrink ++ s1 ++ s2 ++ s3 ++ s4
        }

        override def satisfies(input: String): (Option[(T, U)], String) = leftNode.satisfies(input) match {
            case (Some(left), leftStr) => rightNode.satisfies(leftStr) match {
                case (Some(right), rightStr) => (Some((left, right)), rightStr)
                case (None, rightStr) => (None, rightStr)
            }
            case (None, leftStr) => (None, leftStr)
        }

        override def possibleNext: Either[List[Char], (T, U)] = for {
            left <- leftNode.possibleNext
            right <- rightNode.possibleNext
        } yield (left, right)

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = for {
            lf <- leftNode.correct(forbidden)
            rf <- rightNode.correct(lf)
        } yield rf

        override def correctInvalid(lastFailed: Boolean): Boolean = rightNode.correctInvalid(leftNode.correctInvalid(lastFailed))
    }

    final case class Branch[T: PrettyPrint, U: PrettyPrint, V: PrettyPrint](eitherNode: ParsleyInternal[Either[T, U]], leftNode: ParsleyInternal[ParsleyFunction[T, V]], rightNode: ParsleyInternal[ParsleyFunction[U, V]]) extends ParsleyInternal[V] {
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[V]]] = for {
            eitherInternals <- eitherNode.generate(n)
            leftInternals <- leftNode.generate(n)
            rightInternals <- rightNode.generate(n)
            internal = eitherInternals.zip(leftInternals.zip(rightInternals)).map { case (eitherI, (leftI, rightI)) => ParsleyInternalUnwrapped.Branch(eitherI, leftI, rightI) }
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[V]], Boolean)] = for {
            (eitherInternals, eitherFailed) <- eitherNode.generateInvalid(n, lastFailed)
            (leftInternals, leftFailed) <- leftNode.generateInvalid(n, eitherFailed)
            (rightInternals, rightFailed) <- rightNode.generateInvalid(n, eitherFailed)
            internal = eitherInternals.zip(leftInternals.zip(rightInternals)).map { case (eitherI, (leftI, rightI)) => ParsleyInternalUnwrapped.Branch(eitherI, leftI, rightI) }
        } yield (internal, leftFailed && rightFailed)
        
        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[V]] = {
            val s1 = for (shrinkEitherNode <- eitherNode.shrink) yield Branch(shrinkEitherNode, leftNode, rightNode)
            val s2 = for (shrinkLeftNode <- leftNode.shrink) yield Branch(eitherNode, shrinkLeftNode, rightNode)
            val s3 = for (shrinkRightNode <- rightNode.shrink) yield Branch(eitherNode, leftNode, shrinkRightNode)

            s1 ++ s2 ++ s3
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = eitherNode.shrinkAny
            val s2 = leftNode.shrinkAny
            val s3 = rightNode.shrinkAny
            
            shrink ++ s1 ++ s2 ++ s3
        }
        
        override def satisfies(input: String): (Option[V], String) = eitherNode.satisfies(input) match {
            case (Some(Left(eitherOutput)), str) => leftNode.satisfies(str) match {
                case (Some(leftOutput), leftStr) => (Some(leftOutput(eitherOutput)), leftStr)
                case (None, leftStr) => (None, leftStr)
            }
            case (Some(Right(eitherOutput)), str) => rightNode.satisfies(str) match {
                case (Some(rightOutput), rightStr) => (Some(rightOutput(eitherOutput)), rightStr)
                case (None, rightStr) => (None, rightStr)
            }
            case (None, str) => (None, str)
        }
        
        override def possibleNext: Either[List[Char], V] = eitherNode.possibleNext.flatMap {
            case Left(eitherOutput) => leftNode.possibleNext.map(_(eitherOutput))
            case Right(eitherOutput) => rightNode.possibleNext.map(_(eitherOutput))
        }

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = for {
            ef <- eitherNode.correct(forbidden)
            lf <- leftNode.correct(ef)
            rf <- rightNode.correct(ef)
        } yield lf ++ rf

        override def correctInvalid(lastFailed: Boolean): Boolean = {
            val eitherLastFailed = eitherNode.correctInvalid(lastFailed)
            leftNode.correctInvalid(eitherLastFailed) && rightNode.correctInvalid(eitherLastFailed)
        }
    }

    final case class FlatMap[T: PrettyPrint, U: PrettyPrint](node: ParsleyInternal[T], func: ParsleyFunction.UnwrappedFunction[T, ParsleyInternal[U]]) extends ParsleyInternal[U] {
        final def generateFunc(n: Int): Gen[List[ParsleyFunction.UnwrappedFunction[T, ParsleyInternalUnwrapped[U]]]] = for {
            // Without type annotations on Gen.sequence, a Java ArrayList is
            // inferred instead of a Scala List for some reason...
            newMapping <- Gen.sequence[List[List[(T, ParsleyInternalUnwrapped[U])]], List[(T, ParsleyInternalUnwrapped[U])]](func.mapping.map { case (k, v) => v.generate(n).map(_.map(k -> _)) }).map(_.map(_.toMap))
            newDefault <- func.default.generate(n)
        } yield newMapping.zip(newDefault).map { case (m, d) => ParsleyFunction.UnwrappedFunction(m, d) }

        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[U]]] = for {
            nodeInternals <- node.generate(n)
            funcs <- generateFunc(n)
            internal = nodeInternals.zip(funcs).map { case (nodeI, f) => ParsleyInternalUnwrapped.FlatMap(nodeI, f) }
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[U]], Boolean)] = for {
            (nodeInternals, nodeFailed) <- node.generateInvalid(n, lastFailed)
            funcs <- generateFunc(n)
            internal = nodeInternals.zip(funcs).map { case (nodeI, f) => ParsleyInternalUnwrapped.FlatMap(nodeI, f) }
        } yield (internal, nodeFailed)
        
        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[U]] = {
            val defaultShrink = func.default.shrink
            val mappingShrink = Shrink.shrink(func.mapping)

            val s1 = for (shrinkNode <- node.shrink) yield FlatMap(shrinkNode, func)
            val s2 = for {
                shrinkDefault <- defaultShrink
                shrinkMapping <- mappingShrink.map(_.toMap)
                shrinkFunc = ParsleyFunction.UnwrappedFunction(shrinkMapping, shrinkDefault)
            } yield FlatMap(node, shrinkFunc)
            val s3 = defaultShrink ++ mappingShrink.flatten.map { case (_, p) => p }

            s1 ++ s2 ++ s3
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = node.shrinkAny
            
            shrink ++ s1
        }
        
        override def satisfies(input: String): (Option[U], String) = node.satisfies(input) match {
            case (Some(nodeOutput), str) => func(nodeOutput).satisfies(str) match {
                case (Some(funcOutput), funcStr) => (Some(funcOutput), funcStr)
                case (None, funcStr) => (None, funcStr)
            }
            case (None, str) => (None, str)
        }
        
        override def possibleNext: Either[List[Char], U] = node.possibleNext.flatMap(func(_).possibleNext)

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = for {
            nf <- node.correct(forbidden)
            // Essentially a handrolled Applicative instance for Either
            ff <- (func.mapping.values ++ Seq(func.default)).foldRight[Either[Unit, Set[Char]]](Right(Set())) { case (parser, fEither) => parser.correct(nf).flatMap(fList => fEither.map(_ ++ fList)) }
        } yield ff

        override def correctInvalid(lastFailed: Boolean): Boolean = {
            val nodeLastFailed = node.correctInvalid(lastFailed)
            func.default.correctInvalid(nodeLastFailed) && func.mapping.forall { case (_, p) => p.correctInvalid(nodeLastFailed) }
        }
    }

    // TODO: Actually implement `flatten`
    final case class Flatten[T: PrettyPrint](node: ParsleyInternal[ParsleyInternal[T]]) extends ParsleyInternal[T] {
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[T]]] = ??? // node.generate(n).map(_.map(ParsleyInternalUnwrapped.Flatten(_)))

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[T]], Boolean)] = ??? // node.generateInvalid(n, lastFailed).map(_.map(ParsleyInternalUnwrapped.Flatten(_)))
        
        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[T]] = for (shrinkNode <- node.shrink) yield Flatten(shrinkNode)

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = node.shrinkAny
            
            shrink ++ s1
        }
        
        override def satisfies(input: String): (Option[T], String) = node.satisfies(input) match {
            case (Some(innerOutput), str) => innerOutput.satisfies(str)
            case (None, str) => (None, str)
        }
        
        override def possibleNext: Either[List[Char], T] = node.possibleNext.flatMap(_.possibleNext)

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = node.correct(forbidden)

        override def correctInvalid(lastFailed: Boolean): Boolean = node.correctInvalid(lastFailed)
    }

    final case class Fresh[T: PrettyPrint](value: T) extends ParsleyInternal[T] {
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[T]]] = for {
            internal <- Gen.listOfN(n, Gen.const(ParsleyInternalUnwrapped.Fresh(value)))
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[T]], Boolean)] = generate(n).map((_, lastFailed))
        
        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[T]] = for (shrinkVal <- Shrink.shrink(value)) yield Fresh(shrinkVal)

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = shrink
        
        override def satisfies(input: String): (Option[T], String) = (Some(value), input)
        
        override def possibleNext: Either[List[Char], T] = Right(value)

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = Right(forbidden)

        override def correctInvalid(lastFailed: Boolean): Boolean = lastFailed
    }

    final case class Select[T: PrettyPrint, U: PrettyPrint](eitherNode: ParsleyInternal[Either[T, U]], leftFuncNode: ParsleyInternal[ParsleyFunction[T, U]]) extends ParsleyInternal[U] {
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[U]]] = for {
            eitherInternals <- eitherNode.generate(n)
            leftFuncInternals <- leftFuncNode.generate(n)
            internal = eitherInternals.zip(leftFuncInternals).map { case (eitherI, leftI) => ParsleyInternalUnwrapped.Select(eitherI, leftI) }
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[U]], Boolean)] = for {
            (eitherInternals, eitherFailed) <- eitherNode.generateInvalid(n, lastFailed)
            (leftInternals, leftFailed) <- leftFuncNode.generateInvalid(n, eitherFailed)
            internal = eitherInternals.zip(leftInternals).map { case (eitherI, leftI) => ParsleyInternalUnwrapped.Select(eitherI, leftI) }
        } yield (internal, eitherFailed)
        
        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[U]] = {
            val s1 = for (shrinkEitherNode <- eitherNode.shrink) yield Select(shrinkEitherNode, leftFuncNode)
            val s2 = for (shrinkLeftFuncNode <- leftFuncNode.shrink) yield Select(eitherNode, shrinkLeftFuncNode)

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = eitherNode.shrinkAny
            val s2 = leftFuncNode.shrinkAny
            
            shrink ++ s1 ++ s2
        }
        
        override def satisfies(input: String): (Option[U], String) = eitherNode.satisfies(input) match {
            case (Some(Left(eitherOutput)), str) => leftFuncNode.satisfies(str) match {
                case (Some(leftOutput), leftStr) => (Some(leftOutput(eitherOutput)), leftStr)
                case (None, leftStr) => (None, leftStr)
            }
            case (Some(Right(eitherOutput)), str) => (Some(eitherOutput), str)
            case (None, str) => (None, str)
        }
        
        override def possibleNext: Either[List[Char], U] = eitherNode.possibleNext.flatMap {
            case Left(eitherOutput) => leftFuncNode.possibleNext.map(_(eitherOutput))
            case Right(eitherOutput) => Right(eitherOutput)
        }

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = for {
            ef <- eitherNode.correct(forbidden)
            lf <- leftFuncNode.correct(ef)
        } yield ef ++ lf

        override def correctInvalid(lastFailed: Boolean): Boolean = eitherNode.correctInvalid(lastFailed)
    }

    final case class Collect[T: PrettyPrint, U: PrettyPrint](node: ParsleyInternal[T], res: U) extends ParsleyInternal[U] {
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[U]]] = for {
            nodeInternal <- node.generate(n)
            outputs = nodeInternal.map(_.output).collect { case Some(x) => x }.toSet
            internal = nodeInternal.map(ParsleyInternalUnwrapped.Collect(_, outputs, res))
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[U]], Boolean)] = for {
            (nodeInternal, nodeFailed) <- node.generateInvalid(n, lastFailed)
            outputs = if (nodeFailed) Set[T]() else nodeInternal.map(_.output.getOrElse(throw new Exception("Filter should get all outputs but did not"))).toSet
            internal = nodeInternal.map(ParsleyInternalUnwrapped.Collect(_, outputs, res))
        } yield (internal, nodeFailed)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[U]] = {
            val s1 = for (shrinkNode <- node.shrink) yield Collect(shrinkNode, res)
            val s2 = for (shrinkRes <- Shrink.shrink(res)) yield Pure(shrinkRes)

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = for (shrinkNode <- node.shrinkAny) yield Collect(shrinkNode, res)(shrinkNode.evPrettyPrint, PrettyPrint[U])
            val s2 = node.shrinkAny
            
            shrink ++ s1 ++ s2
        }

        override def satisfies(input: String): (Option[U], String) = node.satisfies(input) match {
            case (Some(_), str) => (Some(res), str)
            case (_, str) => (None, str)
        }

        override def possibleNext: Either[List[Char], U] = node.possibleNext.map(Function.const(res))

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = node.correct(forbidden)

        override def correctInvalid(lastFailed: Boolean): Boolean = node.correctInvalid(lastFailed)
    }

    final case class MapFilter[T: PrettyPrint, U: PrettyPrint](node: ParsleyInternal[T], res: U) extends ParsleyInternal[U] {
        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[U]]] = for {
            nodeInternal <- node.generate(n)
            outputs = nodeInternal.map(_.output).collect { case Some(x) => x }.toSet
            internal = nodeInternal.map(ParsleyInternalUnwrapped.MapFilter(_, outputs, res))
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[U]], Boolean)] = for {
            (nodeInternal, nodeFailed) <- node.generateInvalid(n, lastFailed)
            outputs = if (nodeFailed) Set[T]() else nodeInternal.map(_.output.getOrElse(throw new Exception("Filter should get all outputs but did not"))).toSet
            internal = nodeInternal.map(ParsleyInternalUnwrapped.MapFilter(_, outputs, res))
        } yield (internal, nodeFailed)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[U]] = {
            val s1 = for (shrinkNode <- node.shrink) yield MapFilter(shrinkNode, res)
            val s2 = for (shrinkRes <- Shrink.shrink(res)) yield Pure(shrinkRes)

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = for (shrinkNode <- node.shrinkAny) yield MapFilter(shrinkNode, res)(shrinkNode.evPrettyPrint, PrettyPrint[U])
            val s2 = node.shrinkAny
            
            shrink ++ s1 ++ s2
        }

        override def satisfies(input: String): (Option[U], String) = node.satisfies(input) match {
            case (Some(_), str) => (Some(res), str)
            case (_, str) => (None, str)
        }

        override def possibleNext: Either[List[Char], U] = node.possibleNext.map(Function.const(res))

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = node.correct(forbidden)

        override def correctInvalid(lastFailed: Boolean): Boolean = node.correctInvalid(lastFailed)
    }

    abstract class PFold[T, U: PrettyPrint](node: ParsleyInternal[T], atLeastOne: Boolean, initialValue: U, funcNode: Either[ParsleyFunction[(U, T), U], ParsleyFunction[(T, U), U]]) extends ParsleyInternal[U] {
        // TODO: Should not be asserted if intended to fail
        // require(node.possibleNext.isLeft, "Fold combinators require input")

        def generateNode(internalNodes: List[ParsleyInternalUnwrapped[T]]): ParsleyInternalUnwrapped[U]

        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[U]]] = for {
            rand <- Gen.choose(1, 25)
            foldInternals <- node.generate(rand)
            internal <- Gen.listOfN(n, Gen.atLeastOne(foldInternals).map(_.toList).map(generateNode(_)))
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[U]], Boolean)] = for {
            rand <- Gen.choose(1, 25)
            (foldInternals, _) <- node.generateInvalid(rand, lastFailed)
            internal <- Gen.listOfN(n, Gen.atLeastOne(foldInternals).map(_.toList).map(generateNode(_)))
        } yield (internal, lastFailed)

        override def satisfies(input: String): (Option[U], String) = {
            var (l, i): (List[T], String) = if (atLeastOne) {
                val (value, str) = node.satisfies(input)
                value match {
                    case Some(value) => (List(value), str)
                    case None => return (None, str)
                }
            } else (List(), input)
            var cont = true

            while (cont) {
                val (value, str) = node.satisfies(i)
                value match {
                    case Some(value) => l = l :+ value
                    case None if i == str => cont = false
                    case None => return (None, str)
                }
                i = str
            }

            funcNode match {
                case Left(f) => {
                    // Expand tuple to parameter list
                    val func: ((U, T) => U) = (u, t) => f((u, t))
                    (Some(l.foldLeft(initialValue)(func)), i)
                }
                case Right(f) => {
                    // Expand tuple to parameter list
                    val func: ((T, U) => U) = (t, u) => f((t, u))
                    (Some(l.foldRight(initialValue)(func)), i)
                }
            }
        }

        override def possibleNext: Either[List[Char], U] = Left(node.possibleNext.swap.getOrElse(List()))

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = node.correct(forbidden).map((if (atLeastOne) Set() else forbidden) ++ _ ++ node.possibleNext.swap.getOrElse(List()))

        override def correctInvalid(lastFailed: Boolean): Boolean = node.correctInvalid(lastFailed)
    }

    abstract class PReduce[T, U >: T: PrettyPrint](node: ParsleyInternal[T], funcNode: Either[ParsleyFunction[(U, T), U], ParsleyFunction[(T, U), U]]) extends ParsleyInternal[U] {
        // TODO: Should not be asserted if intended to fail
        // require(node.possibleNext.isLeft, "Reduce combinators require input")

        def generateNode(internalNodes: List[ParsleyInternalUnwrapped[T]]): ParsleyInternalUnwrapped[U]

        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[U]]] = for {
            rand <- Gen.choose(1, 25)
            foldInternals <- node.generate(rand)
            internal <- Gen.listOfN(n, Gen.atLeastOne(foldInternals).map(_.toList).map(generateNode(_)))
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[U]], Boolean)] = for {
            rand <- Gen.choose(1, 25)
            (foldInternals, foldFailed) <- node.generateInvalid(rand, lastFailed)
            internal <- Gen.listOfN(n, Gen.atLeastOne(foldInternals).map(_.toList).map(generateNode(_)))
        } yield (internal, foldFailed)

        override def satisfies(input: String): (Option[U], String) = {
            var (l, i): (List[T], String) = node.satisfies(input) match {
                case (Some(value), str) => (List(value), str)
                case (None, str) => return (None, str)
            }
            var cont = true

            while (cont) {
                val (value, str) = node.satisfies(i)
                value match {
                    case Some(value) => l = l :+ value
                    case None if i == str => cont = false
                    case None => return (None, str)
                }
                i = str
            }

            funcNode match {
                case Left(f) => {
                    // Expand tuple to parameter list
                    val func: ((U, T) => U) = (u, t) => f((u, t))
                    (Some(l.reduceLeft(func)), i)
                }
                case Right(f) => {
                    // Expand tuple to parameter list
                    val func: ((T, U) => U) = (t, u) => f((t, u))
                    (Some(l.reduceRight(func)), i)
                }
            }
        }

        override def possibleNext: Either[List[Char], U] = Left(node.possibleNext.swap.getOrElse(List()))

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = node.correct(forbidden).map(_ ++ node.possibleNext.swap.getOrElse(List()))

        override def correctInvalid(lastFailed: Boolean): Boolean = node.correctInvalid(lastFailed)
    }

    abstract class PReduceOption[T, U >: T: PrettyPrint](node: ParsleyInternal[T], funcNode: Either[ParsleyFunction[(U, T), U], ParsleyFunction[(T, U), U]]) extends ParsleyInternal[Option[U]] {
        // TODO: Should not be asserted if intended to fail
        // require(node.possibleNext.isLeft, "Reduce option combinators require input")

        def generateNode(internalNodes: List[ParsleyInternalUnwrapped[T]]): ParsleyInternalUnwrapped[Option[U]]

        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[Option[U]]]] = for {
            rand <- Gen.choose(1, 25)
            foldInternals <- node.generate(rand)
            internal <- Gen.listOfN(n, Gen.atLeastOne(foldInternals).map(_.toList).map(generateNode(_)))
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[Option[U]]], Boolean)] = for {
            rand <- Gen.choose(1, 25)
            (foldInternals, _) <- node.generateInvalid(rand, lastFailed)
            internal <- Gen.listOfN(n, Gen.atLeastOne(foldInternals).map(_.toList).map(generateNode(_)))
        } yield (internal, lastFailed)

        override def satisfies(input: String): (Option[Option[U]], String) = {
            var (l, i): (List[T], String) = node.satisfies(input) match {
                case (Some(value), str) => (List(value), str)
                case (None, str) if str == input => return (Some(None), str)
                case (None, str) => return (None, str)
            }
            var cont = true

            while (cont) {
                val (value, str) = node.satisfies(i)
                value match {
                    case Some(value) => l = l :+ value
                    case None if i == str => cont = false
                    case None => return (None, str)
                }
                i = str
            }

            funcNode match {
                case Left(f) => {
                    // Expand tuple to parameter list
                    val func: ((U, T) => U) = (u, t) => f((u, t))
                    (Some(l.reduceLeftOption(func)), i)
                }
                case Right(f) => {
                    // Expand tuple to parameter list
                    val func: ((T, U) => U) = (t, u) => f((t, u))
                    (Some(l.reduceRightOption(func)), i)
                }
            }
        }

        override def possibleNext: Either[List[Char], Option[U]] = Left(node.possibleNext.swap.getOrElse(List()))

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = node.correct(forbidden).map(forbidden ++ _ ++ node.possibleNext.swap.getOrElse(List()))

        override def correctInvalid(lastFailed: Boolean): Boolean = node.correctInvalid(lastFailed)
    }

    final case class FoldLeft[T, U: PrettyPrint](node: ParsleyInternal[T], initialValue: U, funcNode: ParsleyFunction[(U, T), U]) extends PFold[T, U](node, false, initialValue, Left(funcNode)) {
        def generateNode(internalNodes: List[ParsleyInternalUnwrapped[T]]): ParsleyInternalUnwrapped[U] = ParsleyInternalUnwrapped.FoldLeft(internalNodes, initialValue, funcNode)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[U]] = {
            val s1 = for (shrinkNode <- node.shrink) yield FoldLeft(shrinkNode, initialValue, funcNode)
            val s2 = for (shrinkRes <- Shrink.shrink(initialValue)) yield Pure(shrinkRes)

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = node.shrinkAny
            
            shrink ++ s1
        }
    }

    final case class FoldLeft1[T, U: PrettyPrint](node: ParsleyInternal[T], initialValue: U, funcNode: ParsleyFunction[(U, T), U]) extends PFold[T, U](node, true, initialValue, Left(funcNode)) {
        def generateNode(internalNodes: List[ParsleyInternalUnwrapped[T]]): ParsleyInternalUnwrapped[U] = ParsleyInternalUnwrapped.FoldLeft1(internalNodes, initialValue, funcNode)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[U]] = {
            val s1 = for (shrinkNode <- node.shrink) yield FoldLeft1(shrinkNode, initialValue, funcNode)
            val s2 = for (shrinkRes <- Shrink.shrink(initialValue)) yield Pure(shrinkRes)

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = node.shrinkAny
            
            shrink ++ s1
        }
    }

    final case class FoldRight[T, U: PrettyPrint](node: ParsleyInternal[T], initialValue: U, funcNode: ParsleyFunction[(T, U), U]) extends PFold[T, U](node, false, initialValue, Right(funcNode)) {
        def generateNode(internalNodes: List[ParsleyInternalUnwrapped[T]]): ParsleyInternalUnwrapped[U] = ParsleyInternalUnwrapped.FoldRight(internalNodes, initialValue, funcNode)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[U]] = {
            val s1 = for (shrinkNode <- node.shrink) yield FoldRight(shrinkNode, initialValue, funcNode)
            val s2 = for (shrinkRes <- Shrink.shrink(initialValue)) yield Pure(shrinkRes)

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = node.shrinkAny
            
            shrink ++ s1
        }
    }

    final case class FoldRight1[T, U: PrettyPrint](node: ParsleyInternal[T], initialValue: U, funcNode: ParsleyFunction[(T, U), U]) extends PFold[T, U](node, true, initialValue, Right(funcNode)) {
        def generateNode(internalNodes: List[ParsleyInternalUnwrapped[T]]): ParsleyInternalUnwrapped[U] = ParsleyInternalUnwrapped.FoldRight1(internalNodes, initialValue, funcNode)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[U]] = {
            val s1 = for (shrinkNode <- node.shrink) yield FoldRight1(shrinkNode, initialValue, funcNode)
            val s2 = for (shrinkRes <- Shrink.shrink(initialValue)) yield Pure(shrinkRes)

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = node.shrinkAny
            
            shrink ++ s1
        }
    }

    final case class ReduceLeft[T, U >: T: PrettyPrint](node: ParsleyInternal[T], funcNode: ParsleyFunction[(U, T), U]) extends PReduce[T, U](node, Left(funcNode)) {
        def generateNode(internalNodes: List[ParsleyInternalUnwrapped[T]]): ParsleyInternalUnwrapped[U] = ParsleyInternalUnwrapped.ReduceLeft(internalNodes, funcNode)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[U]] = for (shrinkNode <- node.shrink) yield ReduceLeft(shrinkNode, funcNode)

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = node.shrinkAny
            
            shrink ++ s1
        }
    }

    final case class ReduceRight[T, U >: T: PrettyPrint](node: ParsleyInternal[T], funcNode: ParsleyFunction[(T, U), U]) extends PReduce[T, U](node, Right(funcNode)) {
        def generateNode(internalNodes: List[ParsleyInternalUnwrapped[T]]): ParsleyInternalUnwrapped[U] = ParsleyInternalUnwrapped.ReduceRight(internalNodes, funcNode)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[U]] = for (shrinkNode <- node.shrink) yield ReduceRight(shrinkNode, funcNode)

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = node.shrinkAny
            
            shrink ++ s1
        }
    }

    final case class ReduceOptionLeft[T, U >: T: PrettyPrint](node: ParsleyInternal[T], funcNode: ParsleyFunction[(U, T), U]) extends PReduceOption[T, U](node, Left(funcNode)) {
        def generateNode(internalNodes: List[ParsleyInternalUnwrapped[T]]): ParsleyInternalUnwrapped[Option[U]] = ParsleyInternalUnwrapped.ReduceOptionLeft(internalNodes, funcNode)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[Option[U]]] = for (shrinkNode <- node.shrink) yield ReduceOptionLeft(shrinkNode, funcNode)

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = node.shrinkAny
            
            shrink ++ s1
        }
    }

    final case class ReduceOptionRight[T, U >: T: PrettyPrint](node: ParsleyInternal[T], funcNode: ParsleyFunction[(T, U), U]) extends PReduceOption[T, U](node, Right(funcNode)) {
        def generateNode(internalNodes: List[ParsleyInternalUnwrapped[T]]): ParsleyInternalUnwrapped[Option[U]] = ParsleyInternalUnwrapped.ReduceOptionRight(internalNodes, funcNode)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[Option[U]]] = for (shrinkNode <- node.shrink) yield ReduceOptionRight(shrinkNode, funcNode)

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = node.shrinkAny
            
            shrink ++ s1
        }
    }

    sealed trait SepEnding
    object SepEnding {
        final object Required extends SepEnding
        final object Optional extends SepEnding
        final object Prohibited extends SepEnding
    }

    abstract class Separated[T: PrettyPrint](node: ParsleyInternal[T], separated: ParsleyInternal[?], sepEnding: SepEnding, atLeastOne: Boolean) extends ParsleyInternal[List[T]] {
        // TODO: Should not be asserted if intended to fail
        // require(node.possibleNext.isLeft && separated.possibleNext.isLeft, "Separated combinators require input")

        def generateNode(internalNodes: List[(ParsleyInternalUnwrapped[T], ParsleyInternalUnwrapped[?])], endingNode: Option[ParsleyInternalUnwrapped[T]]): ParsleyInternalUnwrapped[List[T]]

        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[List[T]]]] = for {
            rand <- Gen.choose(1, 25)
            nodeInternals <- node.generate(rand)
            separatedInternals <- separated.generate(rand)
            internalNodes = nodeInternals.zip(separatedInternals)
            endingNodes <- node.generate(n)
            endingNodeGen = sepEnding match {
                case SepEnding.Required => Gen.const(None)
                case SepEnding.Optional => Gen.option(Gen.oneOf(endingNodes))
                case SepEnding.Prohibited => Gen.oneOf(endingNodes).map(Some(_))
            }
            internal <- Gen.listOfN(n, Gen.zip(Gen.atLeastOne(internalNodes).map(_.toList), endingNodeGen).map { case (internals, ending) => generateNode(internals, ending) })
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[List[T]]], Boolean)] = for {
            rand <- Gen.choose(1, 25)
            (nodeInternals, nodeFailed) <- node.generateInvalid(rand, lastFailed)
            (separatedInternals, separatedFailed) <- separated.generateInvalid(rand, nodeFailed)
            internalNodes = nodeInternals.zip(separatedInternals)
            (endingNodes, endingFailed) <- node.generateInvalid(n, separatedFailed)
            endingNodeGen = sepEnding match {
                case SepEnding.Required => Gen.const(None)
                case SepEnding.Optional => Gen.option(Gen.oneOf(endingNodes))
                case SepEnding.Prohibited => Gen.oneOf(endingNodes).map(Some(_))
            }
            internal <- Gen.listOfN(n, Gen.zip(Gen.atLeastOne(internalNodes).map(_.toList), endingNodeGen).map { case (internals, ending) => generateNode(internals, ending) })
        } yield (internal, if (atLeastOne) nodeFailed else lastFailed)

        override def satisfies(input: String): (Option[List[T]], String) = {
            var (l, i): (List[T], String) = {
                val (value, str) = node.satisfies(input)
                value match {
                    case Some(value) => (List(value), str)
                    case None if !atLeastOne && str == input => return (Some(List()), str)
                    case None => return (None, str)
                }
            }
            var cont = true

            while (cont) {
                val (sepValue, sepStr) = separated.satisfies(i)
                sepValue match {
                    case Some(_) => ()
                    case None if i == sepStr && sepEnding != SepEnding.Required => cont = false
                    case None => return (None, sepStr)
                }

                if (cont) {
                    val (value, str) = node.satisfies(sepStr)
                    value match {
                        case Some(value) => l = l :+ value
                        case None if i == str && sepEnding != SepEnding.Prohibited => cont = false
                        case None => return (None, str)
                    }
                    i = str
                }
            }

            (Some(l), i)
        }

        override def possibleNext: Either[List[Char], List[T]] = Left(node.possibleNext.swap.getOrElse(List()))

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = for {
            nf <- node.correct(forbidden)
            sf <- separated.correct(nf)
            nodePossible = node.possibleNext.swap.getOrElse(List())
            sepPossible = separated.possibleNext.swap.getOrElse(List())
            forbidPossible = sepEnding match {
                case SepEnding.Required => nodePossible
                case SepEnding.Optional => nodePossible ++ sepPossible
                case SepEnding.Prohibited => sepPossible
            }
        } yield nf ++ sf ++ (if (atLeastOne) List() else forbidden) ++ forbidPossible

        override def correctInvalid(lastFailed: Boolean): Boolean = if (atLeastOne) node.correctInvalid(lastFailed) else lastFailed
    }

    final case class EndBy[T: PrettyPrint](node: ParsleyInternal[T], separated: ParsleyInternal[?]) extends Separated[T](node, separated, SepEnding.Required, false) {
        def generateNode(internalNodes: List[(ParsleyInternalUnwrapped[T], ParsleyInternalUnwrapped[?])], endingNode: Option[ParsleyInternalUnwrapped[T]]): ParsleyInternalUnwrapped[List[T]] =
            ParsleyInternalUnwrapped.EndBy(internalNodes)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[List[T]]] = {
            val s1 = for (shrinkNode <- node.shrink) yield EndBy(shrinkNode, separated)
            val s2 = for (shrinkSeparated <- separated.shrink) yield EndBy(node, shrinkSeparated)

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = node.shrinkAny
            val s2 = separated.shrinkAny
            
            shrink ++ s1 ++ s2
        }
    }

    final case class EndBy1[T: PrettyPrint](node: ParsleyInternal[T], separated: ParsleyInternal[?]) extends Separated[T](node, separated, SepEnding.Required, true) {
        def generateNode(internalNodes: List[(ParsleyInternalUnwrapped[T], ParsleyInternalUnwrapped[?])], endingNode: Option[ParsleyInternalUnwrapped[T]]): ParsleyInternalUnwrapped[List[T]] =
            ParsleyInternalUnwrapped.EndBy1(internalNodes)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[List[T]]] = {
            val s1 = for (shrinkNode <- node.shrink) yield EndBy1(shrinkNode, separated)
            val s2 = for (shrinkSeparated <- separated.shrink) yield EndBy1(node, shrinkSeparated)

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = node.shrinkAny
            val s2 = separated.shrinkAny
            
            shrink ++ s1 ++ s2
        }
    }

    final case class SepBy[T: PrettyPrint](node: ParsleyInternal[T], separated: ParsleyInternal[?]) extends Separated[T](node, separated, SepEnding.Prohibited, false) {
        def generateNode(internalNodes: List[(ParsleyInternalUnwrapped[T], ParsleyInternalUnwrapped[?])], endingNode: Option[ParsleyInternalUnwrapped[T]]): ParsleyInternalUnwrapped[List[T]] =
            ParsleyInternalUnwrapped.SepBy(internalNodes, endingNode.get)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[List[T]]] = {
            val s1 = for (shrinkNode <- node.shrink) yield SepBy(shrinkNode, separated)
            val s2 = for (shrinkSeparated <- separated.shrink) yield SepBy(node, shrinkSeparated)

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = node.shrinkAny
            val s2 = separated.shrinkAny
            
            shrink ++ s1 ++ s2
        }
    }

    final case class SepBy1[T: PrettyPrint](node: ParsleyInternal[T], separated: ParsleyInternal[?]) extends Separated[T](node, separated, SepEnding.Prohibited, true) {
        def generateNode(internalNodes: List[(ParsleyInternalUnwrapped[T], ParsleyInternalUnwrapped[?])], endingNode: Option[ParsleyInternalUnwrapped[T]]): ParsleyInternalUnwrapped[List[T]] =
            ParsleyInternalUnwrapped.SepBy1(internalNodes, endingNode.get)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[List[T]]] = {
            val s1 = for (shrinkNode <- node.shrink) yield SepBy1(shrinkNode, separated)
            val s2 = for (shrinkSeparated <- separated.shrink) yield SepBy1(node, shrinkSeparated)

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = node.shrinkAny
            val s2 = separated.shrinkAny
            
            shrink ++ s1 ++ s2
        }
    }

    final case class SepEndBy[T: PrettyPrint](node: ParsleyInternal[T], separated: ParsleyInternal[?]) extends Separated[T](node, separated, SepEnding.Optional, false) {
        def generateNode(internalNodes: List[(ParsleyInternalUnwrapped[T], ParsleyInternalUnwrapped[?])], endingNode: Option[ParsleyInternalUnwrapped[T]]): ParsleyInternalUnwrapped[List[T]] =
            ParsleyInternalUnwrapped.SepEndBy(internalNodes, endingNode)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[List[T]]] = {
            val s1 = for (shrinkNode <- node.shrink) yield SepEndBy(shrinkNode, separated)
            val s2 = for (shrinkSeparated <- separated.shrink) yield SepEndBy(node, shrinkSeparated)

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = node.shrinkAny
            val s2 = separated.shrinkAny
            
            shrink ++ s1 ++ s2
        }
    }

    final case class SepEndBy1[T: PrettyPrint](node: ParsleyInternal[T], separated: ParsleyInternal[?]) extends Separated[T](node, separated, SepEnding.Optional, true) {
        def generateNode(internalNodes: List[(ParsleyInternalUnwrapped[T], ParsleyInternalUnwrapped[?])], endingNode: Option[ParsleyInternalUnwrapped[T]]): ParsleyInternalUnwrapped[List[T]] =
            ParsleyInternalUnwrapped.SepEndBy1(internalNodes, endingNode)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[List[T]]] = {
            val s1 = for (shrinkNode <- node.shrink) yield SepEndBy1(shrinkNode, separated)
            val s2 = for (shrinkSeparated <- separated.shrink) yield SepEndBy1(node, shrinkSeparated)

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = node.shrinkAny
            val s2 = separated.shrinkAny
            
            shrink ++ s1 ++ s2
        }
    }

    abstract class Count(node: ParsleyInternal[?], atLeastOne: Boolean) extends ParsleyInternal[Int] {
        // TODO: Should not be asserted if intended to fail
        // require(node.possibleNext.isLeft, "Count combinators require input")

        def generateNode(internalNodes: List[ParsleyInternalUnwrapped[?]]): ParsleyInternalUnwrapped[Int]

        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[Int]]] = for {
            rand <- Gen.choose(1, 25)
            manyInternals <- node.generate(rand)
            internal <- Gen.listOfN(n, Gen.atLeastOne(manyInternals).map(_.toList).map(generateNode(_)))
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[Int]], Boolean)] = for {
            rand <- Gen.choose(1, 25)
            (manyInternals, manyFailed) <- node.generateInvalid(rand, lastFailed)
            internal <- Gen.listOfN(n, Gen.atLeastOne(manyInternals).map(_.toList).map(generateNode(_)))
        } yield (internal, if (atLeastOne) manyFailed else lastFailed)

        override def satisfies(input: String): (Option[Int], String) = {
            var (l, i): (Int, String) = if (atLeastOne) {
                val (value, str) = node.satisfies(input)
                value match {
                    case Some(_) => (1, str)
                    case None => return (None, str)
                }
            } else (0, input)
            var cont = true

            while (cont) {
                val (value, str) = node.satisfies(i)
                value match {
                    case Some(_) => l += 1
                    case None if i == str => cont = false
                    case None => return (None, str)
                }
                i = str
            }

            (Some(l), i)
        }

        // TODO: Same as before, could have either zero or more iterations
        // so should be using Ior from cats
        override def possibleNext: Either[List[Char], Int] = Left(node.possibleNext.swap.getOrElse(List()))

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = node.correct(forbidden).map((if (atLeastOne) Set() else forbidden) ++ _)

        override def correctInvalid(lastFailed: Boolean): Boolean = node.correctInvalid(lastFailed)
    }

    final case class CountMany(node: ParsleyInternal[?]) extends Count(node, false) {
        override def generateNode(internalNodes: List[ParsleyInternalUnwrapped[?]]): ParsleyInternalUnwrapped[Int] = ParsleyInternalUnwrapped.CountMany(internalNodes)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[Int]] = for (shrinkNode <- node.shrink) yield CountMany(shrinkNode)

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = node.shrinkAny
            
            shrink ++ s1
        }
    }

    final case class CountSome(node: ParsleyInternal[?]) extends Count(node, true) {
        override def generateNode(internalNodes: List[ParsleyInternalUnwrapped[?]]): ParsleyInternalUnwrapped[Int] = ParsleyInternalUnwrapped.CountSome(internalNodes)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[Int]] = for (shrinkNode <- node.shrink) yield CountSome(shrinkNode)

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = node.shrinkAny
            
            shrink ++ s1
        }
    }

    final case class HomogeneousPrecedence(atoms: List[ParsleyInternal[HomogeneousExpr]], ops: List[(Fixity, List[ParsleyInternal[Any]])]) extends ParsleyInternal[HomogeneousExpr] {
        require(!atoms.isEmpty, "Must provide at least one atom for homogeneous precedence parsing in generic ADT")
        require(!ops.isEmpty, "Must provide at least one operation for homogeneous precedence parsing in generic ADT")

        def generateExpr(maxBinding: Int, atoms: List[ParsleyInternalUnwrapped[HomogeneousExpr]], ops: List[(Fixity, List[ParsleyInternalUnwrapped[Any]])]): Gen[(String, Option[HomogeneousExpr])] = Gen.oneOf(
            Gen.oneOf(atoms).map(a => (a.input, a.output)),
            Gen.oneOf(atoms).map(a => (a.input, a.output)),
            ((if (maxBinding != 0) List(
                for {
                    binding <- Gen.choose(1, if (ops.length < maxBinding) ops.length else maxBinding)
                    (fixity, ps) = ops(binding - 1)
                    p <- Gen.oneOf(ps)
                    res <- fixity match {
                        case InfixL => for {
                            (leftStr, leftExpr) <- generateExpr(binding, atoms, ops)
                            (rightStr, rightExpr) <- generateExpr(binding - 1, atoms, ops)
                        } yield (f"$leftStr${p.input}$rightStr",
                            for {
                                left <- leftExpr
                                right <- rightExpr
                            } yield HomogeneousExpr.LeftBinaryExpr(binding, left, right))
                        case InfixN => for {
                            (leftStr, leftExpr) <- generateExpr(binding - 1, atoms, ops)
                            (rightStr, rightExpr) <- generateExpr(binding - 1, atoms, ops)
                        } yield (f"$leftStr${p.input}$rightStr",
                            for {
                                left <- leftExpr
                                right <- rightExpr
                            } yield HomogeneousExpr.NonBinaryExpr(binding, left, right))
                        case InfixR => for {
                            (leftStr, leftExpr) <- generateExpr(binding - 1, atoms, ops)
                            (rightStr, rightExpr) <- generateExpr(binding, atoms, ops)
                        } yield (f"$leftStr${p.input}$rightStr",
                            for {
                                left <- leftExpr
                                right <- rightExpr
                            } yield HomogeneousExpr.RightBinaryExpr(binding, left, right))
                        case Prefix => for {
                            (str, expr) <- generateExpr(binding, atoms, ops)
                        } yield (f"${p.input}$str",
                            for {
                                e <- expr
                            } yield HomogeneousExpr.PrefixUnaryExpr(binding, e))
                        case Postfix => for {
                            (str, expr) <- generateExpr(binding, atoms, ops)
                        } yield (f"$str${p.input}",
                            for {
                                e <- expr
                            } yield HomogeneousExpr.PostfixUnaryExpr(binding, e))
                    }
                } yield res
            ) else List()): _*)
        )

        override def generate(n: Int): Gen[List[ParsleyInternalUnwrapped[HomogeneousExpr]]] = for {
            atomInternals <- Gen.sequence[List[ParsleyInternalUnwrapped[HomogeneousExpr]], ParsleyInternalUnwrapped[HomogeneousExpr]](atoms.map(_.generate))
            opInternals <- Gen.sequence[List[(Fixity, List[ParsleyInternalUnwrapped[Any]])], (Fixity, List[ParsleyInternalUnwrapped[Any]])](ops.map { case (f, p) => Gen.sequence[List[ParsleyInternalUnwrapped[Any]], ParsleyInternalUnwrapped[Any]](p.map(_.generate)).map(f -> _) })
            internal <- Gen.listOfN(n, generateExpr(ops.length + 1, atomInternals, opInternals).map { case (input, output) => ParsleyInternalUnwrapped.HomogeneousPrecedence(atomInternals, opInternals, input, output) })
        } yield internal

        override def generateInvalid(n: Int, lastFailed: Boolean): Gen[(List[ParsleyInternalUnwrapped[HomogeneousExpr]], Boolean)] = for {
            atomInternals <- Gen.sequence[List[ParsleyInternalUnwrapped[HomogeneousExpr]], ParsleyInternalUnwrapped[HomogeneousExpr]](atoms.map(_.generateInvalid(lastFailed)))
            opInternals <- Gen.sequence[List[(Fixity, List[ParsleyInternalUnwrapped[Any]])], (Fixity, List[ParsleyInternalUnwrapped[Any]])](ops.map { case (f, p) => Gen.sequence[List[ParsleyInternalUnwrapped[Any]], ParsleyInternalUnwrapped[Any]](p.map(_.generateInvalid(lastFailed))).map(f -> _) })
            internal <- Gen.listOfN(n, generateExpr(ops.length + 1, atomInternals, opInternals).map { case (input, output) => ParsleyInternalUnwrapped.HomogeneousPrecedence(atomInternals, opInternals, input, output) })
        } yield (internal, lastFailed)

        @nowarn("cat=deprecation") override def shrink: Stream[ParsleyInternal[HomogeneousExpr]] = {
            val s1 = for (shrinkAtoms <- Shrink.shrinkContainer[List, ParsleyInternal[HomogeneousExpr]].suchThat(!_.isEmpty).shrink(atoms)) yield HomogeneousPrecedence(shrinkAtoms, ops)
            val s2 = Shrink.shrink(atoms).flatten

            s1 ++ s2
        }

        @nowarn("cat=deprecation") override def shrinkAny: Stream[ParsleyInternal[Any]] = {
            val s1 = Shrink.shrink(ops.flatMap { case (_, p) => p }).flatten
            
            shrink ++ s1
        }

        // FIXME: Implement a full precedence parser :(
        // In other words, see if a full-blown interpreter can be sidestepped
        // whilst replacing its current usage, mainly in the <|> combinator
        override def satisfies(input: String): (Option[HomogeneousExpr], String) = (None, "")

        // Assumes both atoms and prefix operators take input
        override def possibleNext: Either[List[Char], HomogeneousExpr] = Left(atoms.flatMap(_.possibleNext.swap.getOrElse(List())) ++ ops.collect { case (Prefix, ps) => ps.flatMap(_.possibleNext.swap.getOrElse(List())) }.flatten)

        override def correct(forbidden: Set[Char]): Either[Unit, Set[Char]] = (atoms.map(_.correct(forbidden)) ++ ops.flatMap { case (_, os) => os }.map(_.correct(forbidden))).reduceRight[Either[Unit, Set[Char]]] { case (l, r) => r.flatMap(rf => l.map(lf => lf ++ rf)) }

        override def correctInvalid(lastFailed: Boolean): Boolean = atoms.forall(_.correctInvalid(lastFailed)) && ops.forall { case (_, ps) => ps.forall(_.correctInvalid(lastFailed)) }
    }
}
