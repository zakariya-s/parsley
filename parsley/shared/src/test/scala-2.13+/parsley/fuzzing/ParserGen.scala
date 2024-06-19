package parsley.fuzzing

import parsley.expr.{Fixity, InfixL, InfixN, InfixR, Postfix, Prefix}

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

import ParsleyInternal._

abstract class ParserGen[T: Arbitrary: PrettyPrint] {
    import ParserGen._

    def genLeaf(genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[T], Boolean, Set[Char])]

    def genThen(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[T], Boolean, Set[Char])] = for {
        (left, leftFailed, lf) <- sizedParserAny(size / 2, genInvalidParser, lastFailed, forbidden)
        (right, rightFailed, rf) <- sizedParser(size / 2, genInvalidParser, leftFailed, lf)
    } yield (Then(left, right), rightFailed, rf)

    def genThenDiscard(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[T], Boolean, Set[Char])] = for {
        (left, leftFailed, lf) <- sizedParser(size / 2, genInvalidParser, lastFailed, forbidden)
        (right, rightFailed, rf) <- sizedParserAny(size / 2, genInvalidParser, leftFailed, lf)
    } yield (ThenDiscard(left, right), rightFailed, rf)

    def genOr(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[T], Boolean, Set[Char])] = for {
        (left, leftFailed, lf) <- sizedParser(size / 2, genInvalidParser, lastFailed, forbidden)
        (right, rightFailed, rf) <- sizedParser(size / 2, genInvalidParser, lastFailed, forbidden)
        leftAtomic <- Gen.oneOf(Atomic(left), left)
        node = Or(leftAtomic, right)
    } yield (node, leftFailed && rightFailed, lf ++ rf ++ left.possibleNext.swap.getOrElse(List()))

    def genMap(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[T], Boolean, Set[Char])] = genArbMap[T](size, genInvalidParser, lastFailed, forbidden)(implicitly[Arbitrary[T]], this, PrettyPrint[T])

    def genLookAhead(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[T], Boolean, Set[Char])] = sizedParser(size / 2, genInvalidParser, lastFailed, forbidden).map { case (p, pFailed, f) => (LookAhead(p), pFailed, f) }

    def genIfS(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[T], Boolean, Set[Char])] = for {
        (condP, condFailed, cf) <- ParserGen[Boolean].sizedParser(size / 3, genInvalidParser, lastFailed, forbidden)
        (thenP, thenFailed, tf) <- sizedParser(size / 3, genInvalidParser, condFailed, cf)
        (elseP, elseFailed, ef) <- sizedParser(size / 3, genInvalidParser, condFailed, cf)
    } yield (IfS(condP, thenP, elseP), thenFailed && elseFailed, tf ++ ef)

    def genAp(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[T], Boolean, Set[Char])] = genArbAp[T](size, genInvalidParser, lastFailed, forbidden)(implicitly[Arbitrary[T]], this, PrettyPrint[T])

    def genReverseAp(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[T], Boolean, Set[Char])] = genArbReverseAp[T](size, genInvalidParser, lastFailed, forbidden)(implicitly[Arbitrary[T]], this, PrettyPrint[T])

    def genBranch(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[T], Boolean, Set[Char])] = genArbBranch[T](size, genInvalidParser, lastFailed, forbidden)

    def genFilter(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[T], Boolean, Set[Char])] = for {
        (parser, parserFailed, f) <- sizedParser(size / 2, genInvalidParser, lastFailed, forbidden)
    } yield (Filter(parser), true, f)

    def genAtomic(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[T], Boolean, Set[Char])] = for {
        (parser, parserFailed, f) <- sizedParser(size / 2, genInvalidParser, lastFailed, forbidden)
    } yield (Atomic(parser), parserFailed, f)

    def genReduceLeft(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[T], Boolean, Set[Char])] = for {
        (parser, parserFailed, f) <- sizedParser(size / 2, genInvalidParser, lastFailed, forbidden).suchThat(_._1.possibleNext.isLeft)
        func <- Arbitrary.arbitrary[ParsleyFunction[(T, T), T]]
        node = ReduceLeft(parser, func)
    } yield (node, parserFailed, f ++ parser.possibleNext.swap.getOrElse(List()))

    def genReduceRight(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[T], Boolean, Set[Char])] = for {
        (parser, parserFailed, f) <- sizedParser(size / 2, genInvalidParser, lastFailed, forbidden).suchThat(_._1.possibleNext.isLeft)
        func <- Arbitrary.arbitrary[ParsleyFunction[(T, T), T]]
        node = ReduceLeft(parser, func)
    } yield (node, parserFailed, f ++ parser.possibleNext.swap.getOrElse(List()))

    def genFlatMap(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[T], Boolean, Set[Char])] = genArbFlatMap[T](size, genInvalidParser, lastFailed, forbidden)(implicitly[Arbitrary[T]], this, PrettyPrint[T])

    def genSelect(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[T], Boolean, Set[Char])] = genArbSelect[T](size, genInvalidParser, lastFailed, forbidden)(implicitly[Arbitrary[T]], this, PrettyPrint[T])

    def genCollect(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[T], Boolean, Set[Char])] = genArbCollect[T](size, genInvalidParser, lastFailed, forbidden)(implicitly[Arbitrary[T]], this, PrettyPrint[T])

    def genMapFilter(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[T], Boolean, Set[Char])] = genArbMapFilter[T](size, genInvalidParser, lastFailed, forbidden)(implicitly[Arbitrary[T]], this, PrettyPrint[T])

    def genFoldLeft(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[T], Boolean, Set[Char])] = genArbFoldLeft[T](size, genInvalidParser, lastFailed, forbidden)(implicitly[Arbitrary[T]], this, PrettyPrint[T])

    def genFoldLeft1(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[T], Boolean, Set[Char])] = genArbFoldLeft1[T](size, genInvalidParser, lastFailed, forbidden)(implicitly[Arbitrary[T]], this, PrettyPrint[T])

    def genFoldRight(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[T], Boolean, Set[Char])] = genArbFoldRight[T](size, genInvalidParser, lastFailed, forbidden)(implicitly[Arbitrary[T]], this, PrettyPrint[T])

    def genFoldRight1(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[T], Boolean, Set[Char])] = genArbFoldRight1[T](size, genInvalidParser, lastFailed, forbidden)(implicitly[Arbitrary[T]], this, PrettyPrint[T])

    // Selection manager stuff
    // type ParserFunc = (Int, Boolean, Boolean, Set[Char]) => Gen[(ParsleyInternal[T], Boolean, Set[Char])]

    // def allGen: scala.collection.immutable.Map[Class[_ <: ParsleyInternal[T]], ParserFunc] = scala.collection.immutable.Map(
    //     classOf[ParsleyInternal.Then[T, Any]] -> genThen,
    //     classOf[ParsleyInternal.ThenDiscard[T, Any]] -> genThenDiscard,
    //     classOf[ParsleyInternal.Or[T]] -> genOr,
    //     classOf[ParsleyInternal.Map[Any, T]] -> genMap,
    // }

    def allGen = Seq(
        genThen,
        genThenDiscard,
        genOr,
        genMap,
        genLookAhead,
        genIfS,
        genAp,
        genReverseAp,
        genBranch,
        genAtomic,
        genFilter,
        /*genReduceLeft,
        genReduceRight,*/
        genFlatMap,
        genSelect,
        genCollect,
        genMapFilter,
        /*genFoldLeft,
        genFoldLeft1,
        genFoldRight,
        genFoldRight1*/)

    final def sizedParser(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[T], Boolean, Set[Char])] =
        if (size <= 0 && genInvalidParser && !lastFailed) {
            val node = Empty()
            Gen.const((node, true, forbidden))
        } else if (size <= 0) genLeaf(genInvalidParser, lastFailed, forbidden)
        else Gen.frequency(((1 -> genLeaf(genInvalidParser, lastFailed, forbidden)) +: (allGen.map(gen => 2 -> Gen.lzy(gen(size, genInvalidParser, lastFailed, forbidden))) ++ (if (genInvalidParser) List(2 -> Gen.const((Empty(), true, forbidden))) else List()))): _*)
}

object ParserGen {
    private implicit val arbChar: Arbitrary[Char] = Arbitrary(Gen.asciiPrintableChar)
    private implicit val arbString: Arbitrary[String] = Arbitrary(Gen.asciiPrintableStr)

    def apply[T](implicit ev: ParserGen[T]): ParserGen[T] = ev

    private implicit def parserHomogeneousExpr: ParserGen[HomogeneousExpr] = new ParserGen[HomogeneousExpr] {
        override def genLeaf(genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = Arbitrary.arbitrary[HomogeneousExpr].map(v => (Pure(v), lastFailed, forbidden))
        override def allGen = super.allGen ++ Seq(genHomogeneousPrecedence)

        private def genFixity: Gen[Fixity] = Gen.oneOf(
            InfixL,
            InfixN,
            InfixR,
            Prefix,
            Postfix
        )

        private def listOfAtoms(n: Int, size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char], firsts: Set[Char]): Gen[List[(ParsleyInternal[HomogeneousExpr], Boolean, Set[Char])]] = if (n <= 0) Gen.const(List()) else for {
            (p, pFailed, pForbidden) <- sizedParserAny(size, genInvalidParser, lastFailed, forbidden ++ firsts).suchThat(_._1.possibleNext.isLeft).map { case (p, pFailed, pForbidden) => (Map(p, ParsleyFunction.AtomExpression[Any]()), pFailed, pForbidden) }
            next <- listOfAtoms(n - 1, size, genInvalidParser, lastFailed, forbidden, firsts ++ p.possibleNext.swap.getOrElse(List()))
        } yield (p, pFailed, pForbidden) :: next

        private def listOfOps(n: Int, size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char], firsts: Set[Char]): Gen[List[(ParsleyInternal[Any], Boolean, Set[Char])]] = if (n <= 0) Gen.const(List()) else for {
            (p, pFailed, pForbidden) <- sizedParserAny(size, genInvalidParser, lastFailed, forbidden ++ firsts).suchThat(_._1.possibleNext.isLeft)
            next <- listOfOps(n - 1, size, genInvalidParser, lastFailed, forbidden, firsts ++ p.possibleNext.swap.getOrElse(List()))
        } yield (p, pFailed, pForbidden) :: next

        def genHomogeneousPrecedence(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[HomogeneousExpr], Boolean, Set[Char])] = for {
            numAtoms <- Gen.choose(1, 5)
            // atomsList <- Gen.listOfN(numAtoms, sizedParserAny(size / 2, genInvalidParser, lastFailed, forbidden).suchThat(_._1.possibleNext.isLeft).map { case (p, pFailed, pForbidden) => (Map(p, ParsleyFunction.AtomExpression[Any]()), pFailed, pForbidden) })
            atomsList <- listOfAtoms(numAtoms, size / 2, genInvalidParser, lastFailed, forbidden, Set())
            (atoms, atomsFailed, af) = atomsList.foldRight((List[ParsleyInternal[HomogeneousExpr]](), false, Set[Char]())) { case ((generatedA, tFailed, tf), (at, fail, forb)) => (generatedA :: at, fail && tFailed, forb ++ tf) }
            atomsFirsts = atoms.flatMap(_.possibleNext.swap.getOrElse(List())).toSet
            // opsListGen = Gen.choose(1, 3).flatMap(numParsersPerLevel => Gen.listOfN(numParsersPerLevel, sizedParserAny(size / 2, genInvalidParser, lastFailed, forbidden ++ af).suchThat(_._1.possibleNext.isLeft)))
            opsListGen = Gen.choose(1, 3).flatMap(numParsersPerLevel => listOfOps(numParsersPerLevel, size / 2, genInvalidParser, lastFailed, forbidden ++ af, atomsFirsts))
            opsGen = opsListGen.map(_.foldRight((List[ParsleyInternal[Any]](), false, Set[Char]())) { case ((generatedA, tFailed, tf), (at, fail, forb)) => (generatedA :: at, fail && tFailed, forb ++ tf) })
            parserLevelGen = Gen.zip(genFixity, opsGen).map { case (f, (ops, opsFailed, of)) => ((f, ops), opsFailed, of) }
            numLevels <- Gen.choose(1, 5)
            opsList <- Gen.listOfN(numLevels, parserLevelGen)
            (ops, opsFailed, of) = opsList.foldRight((List[(Fixity, List[ParsleyInternal[Any]])](), false, Set[Char]())) { case ((generatedO, tFailed, tf), (at, fail, forb)) => (generatedO :: at, fail && tFailed, forb ++ tf) }
            if ops.flatMap { case (_, os) => os }.forall(_.correct(forbidden ++ af ++ of).isRight)
            if atoms.forall(_.correct(forbidden ++ of).isRight)
            node = HomogeneousPrecedence(atoms, ops)
        } yield (node, atomsFailed && opsFailed, forbidden ++ af ++ of)
    }

    private implicit def parserEither[T: ParserGen: Arbitrary: PrettyPrint, U: ParserGen: Arbitrary: PrettyPrint]: ParserGen[Either[T, U]] = new ParserGen[Either[T, U]] {
        override def genLeaf(genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = Gen.oneOf(
            ParserGen[T].genLeaf(genInvalidParser, lastFailed, forbidden).map { case (n, nFailed, f) => (Map(n, ParsleyFunction.LeftFunction[T, U]()), nFailed, f) },
            ParserGen[U].genLeaf(genInvalidParser, lastFailed, forbidden).map { case (n, nFailed, f) => (Map(n, ParsleyFunction.RightFunction[T, U]()), nFailed, f) }
        )

        override def genOr(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = Gen.oneOf(
            Gen.lzy(super.genOr(size, genInvalidParser, lastFailed, forbidden)),
            Gen.lzy(for {
                (left, leftFailed, lf) <- ParserGen[T].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden)
                (right, rightFailed, rf) <- ParserGen[U].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden)
                node = Sum(left, right)
            } yield (node, leftFailed && rightFailed, lf ++ rf ++ left.possibleNext.swap.getOrElse(List())))
        )
    }

    private implicit def parserTuple[T: ParserGen: Arbitrary: PrettyPrint, U: ParserGen: Arbitrary: PrettyPrint]: ParserGen[(T, U)] = new ParserGen[(T, U)] {
        override def genLeaf(genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = for {
            left <- Arbitrary.arbitrary[T]
            right <- Arbitrary.arbitrary[U]
        } yield (Pure((left, right)), lastFailed, forbidden)
        override def allGen = super.allGen ++ Seq(genZip)

        def genZip(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = for {
            (left, leftFailed, lf) <- ParserGen[T].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden)
            (right, rightFailed, rf) <- ParserGen[U].sizedParser(size / 2, genInvalidParser, leftFailed, lf)
        } yield (Zip(left, right), rightFailed, rf)
    }

    private implicit def parserOption[T: ParserGen: Arbitrary: PrettyPrint]: ParserGen[Option[T]] = new ParserGen[Option[T]] {
        override def genLeaf(genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = Gen.oneOf(
            // ParserGen[T].genLeaf(forbidden).map { case (n, f) => (Map(n, Some.apply[T]), f) },
            ParserGen[T].genLeaf(genInvalidParser, lastFailed, forbidden).map { case (n, nFailed, f) => (Map(n, ParsleyFunction.SomeFunction[T]()), nFailed, forbidden ++ f) },
            Arbitrary.arbitrary[T].map(v => (Pure(Option.apply(v)), lastFailed, forbidden)),
            Gen.const((Pure(Option.empty[T]), lastFailed, forbidden))
        )
        override def allGen = super.allGen ++ Seq(genReduceOptionLeft, genReduceOptionRight)

        def genReduceOptionLeft(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[Option[T]], Boolean, Set[Char])] = for {
            (parser, parserFailed, f) <- ParserGen[T].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden).suchThat(_._1.possibleNext.isLeft)
            func <- Arbitrary.arbitrary[ParsleyFunction[(T, T), T]]
            node = ReduceOptionLeft(parser, func)
        } yield (node, parserFailed, forbidden ++ f ++ parser.possibleNext.swap.getOrElse(List()))

        def genReduceOptionRight(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[Option[T]], Boolean, Set[Char])] = for {
            (parser, parserFailed, f) <- ParserGen[T].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden).suchThat(_._1.possibleNext.isLeft)
            func <- Arbitrary.arbitrary[ParsleyFunction[(T, T), T]]
            node = ReduceOptionRight(parser, func)
        } yield (node, parserFailed, forbidden ++ f ++ parser.possibleNext.swap.getOrElse(List()))
    }

    private implicit def parserList[T: ParserGen: Arbitrary: PrettyPrint]: ParserGen[List[T]] = new ParserGen[List[T]] {
        override def genLeaf(genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = Gen.containerOf[List, T](Arbitrary.arbitrary[T]).map(list => (Pure(list), lastFailed, forbidden))
        override def allGen = super.allGen ++ Seq(genMany, genSome, genCons, genEndBy, genEndBy1, genSepBy, genSepBy1, genSepEndBy, genSepEndBy1)

        private def genMany(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[List[T]], Boolean, Set[Char])] =
            ParserGen[T].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden).suchThat(_._1.possibleNext.isLeft).suchThat { case (p, _, f) => p.correct(forbidden ++ f).isRight }.map { case (p, pFailed, f) =>
                val node = Many(p)
                (node, pFailed, forbidden ++ f ++ p.possibleNext.swap.getOrElse(List()))
            }

        private def genSome(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[List[T]], Boolean, Set[Char])] =
            ParserGen[T].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden).suchThat(_._1.possibleNext.isLeft).suchThat { case (p, _, f) => p.correct(forbidden ++ f).isRight }.map { case (p, pFailed, f) =>
                val node = PSome(p)
                (node, pFailed, f ++ p.possibleNext.swap.getOrElse(List()))
            }

        private def genCons(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[List[T]], Boolean, Set[Char])] = for {
            (elem, elemFailed, ef) <- ParserGen[T].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden)
            (list, listFailed, lf) <- sizedParser(size / 2, genInvalidParser, elemFailed, ef)
        } yield (Cons(elem, list), listFailed, lf)

        def genEndBy(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[List[T]], Boolean, Set[Char])] = for {
            (parser, parserFailed, f) <- ParserGen[T].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden).suchThat(_._1.possibleNext.isLeft)
            (sep, sepFailed, sf) <- sizedParserAny(size / 2, genInvalidParser, parserFailed, f).suchThat(_._1.possibleNext.isLeft)
            if parser.correct(forbidden ++ sf).isRight
            node = EndBy(parser, sep)
        } yield (node, sepFailed, forbidden ++ sf ++ parser.possibleNext.swap.getOrElse(List()))

        def genEndBy1(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[List[T]], Boolean, Set[Char])] = for {
            (parser, parserFailed, f) <- ParserGen[T].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden).suchThat(_._1.possibleNext.isLeft)
            (sep, sepFailed, sf) <- sizedParserAny(size / 2, genInvalidParser, parserFailed, f).suchThat(_._1.possibleNext.isLeft)
            if parser.correct(forbidden ++ sf).isRight
            node = EndBy1(parser, sep)
        } yield (node, sepFailed, sf ++ parser.possibleNext.swap.getOrElse(List()))

        def genSepBy(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[List[T]], Boolean, Set[Char])] = for {
            (parser, parserFailed, f) <- ParserGen[T].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden).suchThat(_._1.possibleNext.isLeft)
            (sep, sepFailed, sf) <- sizedParserAny(size / 2, genInvalidParser, parserFailed, f).suchThat(_._1.possibleNext.isLeft)
            if parser.correct(forbidden ++ sf).isRight
            node = SepBy(parser, sep)
        } yield (SepBy(parser, sep), sepFailed, forbidden ++ f ++ sep.possibleNext.swap.getOrElse(List()))

        def genSepBy1(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[List[T]], Boolean, Set[Char])] = for {
            (parser, parserFailed, f) <- ParserGen[T].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden).suchThat(_._1.possibleNext.isLeft)
            (sep, sepFailed, sf) <- sizedParserAny(size / 2, genInvalidParser, parserFailed, f).suchThat(_._1.possibleNext.isLeft)
            if parser.correct(forbidden ++ sf).isRight
            node = SepBy1(parser, sep)
        } yield (node, sepFailed, f ++ sep.possibleNext.swap.getOrElse(List()))

        def genSepEndBy(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[List[T]], Boolean, Set[Char])] = for {
            (parser, parserFailed, f) <- ParserGen[T].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden).suchThat(_._1.possibleNext.isLeft)
            (sep, sepFailed, sf) <- sizedParserAny(size / 2, genInvalidParser, parserFailed, f).suchThat(_._1.possibleNext.isLeft)
            if parser.correct(forbidden ++ sf).isRight
            node = SepEndBy(parser, sep)
        } yield (node, sepFailed, forbidden ++ f ++ sf ++ (parser.possibleNext.swap.getOrElse(List()) ++ sep.possibleNext.swap.getOrElse(List())))

        def genSepEndBy1(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[List[T]], Boolean, Set[Char])] = for {
            (parser, parserFailed, f) <- ParserGen[T].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden).suchThat(_._1.possibleNext.isLeft)
            (sep, sepFailed, sf) <- sizedParserAny(size / 2, genInvalidParser, parserFailed, f).suchThat(_._1.possibleNext.isLeft)
            if parser.correct(forbidden ++ sf).isRight
            node = SepEndBy1(parser, sep)
        } yield (node, sepFailed, f ++ sf ++ (parser.possibleNext.swap.getOrElse(List()) ++ sep.possibleNext.swap.getOrElse(List())))
    }

    private implicit def parserFunction[T, U](implicit ev: Arbitrary[ParsleyFunction[T, U]]): ParserGen[ParsleyFunction[T, U]] = new ParserGen[ParsleyFunction[T, U]] {
        override def genLeaf(genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[ParsleyFunction[T, U]], Boolean, Set[Char])] = Arbitrary.arbitrary[ParsleyFunction[T, U]].map(func => (Pure(func), lastFailed, forbidden))
    }

    private implicit val parserInt: ParserGen[Int] = new ParserGen[Int] {
        override def genLeaf(genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = Arbitrary.arbitrary[Int].map(i => (Pure(i), lastFailed, forbidden))
        override def allGen = super.allGen ++ Seq(genCountMany, genCountSome)

        private def genCountMany(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[Int], Boolean, Set[Char])] =
            sizedParserAny(size / 2, genInvalidParser, lastFailed, forbidden).suchThat(_._1.possibleNext.isLeft).suchThat { case (p, _, f) => p.correct(forbidden ++ f).isRight }.map { case (p, pFailed, f) =>
                val node = CountMany(p)
                (node, pFailed, forbidden ++ f ++ p.possibleNext.swap.getOrElse(List()))
            }

        private def genCountSome(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[Int], Boolean, Set[Char])] =
            sizedParserAny(size / 2, genInvalidParser, lastFailed, forbidden).suchThat(_._1.possibleNext.isLeft).suchThat { case (p, _, f) => p.correct(forbidden ++ f).isRight }.map { case (p, pFailed, f) =>
                val node = CountSome(p)
                (node, pFailed, forbidden ++ f ++ p.possibleNext.swap.getOrElse(List()))
            }
    }

    private implicit val parserChar: ParserGen[Char] = new ParserGen[Char] {
        override def genLeaf(genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = Gen.oneOf(
            Gen.asciiPrintableChar.suchThat(!forbidden.contains(_)).map { c =>
                val node = PChar(c)
                (node, true, Set[Char]())
            },
            Gen.asciiPrintableChar.map(c => (Pure(c), lastFailed, forbidden)),
            (if (genInvalidParser && !forbidden.isEmpty) List(Gen.oneOf(forbidden).map { c =>
                val node = PChar(c)
                (node, true, Set[Char]())
            })
            else List()): _*
        )
    }

    private implicit val parserString: ParserGen[String] = new ParserGen[String] {
        override def genLeaf(genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = Gen.oneOf(
            Gen.nonEmptyListOf[Char](Gen.asciiPrintableChar).map(_.mkString).suchThat(str => !forbidden.contains(str(0))).map { str =>
                val node = PString(str)
                (node, true, Set[Char]())
            },
            Gen.listOf[Char](Gen.asciiPrintableChar).map(_.mkString).map(str => (Pure(str), lastFailed, forbidden)),
            (if (genInvalidParser && !forbidden.isEmpty) List(Gen.zip(Gen.oneOf(forbidden), Gen.nonEmptyListOf[Char](Gen.asciiPrintableChar).map(_.mkString)).map { case (c, str) =>
                val node = PString(s"$c$str")
                (node, true, Set[Char]())
            })
            else List()): _*
        )
    }

    private implicit val parserUnit: ParserGen[Unit] = new ParserGen[Unit] {
        override def genLeaf(genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = Gen.const((Pure(()), lastFailed, forbidden))
        override def allGen = super.allGen ++ Seq(genNotFollowedBy, genOptional)

        private def genNotFollowedBy(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[Unit], Boolean, Set[Char])] =
            // FIXME: Think about `many`: it returns possibleNext with a Left value, even though it may consume nothing...
            sizedParserAny(size / 2, false, lastFailed, forbidden).suchThat(genInvalidParser || _._1.satisfies("")._1.isEmpty).map { case (p, _, _) =>
                val node = NotFollowedBy(p)
                (node, lastFailed, forbidden ++ p.possibleNext.swap.getOrElse(List()))
            }

        private def genOptional(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[Unit], Boolean, Set[Char])] =
            sizedParserAny(size / 2, genInvalidParser, lastFailed, forbidden).map { case (p, pFailed, f) =>
                val node = POptional(p)
                (node, pFailed, f ++ forbidden ++ p.possibleNext.swap.getOrElse(List()))
            }
    }

    private implicit val parserBoolean: ParserGen[Boolean] = new ParserGen[Boolean] {
        override def genLeaf(genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = Arbitrary.arbitrary[Boolean].map(b => (Pure(b), lastFailed, forbidden))
    }

    private trait GenArbFunc {
        def apply[T: Arbitrary: ParserGen: PrettyPrint, U: Arbitrary: ParserGen: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[T], Boolean, Set[Char])]
    }

    private def genArbNode[T: Arbitrary: ParserGen: PrettyPrint](genArbFunc: GenArbFunc)(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = Gen.oneOf(
        genArbFunc.apply[T, Int],
        genArbFunc.apply[T, Char],
        genArbFunc.apply[T, String],
        genArbFunc.apply[T, Unit],
        genArbFunc.apply[T, Boolean]
    ).flatMap(f => f(size, genInvalidParser, lastFailed, forbidden))

    private val genArbFlatMapFunc = new GenArbFunc {
        override def apply[T: Arbitrary: ParserGen: PrettyPrint, U: Arbitrary: ParserGen: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = for {
            (parser, parserFailed, pf) <- ParserGen[U].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden)
            elems <- Gen.choose(1, 10)
            mappingList <- Gen.listOfN(elems, Gen.zip(Arbitrary.arbitrary[U], ParserGen[T].sizedParser(size / 2, genInvalidParser, parserFailed, pf)))
            (mapping, fail, forb) = mappingList.foldRight((scala.collection.immutable.Map[U, ParsleyInternal[T]](), true, Set[Char]())) { case ((generatedU, (generatedT, tFailed, tf)), (mp, fail, forb)) => (mp + ((generatedU, generatedT)), fail && tFailed, forb ++ tf) }
            (default, dFailed, df) <- ParserGen[T].sizedParser(size / 2, genInvalidParser, parserFailed, pf)
            func = ParsleyFunction.UnwrappedFunction(mapping, default)
        } yield (FlatMap(parser, func), fail && dFailed, forb ++ df)
    }

    private val genArbSelectFunc = new GenArbFunc {
        override def apply[T: Arbitrary: ParserGen: PrettyPrint, U: Arbitrary: ParserGen: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = for {
            (parser, parserFailed, pf) <- ParserGen[Either[U, T]].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden)
            (func, _, ff) <- ParserGen[ParsleyFunction[U, T]].sizedParser(size / 2, genInvalidParser, parserFailed, pf)
        } yield (Select(parser, func), parserFailed, pf ++ ff)
    }

    private val genArbCollectFunc = new GenArbFunc {
        override def apply[T: Arbitrary: ParserGen: PrettyPrint, U: Arbitrary: ParserGen: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = for {
            (parser, parserFailed, pf) <- ParserGen[U].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden)
            res <- Arbitrary.arbitrary[T]
        } yield (Collect(parser, res), parserFailed, pf)
    }

    private val genArbMapFilterFunc = new GenArbFunc {
        override def apply[T: Arbitrary: ParserGen: PrettyPrint, U: Arbitrary: ParserGen: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = for {
            (parser, parserFailed, pf) <- ParserGen[U].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden)
            res <- Arbitrary.arbitrary[T]
        } yield (MapFilter(parser, res), parserFailed, pf)
    }

    private val genArbFoldLeftFunc = new GenArbFunc {
        override def apply[T: Arbitrary: ParserGen: PrettyPrint, U: Arbitrary: ParserGen: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = for {
            (parser, parserFailed, pf) <- ParserGen[U].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden).suchThat(_._1.possibleNext.isLeft)
            initialValue <- Arbitrary.arbitrary[T]
            func <- Arbitrary.arbitrary[ParsleyFunction[(T, U), T]]
            node = FoldLeft(parser, initialValue, func)
        } yield (node, parserFailed, forbidden ++ pf ++ parser.possibleNext.swap.getOrElse(List()))
    }

    private val genArbFoldLeft1Func = new GenArbFunc {
        override def apply[T: Arbitrary: ParserGen: PrettyPrint, U: Arbitrary: ParserGen: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = for {
            (parser, parserFailed, pf) <- ParserGen[U].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden).suchThat(_._1.possibleNext.isLeft)
            initialValue <- Arbitrary.arbitrary[T]
            func <- Arbitrary.arbitrary[ParsleyFunction[(T, U), T]]
            node = FoldLeft1(parser, initialValue, func)
        } yield (node, parserFailed, pf ++ parser.possibleNext.swap.getOrElse(List()))
    }

    private val genArbFoldRightFunc = new GenArbFunc {
        override def apply[T: Arbitrary: ParserGen: PrettyPrint, U: Arbitrary: ParserGen: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = for {
            (parser, parserFailed, pf) <- ParserGen[U].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden).suchThat(_._1.possibleNext.isLeft)
            initialValue <- Arbitrary.arbitrary[T]
            func <- Arbitrary.arbitrary[ParsleyFunction[(U, T), T]]
            node = FoldRight(parser, initialValue, func)
        } yield (node, parserFailed, forbidden ++ pf ++ parser.possibleNext.swap.getOrElse(List()))
    }

    private val genArbFoldRight1Func = new GenArbFunc {
        override def apply[T: Arbitrary: ParserGen: PrettyPrint, U: Arbitrary: ParserGen: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = for {
            (parser, parserFailed, pf) <- ParserGen[U].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden).suchThat(_._1.possibleNext.isLeft)
            initialValue <- Arbitrary.arbitrary[T]
            func <- Arbitrary.arbitrary[ParsleyFunction[(U, T), T]]
            node = FoldRight1(parser, initialValue, func)
        } yield (node, parserFailed, pf ++ parser.possibleNext.swap.getOrElse(List()))
    }

    private val genArbMapFunc = new GenArbFunc {
        override def apply[T: Arbitrary: ParserGen: PrettyPrint, U: Arbitrary: ParserGen: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = for {
            (parser, parserFailed, pf) <- ParserGen[U].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden)
            func <- Arbitrary.arbitrary[ParsleyFunction[U, T]]
        } yield (Map(parser, func), parserFailed, pf)
    }

    private val genArbApFunc = new GenArbFunc {
        override def apply[T: Arbitrary: ParserGen: PrettyPrint, U: Arbitrary: ParserGen: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = for {
            (funcParser, funcFailed, ff) <- ParserGen[ParsleyFunction[U, T]].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden)
            (argParser, argFailed, af) <- ParserGen[U].sizedParser(size / 2, genInvalidParser, funcFailed, ff)
        } yield (Ap(funcParser, argParser), argFailed, af)
    }

    private val genArbReverseApFunc = new GenArbFunc {
        override def apply[T: Arbitrary: ParserGen: PrettyPrint, U: Arbitrary: ParserGen: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = for {
            (argParser, argFailed, af) <- ParserGen[U].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden)
            (funcParser, funcFailed, ff) <- ParserGen[ParsleyFunction[U, T]].sizedParser(size / 2, genInvalidParser, argFailed, af)
        } yield (ReverseAp(argParser, funcParser), funcFailed, ff)
    }

    private def genArbFlatMap[T: Arbitrary: ParserGen: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = genArbNode[T](genArbFlatMapFunc)(size, genInvalidParser, lastFailed, forbidden)
    private def genArbSelect[T: Arbitrary: ParserGen: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = genArbNode[T](genArbSelectFunc)(size, genInvalidParser, lastFailed, forbidden)
    private def genArbCollect[T: Arbitrary: ParserGen: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = genArbNode[T](genArbCollectFunc)(size, genInvalidParser, lastFailed, forbidden)
    private def genArbMapFilter[T: Arbitrary: ParserGen: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = genArbNode[T](genArbMapFilterFunc)(size, genInvalidParser, lastFailed, forbidden)
    private def genArbFoldLeft[T: Arbitrary: ParserGen: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = genArbNode[T](genArbFoldLeftFunc)(size, genInvalidParser, lastFailed, forbidden)
    private def genArbFoldLeft1[T: Arbitrary: ParserGen: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = genArbNode[T](genArbFoldLeft1Func)(size, genInvalidParser, lastFailed, forbidden)
    private def genArbFoldRight[T: Arbitrary: ParserGen: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = genArbNode[T](genArbFoldRightFunc)(size, genInvalidParser, lastFailed, forbidden)
    private def genArbFoldRight1[T: Arbitrary: ParserGen: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = genArbNode[T](genArbFoldRight1Func)(size, genInvalidParser, lastFailed, forbidden)
    private def genArbMap[T: Arbitrary: ParserGen: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = genArbNode[T](genArbMapFunc)(size, genInvalidParser, lastFailed, forbidden)
    private def genArbAp[T: Arbitrary: ParserGen: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = genArbNode[T](genArbApFunc)(size, genInvalidParser, lastFailed, forbidden)
    private def genArbReverseAp[T: Arbitrary: ParserGen: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = genArbNode[T](genArbReverseApFunc)(size, genInvalidParser, lastFailed, forbidden)

    private def genArbBranch[T: PrettyPrint, U: ParserGen: Arbitrary: PrettyPrint, V: ParserGen: Arbitrary: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char])(implicit ev1: Arbitrary[ParsleyFunction[U, T]], ev2: Arbitrary[ParsleyFunction[V, T]]) = for {
        (eitherParser, eitherFailed, ef) <- ParserGen[Either[U, V]].sizedParser(size / 2, genInvalidParser, lastFailed, forbidden)
        (leftParser, leftFailed, lf) <- ParserGen[ParsleyFunction[U, T]].sizedParser(size / 2, genInvalidParser, eitherFailed, ef)
        (rightParser, rightFailed, rf) <- ParserGen[ParsleyFunction[V, T]].sizedParser(size / 2, genInvalidParser, eitherFailed, ef)
    } yield (Branch(eitherParser, leftParser, rightParser), leftFailed && rightFailed, lf ++ rf)

    private def genArbBranch[T: Arbitrary: PrettyPrint](size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]) = Gen.oneOf(
        genArbBranch[T, Int, Char],
        genArbBranch[T, Char, String],
        genArbBranch[T, String, Unit],
        genArbBranch[T, Unit, Boolean],
        genArbBranch[T, Boolean, Int]
    ).flatMap(f => f(size, genInvalidParser, lastFailed, forbidden))

    private def parsersWithoutFuncs = Seq(
        ParserGen[HomogeneousExpr],
        ParserGen[Int],
        ParserGen[Char],
        ParserGen[String],
        ParserGen[Unit],
        ParserGen[Boolean],
        ParserGen[Either[Int, Int]],
        ParserGen[Option[Int]],
        ParserGen[List[Int]],
        ParserGen[(Int, Int)]
    )

    // private def parsersWithFuncs = parsersWithoutFuncs ++ Seq(
    //     ParserGen[Int => Int]
    // )

    private def parsersWithFuncs = parsersWithoutFuncs ++ Seq(
        ParserGen[ParsleyFunction[Int, Int]]
    )

    private def sizedParserAny(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[Any], Boolean, Set[Char])] = Gen.oneOf(
        parsersWithFuncs
    ).flatMap(_.sizedParser(size, genInvalidParser, lastFailed, forbidden))

    // Cannot compare functions (otherwise we would have solved the Halting problem), so ignore
    // parsers which have a function as its final result
    // FIXME: No longer an issue due to ParsleyFunction as opposed to Scala-provided total functions
    private def sizedParserAnyNoFunctions(size: Int, genInvalidParser: Boolean, lastFailed: Boolean, forbidden: Set[Char]): Gen[(ParsleyInternal[Any], Boolean, Set[Char])] = Gen.oneOf(
        parsersWithoutFuncs
    ).flatMap(_.sizedParser(size, genInvalidParser, lastFailed, forbidden))

    val genParser: Gen[ParsleyInternal[Any]] = Gen.sized(sizedParserAnyNoFunctions(_, false, false, Set())).map(_._1)

    // Generates parsers which allow for invalid constructs (i.e. `empty` and ignoring forbidden sets)
    val genInvalidParser: Gen[(ParsleyInternal[Any], Boolean)] = Gen.sized(sizedParserAnyNoFunctions(_, true, false, Set())).suchThat(_._2).map { case (parser, lastFailed, _) => (parser, lastFailed) }

    // Parser laws for evaluation

    // atomic(p *> q) <|> (r *> q) === (atomic(p) <|> r) *> q
    def parserLaw2(genInvalidParser: Boolean): Gen[(ParsleyInternal[Any], Boolean)] = Gen.sized { size => for {
        (p, pFailed, pForbidden) <- sizedParserAny(size / 2, genInvalidParser, false, Set())
        (r, rFailed, rForbidden) <- sizedParserAny(size / 2, genInvalidParser, false, Set())
        (q, qFailed, qForbidden) <- sizedParserAny(size / 2, genInvalidParser, pFailed && rFailed, pForbidden ++ rForbidden)
        node = Or(Atomic(Then(p, q)(q.evPrettyPrint))(q.evPrettyPrint), Then(r, q)(q.evPrettyPrint))(q.evPrettyPrint)
    } yield (node, qFailed) }

    // (p <|> pure(f)) <*> q ==> (p <*> q) <|> (f <$> q)
    def parserLaw1LhsToRhsImpl[T: Arbitrary: PrettyPrint, U: Arbitrary: PrettyPrint: ParserGen](genInvalidParser: Boolean): Gen[(ParsleyInternal[T], Boolean)] = Gen.sized { size => for {
        (p, pFailed, pForbidden) <- ParserGen[ParsleyFunction[U, T]].sizedParser(size / 2, genInvalidParser, false, Set())
        (q, qFailed, qForbidden) <- ParserGen[U].sizedParser(size / 2, genInvalidParser, false, pForbidden)
        f <- Arbitrary.arbitrary[ParsleyFunction[U, T]]
        node = Ap(Or(p, Pure(f)), q)
    } yield (node, qFailed) }

    // (p <|> pure(f)) <*> q ==> (p <*> q) <|> (f <$> q)
    def parserLaw1RhsToLhsImpl[T: Arbitrary: PrettyPrint, U: Arbitrary: PrettyPrint: ParserGen](genInvalidParser: Boolean): Gen[(ParsleyInternal[T], Boolean)] = Gen.sized { size => for {
        (p, pFailed, pForbidden) <- ParserGen[ParsleyFunction[U, T]].sizedParser(size / 2, genInvalidParser, false, Set())
        (q, qFailed, qForbidden) <- ParserGen[U].sizedParser(size / 2, genInvalidParser, false, pForbidden)
        f <- Arbitrary.arbitrary[ParsleyFunction[U, T]]
        node = Or(Ap(p, q), Map(q, f))
    } yield (node, qFailed) }

    def parserLaw1LhsToRhs(genInvalidParser: Boolean): Gen[(ParsleyInternal[Any], Boolean)] = Gen.oneOf(
        Gen.lzy(parserLaw1LhsToRhsImpl[Int, Char](genInvalidParser)),
        Gen.lzy(parserLaw1LhsToRhsImpl[Char, String](genInvalidParser)),
        Gen.lzy(parserLaw1LhsToRhsImpl[String, Unit](genInvalidParser)),
        Gen.lzy(parserLaw1LhsToRhsImpl[Unit, Boolean](genInvalidParser)),
        Gen.lzy(parserLaw1LhsToRhsImpl[Boolean, Int](genInvalidParser))
    )

    def parserLaw1RhsToLhs(genInvalidParser: Boolean): Gen[(ParsleyInternal[Any], Boolean)] = Gen.oneOf(
        Gen.lzy(parserLaw1RhsToLhsImpl[Int, Char](genInvalidParser)),
        Gen.lzy(parserLaw1RhsToLhsImpl[Char, String](genInvalidParser)),
        Gen.lzy(parserLaw1RhsToLhsImpl[String, Unit](genInvalidParser)),
        Gen.lzy(parserLaw1RhsToLhsImpl[Unit, Boolean](genInvalidParser)),
        Gen.lzy(parserLaw1RhsToLhsImpl[Boolean, Int](genInvalidParser))
    )
}
