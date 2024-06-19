package parsley.fuzzing.lexer

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

import parsley.token.predicate._
import parsley.token.descriptions.LexicalDesc

object NameGen {
    private [lexer] def genCharFromPredicate(lexer: LexicalDesc, charPredicate: CharPredicate): Gen[Char] = (charPredicate, lexer.spaceDesc.space) match {
        case (Basic(predicate), Basic(spacePredicate)) => Arbitrary.arbitrary[Char].suchThat(c => !spacePredicate(c) && predicate(c))
        case (NotRequired, Basic(spacePredicate)) => Arbitrary.arbitrary[Char].suchThat(c => !spacePredicate(c))
        case _ => Gen.const('\u0000')
    }

    private def combineCharPredicates(charPredicate1: CharPredicate, charPredicate2: CharPredicate) = (charPredicate1, charPredicate2) match {
        case (Basic(predicate1), Basic(predicate2)) => Basic(c => predicate1(c) && predicate2(c))
        case (NotRequired, Basic(predicate2)) => Basic(predicate2)
        case (Basic(predicate1), NotRequired) => Basic(predicate1)
        case (NotRequired, NotRequired) => NotRequired
    }

    def genIdentifier(lexer: LexicalDesc, startChar: Option[CharPredicate]): Gen[String] = {
        val symbolDesc = lexer.symbolDesc
        val startPredicate = startChar match {
            case Some(predicate) => combineCharPredicates(predicate, lexer.nameDesc.identifierStart)
            case None => lexer.nameDesc.identifierStart
        }

        for {
            startStr <- genCharFromPredicate(lexer, startPredicate)
            endStr <- Gen.nonEmptyListOf[Char](genCharFromPredicate(lexer, lexer.nameDesc.identifierLetter)).map(_.mkString)
            identifier = s"$startStr$endStr"
            if !symbolDesc.hardKeywords.contains(identifier)
        } yield identifier
    }

    def genUserDefinedOperator(lexer: LexicalDesc, startEndChar: Option[(CharPredicate, CharPredicate)]): Gen[String] = {
        val symbolDesc = lexer.symbolDesc
        val (startPredicate, endPredicate) = startEndChar match {
            case Some((startCharPredicate, endCharPredicate)) => (combineCharPredicates(startCharPredicate, lexer.nameDesc.operatorStart), combineCharPredicates(endCharPredicate, lexer.nameDesc.operatorLetter))
            case None => (lexer.nameDesc.operatorStart, lexer.nameDesc.operatorLetter)
        }

        for {
            startStr <- genCharFromPredicate(lexer, startPredicate)
            middleStr <- Gen.listOf[Char](genCharFromPredicate(lexer, lexer.nameDesc.operatorLetter)).map(_.mkString)
            endStr <- genCharFromPredicate(lexer, endPredicate)
            userDefinedOperator = s"$startStr$middleStr$endStr"
            if !symbolDesc.hardOperators.contains(userDefinedOperator)
        } yield userDefinedOperator
    }
}
