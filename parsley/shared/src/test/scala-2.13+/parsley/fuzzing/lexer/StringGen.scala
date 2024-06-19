package parsley.fuzzing.lexer

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

import parsley.token.predicate._
import parsley.token.descriptions.LexicalDesc

object StringGen {
    private def genStringGaps(lexer: LexicalDesc): Gen[String] = {
        val escBegin = lexer.textDesc.escapeSequences.escBegin.toString
        val stringGap = if (lexer.textDesc.escapeSequences.gapsSupported) {
            lexer.spaceDesc.space match {
                case Basic(spacePredicate) => Gen.stringOf(Arbitrary.arbitrary[Char].suchThat(spacePredicate)).map(escBegin + _ + escBegin)
                case _ => Gen.const("")
            }
        } else Gen.const("")

        Gen.oneOf(
            Gen.const(""),
            stringGap
        )
    }

    def genString(lexer: LexicalDesc): Gen[(String, String)] = {
        Gen.listOf[((String, Char), String)](Gen.zip(CharacterGen.genCharacter(lexer), genStringGaps(lexer))).map { lst =>
            val (strs, chrs): (List[String], List[Char]) = lst.map { case ((str, c), gap) => (str + gap, c) }.unzip
            (strs.mkString, chrs.mkString)
        }
    }
}
