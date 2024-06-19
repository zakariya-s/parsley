package parsley.fuzzing.lexer

import org.scalacheck.Gen

import parsley.token.descriptions.LexicalDesc
import parsley.token.descriptions.text._
import parsley.fuzzing.lexer.NumericGen.NumberBase

object CharacterGen {
    private def codepointToChar(codepoint: Int): Char = new String(Character.toChars(codepoint)).charAt(0)

    private def convertCodepoint(codepoint: Int, lexer: LexicalDesc, prefix: Option[Char], numberBase: NumberBase): (String, Char) = {
        val char = codepointToChar(codepoint)
        val escBegin = lexer.textDesc.escapeSequences.escBegin.toString
        val str = escBegin + prefix.map(_.toString).getOrElse("") + Integer.toString(codepoint, numberBase.base)

        (str, char)
    }

    private def genEscapeCharacter(lexer: LexicalDesc, numericEscape: NumericEscape, numberBase: NumberBase): Option[Gen[(String, Char)]] = numericEscape match {
        case NumericEscape.Supported(prefix, NumberOfDigits.AtMost(n), maxValue) => Some({
            val maximum = math.min(math.pow(numberBase.base.toDouble, n.toDouble).toInt - 1, maxValue)
            for {
                codepoint <- Gen.choose(0, maximum)
            } yield convertCodepoint(codepoint, lexer, prefix, numberBase)
        })
        case NumericEscape.Supported(prefix, NumberOfDigits.Exactly(n, ns), maxValue) => Some({
            val base = numberBase.base
            for {
                digits <- Gen.oneOf(n, ns)
                minimum = math.pow(base.toDouble, (digits - 1).toDouble).toInt
                maximum = math.min(math.pow(base.toDouble, digits.toDouble).toInt - 1, maxValue)
                codepoint <- Gen.choose(minimum, maximum)
            } yield convertCodepoint(codepoint, lexer, prefix, numberBase)
        })
        case NumericEscape.Supported(prefix, NumberOfDigits.Unbounded, maxValue) => Some({
            for {
                codepoint <- Gen.choose(0, maxValue)
            } yield convertCodepoint(codepoint, lexer, prefix, numberBase)
        })
        case NumericEscape.Illegal => None
    }

    def genCharacter(lexer: LexicalDesc): Gen[(String, Char)] = {
        val textDesc = lexer.textDesc
        val escapeDesc = textDesc.escapeSequences

        val escBegin = escapeDesc.escBegin.toString
        
        val graphicChar = NameGen.genCharFromPredicate(lexer, textDesc.graphicCharacter).map(c => (c.toString, c))
        val literalChar: Option[Gen[(String, Char)]] = escapeDesc.literals.headOption.map(_ => Gen.oneOf(escapeDesc.literals.toSeq).map(c => (escBegin + c, c)))
        val mappingChar: Option[Gen[(String, Char)]] = escapeDesc.mapping.headOption.map { case (str, codepoint) =>
            // We take the first character as we limit ourselves to basic chars
            // (i.e. no Unicode magic with surrogate pairs and graphemes...)
            (escBegin + str, codepointToChar(codepoint))
        }

        // Only consider generators which are possible for the given lexical description
        val gens: List[Gen[(String, Char)]] = List(
            literalChar,
            mappingChar,
            genEscapeCharacter(lexer, escapeDesc.decimalEscape, NumberBase.Decimal),
            genEscapeCharacter(lexer, escapeDesc.hexadecimalEscape, NumberBase.Hexadecimal),
            genEscapeCharacter(lexer, escapeDesc.octalEscape, NumberBase.Octal),
            genEscapeCharacter(lexer, escapeDesc.binaryEscape, NumberBase.Binary)
        ).flatten

        val charLiteralEnd = textDesc.characterLiteralEnd.toString
        Gen.oneOf(graphicChar, graphicChar, gens: _*).map { case (input, output) => (charLiteralEnd + input + charLiteralEnd, output) }
    }
}
