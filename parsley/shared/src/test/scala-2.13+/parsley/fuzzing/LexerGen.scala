package parsley.fuzzing

import org.scalacheck.Gen
import org.scalacheck.Arbitrary

import parsley.token.predicate._
import parsley.token.descriptions.LexicalDesc
import parsley.token.descriptions.NameDesc
import parsley.token.descriptions.SymbolDesc
import parsley.token.descriptions.numeric.NumericDesc
import parsley.token.descriptions.numeric._
import parsley.token.descriptions.text.TextDesc
import parsley.token.descriptions.text.EscapeDesc
import parsley.token.descriptions.text.NumericEscape
import parsley.token.descriptions.text.NumberOfDigits
import parsley.token.descriptions.SpaceDesc

object LexerGen {
    val genIdentifier = Gen.oneOf[CharPredicate](
        Basic(_.isLetter),
        Basic(_.isLetterOrDigit),
        Gen.asciiPrintableChar.map(c => Basic(_ == c)),
        Gen.nonEmptyContainerOf[Set, Char](Gen.asciiPrintableChar).map(Basic.apply)
    )

    def genNameDesc: Gen[NameDesc] = for {
        identifierStart <- genIdentifier
        identifierLetter <- genIdentifier
        operatorStart <- genIdentifier
        operatorLetter <- genIdentifier
    } yield new NameDesc(identifierStart, identifierLetter, operatorStart, operatorLetter)

    private val genString: Gen[String] = Gen.nonEmptyListOf[Char](Gen.asciiPrintableChar).map(_.mkString)
    private val genEmptyString: Gen[String] = Gen.listOf[Char](Gen.asciiPrintableChar).map(_.mkString)
    private val genStringTuple: Gen[(String, String)] = for {
        s1 <- genString
        s2 <- genString
    } yield (s1, s2)

    def genSymbolDesc: Gen[SymbolDesc] = for {
        hardKeywords <- Gen.containerOf[Set, String](genString)
        hardOperators <- Gen.containerOf[Set, String](genString).suchThat(hardKeywords.intersect(_).isEmpty)
        caseSensitive <- Arbitrary.arbitrary[Boolean]
    } yield new SymbolDesc(hardKeywords, hardOperators, caseSensitive)

    private val genExponentDesc: Gen[ExponentDesc] = Gen.oneOf(
        Gen.const(ExponentDesc.NoExponents),
        for {
            compulsory <- Arbitrary.arbitrary[Boolean]
            chars <- Gen.nonEmptyContainerOf[Set, Char](Gen.alphaChar)
            base <- Gen.choose(1, 64)
            positiveSign <- Gen.oneOf(PlusSignPresence.Illegal, PlusSignPresence.Optional, PlusSignPresence.Required)
            leadingZerosAllowed <- Arbitrary.arbitrary[Boolean]
        } yield new ExponentDesc.Supported(compulsory, chars, base, positiveSign, leadingZerosAllowed)
    )

    def genNumericDesc: Gen[NumericDesc] = for {
        breakCharDesc <- Gen.oneOf(
            Gen.const(BreakCharDesc.NoBreakChar),
            for {
                // Ensure no conflicts with digits (including hexadecimal digits)
                breakChar <- Gen.asciiPrintableChar.suchThat(c => !(('a' to 'f') ++ ('A' to 'F') ++ ('0' to '9') contains c))
                allowedAfterNonDecimalPrefix <- Arbitrary.arbitrary[Boolean]
            } yield new BreakCharDesc.Supported(breakChar, allowedAfterNonDecimalPrefix)
        )
        leadingDotAllowed <- Arbitrary.arbitrary[Boolean]
        trailingDotAllowed <- Arbitrary.arbitrary[Boolean]
        leadingZerosAllowed <- Arbitrary.arbitrary[Boolean]
        positiveSign <- Gen.oneOf(PlusSignPresence.Illegal, PlusSignPresence.Optional, PlusSignPresence.Required)
        integerNumbersCanBeHexadecimal <- Arbitrary.arbitrary[Boolean]
        integerNumbersCanBeOctal <- Arbitrary.arbitrary[Boolean]
        integerNumbersCanBeBinary <- Arbitrary.arbitrary[Boolean]
        realNumbersCanBeHexadecimal <- Arbitrary.arbitrary[Boolean]
        realNumbersCanBeOctal <- Arbitrary.arbitrary[Boolean]
        realNumbersCanBeBinary <- Arbitrary.arbitrary[Boolean]
        // Ensure no overlaps between leads, else it _may_ be ambiguous
        // Would be interesting if Parsley could tell if a number cannot be binary for example
        hexadecimalLeads <- Gen.nonEmptyContainerOf[Set, Char](Gen.alphaChar)
        octalLeads <- Gen.nonEmptyContainerOf[Set, Char](Gen.alphaChar).suchThat(hexadecimalLeads.intersect(_).isEmpty)
        binaryLeads <- Gen.nonEmptyContainerOf[Set, Char](Gen.alphaChar).suchThat((hexadecimalLeads ++ octalLeads).intersect(_).isEmpty)
        decimalExponentDesc <- genExponentDesc
        hexadecimalExponentDesc <- genExponentDesc
        octalExponentDesc <- genExponentDesc
        binaryExponentDesc <- genExponentDesc
    } yield new NumericDesc(
        breakCharDesc,
        leadingDotAllowed,
        trailingDotAllowed,
        leadingZerosAllowed,
        positiveSign,
        integerNumbersCanBeHexadecimal,
        integerNumbersCanBeOctal,
        integerNumbersCanBeBinary,
        realNumbersCanBeHexadecimal,
        realNumbersCanBeOctal,
        realNumbersCanBeBinary,
        hexadecimalLeads,
        octalLeads,
        binaryLeads,
        decimalExponentDesc,
        hexadecimalExponentDesc,
        octalExponentDesc,
        binaryExponentDesc,
    )

    def numericEscapeOf(prefix: Option[Char])(numDigits: NumberOfDigits): NumericEscape = {
        NumericEscape.Supported(prefix, numDigits, java.lang.Character.MAX_CODE_POINT)
    }

    private val digitsRange = Gen.choose(1, 16)
    private def genNumericEscape(prefix: Option[Char]): Gen[NumericEscape] = Gen.oneOf(
        Gen.const(NumericEscape.Illegal),
        Gen.const(NumberOfDigits.Unbounded).map(numericEscapeOf(prefix)),
        digitsRange.map(NumberOfDigits.AtMost(_)).map(numericEscapeOf(prefix)),
        Gen.containerOfN[Set, Int](4, digitsRange).suchThat(_.nonEmpty).map { set =>
            val x::xs = set.toList: @unchecked
            numericEscapeOf(prefix)(NumberOfDigits.Exactly(x, xs: _*))
        }
    )

    private val genCodepoint = Gen.choose(0, java.lang.Character.MAX_CODE_POINT)
    private val genEscapeDesc: Gen[EscapeDesc] = for {
        literals <- Gen.containerOf[Set, Char](Gen.oneOf('\'', '\"', '\\'))
        mapping <- Gen.mapOf(Gen.zip(Gen.nonEmptyListOf(Gen.alphaChar).map(_.mkString), genCodepoint))
        decimalEscape <- genNumericEscape(None)
        hexadecimalEscape <- genNumericEscape(Some('x'))
        octalEscape <- genNumericEscape(Some('o'))
        binaryEscape <- genNumericEscape(Some('b'))
        emptyEscape <- Gen.oneOf(None, Some('&'))
        gapsSupported <- Arbitrary.arbitrary[Boolean]
        escBegin = '\\'
    } yield EscapeDesc(escBegin, literals, mapping, decimalEscape, hexadecimalEscape, octalEscape, binaryEscape, emptyEscape, gapsSupported)

    def genTextDesc: Gen[TextDesc] = for {
        escapeDesc <- genEscapeDesc
        characterLiteralEnd <- Gen.asciiPrintableChar
        stringEnds <- Gen.containerOf[Set, (String, String)](genStringTuple)
        multiStringEnds <- Gen.containerOf[Set, (String, String)](genStringTuple)
        // TODO: Fixme
        graphicCharacter <- genIdentifier
    } yield new TextDesc(escapeDesc, characterLiteralEnd, stringEnds, multiStringEnds, graphicCharacter)

    def genSpaceDesc: Gen[SpaceDesc] = for {
        lineCommentStart <- genEmptyString
        lineCommentAllowsEOF <- Arbitrary.arbitrary[Boolean]
        multiLineCommentStart <- genEmptyString
        // If a multi-line comment is defined (i.e. start or end is defined),
        // both start and end must be non-empty
        multiLineCommentEnd <- if (multiLineCommentStart.isEmpty) Gen.const("") else genString
        multiLineNestedComments <- Arbitrary.arbitrary[Boolean]
        // TODO: Make this more complex later
        space = Basic(_.isWhitespace)
        // whitespaceIsContextDependent <- Arbitrary.arbitrary[Boolean]
        // Needs special handling if true, else errors *will* happen related to registers
        whitespaceIsContextDependent = false
    } yield new SpaceDesc(lineCommentStart, lineCommentAllowsEOF, multiLineCommentStart, multiLineCommentEnd, multiLineNestedComments, space, whitespaceIsContextDependent)

    def genLexicalDesc: Gen[LexicalDesc] = for {
        nameDesc <- genNameDesc
        symbolDesc <- genSymbolDesc
        numericDesc <- genNumericDesc
        textDesc <- genTextDesc
        spaceDesc <- genSpaceDesc
    } yield new LexicalDesc(nameDesc, symbolDesc, numericDesc, textDesc, spaceDesc)
}
