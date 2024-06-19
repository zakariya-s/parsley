package parsley.fuzzing

import org.scalatest.propspec.AnyPropSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

// import parsley.token.Lexer

// import org.scalacheck.Gen
// import org.scalacheck.Arbitrary
import org.scalacheck.Shrink

// import parsley.Success

import parsley.fuzzing.lexer.NumericGen.NumberBase
// import parsley.token.predicate._
// import parsley.fuzzing.lexer.NumericGen
// import parsley.fuzzing.lexer.NameGen
// import parsley.fuzzing.lexer.CharacterGen
// import parsley.fuzzing.lexer.StringGen

// import parsley.token.descriptions.LexicalDesc
// import parsley.token.descriptions.numeric.NumericDesc
// import parsley.token.descriptions.numeric._

class LexerTests extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers {
    implicit val noShrinkString: Shrink[String] = Shrink.shrinkAny
    implicit val noShrinkInt: Shrink[Int] = Shrink.shrinkAny
    implicit val noShrinkIntTuple: Shrink[(String, BigInt, NumberBase)] = Shrink.shrinkAny
    implicit val noShrinkRealTuple: Shrink[(String, BigDecimal, NumberBase)] = Shrink.shrinkAny

    /*property("integers should be properly handled by the lexer") {
        forAll(LexerGen.genLexicalDesc -> "lexical description", minSuccessful(1000)) { lexicalDesc =>
            val lexer = new Lexer(lexicalDesc)

            forAll(NumericGen.genInteger(lexicalDesc, 999999999) -> "(number string to parse, actual number, number base)", minSuccessful(1000)) { case (numStr, num, numberBase) =>
                lexer.lexeme.integer.number.parse(numStr) shouldBe Success(num)

                val specificIntegerBaseParser = numberBase match {
                    case NumberBase.Decimal => lexer.lexeme.integer.decimal
                    case NumberBase.Hexadecimal => lexer.lexeme.integer.hexadecimal
                    case NumberBase.Octal => lexer.lexeme.integer.octal
                    case NumberBase.Binary => lexer.lexeme.integer.binary
                }

                specificIntegerBaseParser.parse(numStr) shouldBe Success(num)
            }
        }
    }

    property("reals should be properly handled by the lexer") {
        forAll(LexerGen.genLexicalDesc -> "lexical description", minSuccessful(1000)) { lexicalDesc =>
            val lexer = new Lexer(lexicalDesc)

            forAll(NumericGen.genReal(lexicalDesc, 999999999) -> "(number string to parse, actual number, number base)", minSuccessful(1000)) { case (numStr, num, numberBase) =>
                lexer.lexeme.real.number.parse(numStr) shouldBe Success(num)

                val specificRealBaseParser = numberBase match {
                    case NumberBase.Decimal => lexer.lexeme.real.decimal
                    case NumberBase.Hexadecimal => lexer.lexeme.real.hexadecimal
                    case NumberBase.Octal => lexer.lexeme.real.octal
                    case NumberBase.Binary => lexer.lexeme.real.binary
                }

                specificRealBaseParser.parse(numStr) shouldBe Success(num)
            }
        }
    }

    private def genCharPredicate = Gen.oneOf[CharPredicate](
        Basic(_.isLetter),
        Basic(_.isLetterOrDigit),
        Gen.asciiPrintableChar.map(c => Basic(_ == c)),
        Gen.nonEmptyContainerOf[Set, Char](Gen.asciiPrintableChar).map(Basic.apply),
        NotRequired
    )

    property("identifiers should be properly handled by the lexer") {
        forAll(LexerGen.genLexicalDesc -> "lexical description", Gen.option(genCharPredicate) -> "start char predicate", minSuccessful(1000)) { (lexicalDesc, startChar) =>
            val lexer = new Lexer(lexicalDesc)

            forAll(NameGen.genIdentifier(lexicalDesc, startChar) -> "(number string to parse, actual number, number base)", minSuccessful(1000)) { identifier =>
                startChar match {
                    case Some(startCharPredicate) => lexer.lexeme.names.identifier(startCharPredicate).parse(identifier) shouldBe Success(identifier)
                    case None => lexer.lexeme.names.identifier.parse(identifier) shouldBe Success(identifier)
                }
            }
        }
    }

    property("user-defined operators should be properly handled by the lexer") {
        forAll(LexerGen.genLexicalDesc -> "lexical description", Gen.option(Gen.zip(genCharPredicate, genCharPredicate)) -> "(start char predicate, end char predicate)", minSuccessful(1000)) { (lexicalDesc, startEndChar) =>
            val lexer = new Lexer(lexicalDesc)

            forAll(NameGen.genUserDefinedOperator(lexicalDesc, startEndChar) -> "(number string to parse, actual number, number base)", minSuccessful(1000)) { identifier =>
                startEndChar match {
                    case Some((startCharPredicate, endCharPredicate)) => lexer.lexeme.names.userDefinedOperator(startCharPredicate, endCharPredicate).parse(identifier) shouldBe Success(identifier)
                    case None => lexer.lexeme.names.userDefinedOperator.parse(identifier) shouldBe Success(identifier)
                }
            }
        }
    }

    property("characters should be properly handled by the lexer") {
        forAll(LexerGen.genLexicalDesc -> "lexical description", minSuccessful(1000)) { lexicalDesc =>
            val lexer = new Lexer(lexicalDesc)

            forAll(CharacterGen.genCharacter(lexicalDesc) -> "(character to parse, actual character)", minSuccessful(1000)) { case (charStr, char) =>
                lexer.lexeme.character.ascii.parse(charStr) shouldBe Success(char)
                lexer.lexeme.character.basicMultilingualPlane.parse(charStr) shouldBe Success(char)
                lexer.lexeme.character.latin1.parse(charStr) shouldBe Success(char)
                lexer.lexeme.character.fullUtf16.parse(charStr) shouldBe Success(char.toInt)
                // TODO: Generate Unicode characters too 
            }
        }
    }

    property("normal strings should be properly handled by the lexer") {
        forAll(LexerGen.genLexicalDesc -> "lexical description", minSuccessful(1000)) { lexicalDesc =>
            val lexer = new Lexer(lexicalDesc)

            forAll(StringGen.genString(lexicalDesc) -> "(string to parse, actual string)", minSuccessful(1000)) { case (parseStr, actualStr) =>
                lexer.lexeme.string.ascii.parse(parseStr) shouldBe Success(actualStr)
                lexer.lexeme.string.fullUtf16.parse(parseStr) shouldBe Success(actualStr)
                lexer.lexeme.string.latin1.parse(parseStr) shouldBe Success(actualStr)
                // TODO: Generate Unicode strings too 
            }
        }
    }*/
}
