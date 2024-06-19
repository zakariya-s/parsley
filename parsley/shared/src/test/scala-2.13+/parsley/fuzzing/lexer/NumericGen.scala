package parsley.fuzzing.lexer

import org.scalacheck.Gen

import parsley.token.descriptions.LexicalDesc
import parsley.token.descriptions.numeric._

object NumericGen {
    sealed trait NumberBase {
        def base: Int
    }
    object NumberBase {
        final object Decimal extends NumberBase {
            override def base = 10
        }
        final object Hexadecimal extends NumberBase {
            override def base = 16
        }
        final object Octal extends NumberBase {
            override def base = 8
        }
        final object Binary extends NumberBase {
            override def base = 2
        }
    }

    private def genNumStr(lexer: LexicalDesc, num: BigInt, numberBase: NumberBase): Gen[(String, String)] = {
        val numericDesc = lexer.numericDesc

        val hexStr = num.toString(16)
        val octalStr = num.toString(8)
        val binaryStr = num.toString(2)

        val gen = (leads: Set[Char], numStr: String) => Gen.oneOf(leads).map("0" + _.toString).map((_, numStr))
        
        val res = numberBase match {
            case NumberBase.Decimal => Gen.const(("", num.toString))
            case NumberBase.Hexadecimal => gen(numericDesc.hexadecimalLeads, hexStr)
            case NumberBase.Octal => gen(numericDesc.octalLeads, octalStr)
            case NumberBase.Binary => gen(numericDesc.binaryLeads, binaryStr)
        }

        res.flatMap { case (lead, numStr) => Gen.oneOf((lead, numStr), (lead, numStr.toUpperCase)) }
    }

    private def leadingZeros(numStr: String, leadingZerosAllowed: Boolean): Gen[String] = if (leadingZerosAllowed) {
        Gen.listOf[Char]('0').map(_.mkString).map(_ + numStr)
    } else {
        Gen.const(numStr)
    }

    private def genExponents(num: BigDecimal, exponentDesc: ExponentDesc, maxNum: BigInt): Gen[(String, BigDecimal)] = {
        // Suppress maxNum unused warnings for now
        maxNum match { case _ => () }

        exponentDesc match {
            case ExponentDesc.NoExponents => for {
                res <- Gen.const(("", num))
            } yield res
            case ExponentDesc.Supported(compulsory, chars, base, positiveSign, leadingZerosAllowed) => {
                // val exponent = (num.log() / math.log(base)).toInt
                // val mantissa = (num / math.pow(base, exponent)).toInt
                // val n = (mantissa * math.pow(base, exponent)).toInt

                val res = for {
                    power <- Gen.choose(0, 20)
                    n = num * BigDecimal(Math.pow(base.toDouble, power.toDouble).toInt)
                    exponentChar <- Gen.oneOf(chars)
                    sign <- positiveSign match {
                        case PlusSignPresence.Illegal => Gen.const("")
                        case PlusSignPresence.Optional => Gen.oneOf("", "+")
                        case PlusSignPresence.Required => Gen.const("+")
                    }
                    leadingZeros <- leadingZeros(power.toString, leadingZerosAllowed)
                } yield (s"$exponentChar$sign$leadingZeros", n)

                if (compulsory) res else Gen.oneOf(Gen.const(("", num)), res)
            }
        }
    }

    def genInteger(lexer: LexicalDesc, maxNum: BigInt): Gen[(String, BigInt, NumberBase)] = for {
        num <- Gen.choose(BigInt(0), maxNum)
        numericDesc = lexer.numericDesc
        sign <- numericDesc.positiveSign match {
            case PlusSignPresence.Illegal => Gen.const("")
            case PlusSignPresence.Optional => Gen.oneOf("", "+")
            case PlusSignPresence.Required => Gen.const("+")
        }
        possibleNumberBases = (if (numericDesc.integerNumbersCanBeHexadecimal) List(NumberBase.Hexadecimal) else List())
                            ++ (if (numericDesc.integerNumbersCanBeOctal) List(NumberBase.Octal) else List())
                            ++ (if (numericDesc.integerNumbersCanBeBinary) List(NumberBase.Binary) else List())
        numberBase <- Gen.oneOf(NumberBase.Decimal, NumberBase.Decimal, possibleNumberBases: _*)
        (lead, numStr) <- genNumStr(lexer, num, numberBase)
        numStrBreak <- numericDesc.literalBreakChar match {
            case BreakCharDesc.NoBreakChar => Gen.const(numStr)
            case BreakCharDesc.Supported(breakChar, allowedAfterNonDecimalPrefix@_) => Gen.oneOf(
                Gen.const(numStr),
                for {
                    // Cannot have a break at the end of the number
                    breaks <- Gen.listOfN(numStr.length - 1, Gen.oneOf("", breakChar.toString)).map(l => l :+ "")
                    breakStr = numStr.zip(breaks).map { case (x, y) => x.toString + y }.mkString
                } yield breakStr
            )
        }
        numStrLeadingZeros <- leadingZeros(numStrBreak, numericDesc.leadingZerosAllowed)
    } yield (s"$sign$lead$numStrLeadingZeros", num, numberBase)

    def genReal(lexer: LexicalDesc, maxNum: BigInt): Gen[(String, BigDecimal, NumberBase)] = for {
        num <- Gen.choose(BigDecimal(0), BigDecimal(maxNum))
        numericDesc = lexer.numericDesc
        sign <- numericDesc.positiveSign match {
            case PlusSignPresence.Illegal => Gen.const("")
            case PlusSignPresence.Optional => Gen.oneOf("", "+")
            case PlusSignPresence.Required => Gen.const("+")
        }
        possibleNumberBases = (if (numericDesc.realNumbersCanBeHexadecimal) List(NumberBase.Hexadecimal) else List())
                            ++ (if (numericDesc.realNumbersCanBeOctal) List(NumberBase.Octal) else List())
                            ++ (if (numericDesc.realNumbersCanBeBinary) List(NumberBase.Binary) else List())
        numberBase <- Gen.oneOf(NumberBase.Decimal, NumberBase.Decimal, possibleNumberBases: _*)
        (exponent, n) <- numberBase match {
            case NumberBase.Decimal => genExponents(num, numericDesc.decimalExponentDesc, maxNum)
            case NumberBase.Hexadecimal => genExponents(num, numericDesc.hexadecimalExponentDesc, maxNum)
            case NumberBase.Octal => genExponents(num, numericDesc.octalExponentDesc, maxNum)
            case NumberBase.Binary => genExponents(num, numericDesc.binaryExponentDesc, maxNum)
        }
        mantissa = n.toBigInt
        fractional = n.remainder(BigDecimal(1))
        // Remove the leading zero so that we are left with just the dot and the fractional component
        fractionalStr = if (numericDesc.trailingDotAllowed && fractional == 0 && exponent.isEmpty) "." else n.toString.substring(1)
        (lead, numStr) <- genNumStr(lexer, mantissa, numberBase)
        numStrBreak <- numericDesc.literalBreakChar match {
            case BreakCharDesc.NoBreakChar => Gen.const(numStr)
            case BreakCharDesc.Supported(breakChar, allowedAfterNonDecimalPrefix@_) => Gen.oneOf(
                Gen.const(numStr),
                for {
                    // Cannot have a break at the end of the number
                    breaks <- Gen.listOfN(numStr.length - 1, Gen.oneOf("", breakChar.toString)).map(l => l :+ "")
                    breakStr = numStr.zip(breaks).map { case (x, y) => x.toString + y }.mkString
                } yield breakStr
            )
        }
        numStrLeadingZeros <- leadingZeros(numStrBreak, numericDesc.leadingZerosAllowed)
    } yield (s"$sign$lead$numStrLeadingZeros$fractionalStr$exponent", n, numberBase)
}
