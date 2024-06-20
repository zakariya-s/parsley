package parsley.fuzzing

import org.scalatest.propspec.AnyPropSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import org.scalacheck.Shrink
// import org.scalacheck.Gen
// import parsley.Success

class FuzzingTests extends AnyPropSpec with ScalaCheckPropertyChecks with Matchers {
    implicit def shrinkParsleyInternal: Shrink[ParsleyInternal[Any]] = Shrink[ParsleyInternal[Any]](_.shrinkAny).suchThat(_.correct(Set()).isRight)
    implicit def shrinkInvalidParsleyInternal: Shrink[(ParsleyInternal[Any], Boolean)] = Shrink[(ParsleyInternal[Any], Boolean)](_._1.shrinkAny.map(s => (s, s.correctInvalid(false))).collect {
        case (s, lastFailed) if lastFailed => (s, lastFailed)
    })

    /*property("parsers should retain semantics with built-in optimisations") {
        // import org.scalacheck.Gen.Parameters
        // import org.scalacheck.rng.Seed

        // val parser = ParserGen.genParser.apply(Parameters.default, Seed.apply(6882825281813765860L)).get

        forAll(ParserGen.genParser -> "parser") { parser =>
            //implicit val noShrink: Shrink[String] = Shrink.shrinkAny

            // Assert that correct() is correct...
            // parser.correct(List()).isRight shouldBe true

            forAll(parser.generate -> "refined parser") { internal =>
                internal.generate.parse(internal.input) shouldBe Success(internal.output.get)
            }
        }
    }

    property("semantically-equivalent de-optimized and optimized parsers should behave the same") {
        forAll(ParserGen.genParser -> "parser") { parser =>
            parser.correct(Set()).isRight shouldBe true

            // forAll(parser.disableOptimizations.generate -> "de-optimized refined parser") { internal =>
            //     internal.generate.parse(internal.input) shouldBe Success(internal.output)
            // }

            // forAll(parser.optimize.generate -> "optimized refined parser") { internal =>
            //     internal.generate.parse(internal.input) shouldBe Success(internal.output)
            // }

            forAll(parser.generate -> "refined parser") { internal =>
                forAll(Gen.const(internal.disableOptimizations) -> "de-optimized refined parser") { deoptimizedInternal =>
                    internal.input shouldBe deoptimizedInternal.input
                    internal.output shouldBe deoptimizedInternal.output

                    deoptimizedInternal.generate.parse(internal.input) shouldBe Success(internal.output.get)
                }

                forAll(Gen.const(internal.optimize) -> "optimized refined parser") { optimizedInternal =>
                    internal.input shouldBe optimizedInternal.input
                    internal.output shouldBe optimizedInternal.output

                    optimizedInternal.generate.parse(internal.input) shouldBe Success(internal.output.get)
                }
            }
        }
    }*/

    // Invalid parser tests

    // property("failing parsers should produce the same error message upon optimization") {
    //     forAll(ParserGen.genInvalidParser -> "invalid parser") { case (parser, lastFailed) =>
    //         // parser.correctInvalid(false) shouldBe true

    //         forAll(parser.generateInvalid(lastFailed) -> "unoptimised invalid refined parser") { internal =>
    //             forAll(Gen.const(internal.optimize.disableOptimizations) -> "optimized invalid refined parser") { optimizedInternal =>
    //                 internal.generate.parse(internal.input) shouldBe optimizedInternal.generate.parse(internal.input)
    //             }
    //         }
    //     }
    // }

    // Parser law tests

    // (p <|> pure(f)) <*> q ==> (p <*> q) <|> (f <$> q)
    /*property("first parser law LHS-to-RHS should work") {
        implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny
        import ParsleyInternalUnwrapped._
        forAll(ParserGen.parserLaw1LhsToRhs(false) -> "parser") { case (parser, _) =>
            forAll(parser.generate -> "original parser LHS") { internal =>
                val transformedParser = internal match {
                    case par @ Ap(Or(p, Pure(f), leftParser), q) => Or(Ap(p, q)(q.evPrettyPrint, par.evPrettyPrint), Map(q, f)(par.evPrettyPrint), leftParser)(par.evPrettyPrint)
                    case _ => fail("Parser did not match pattern of parser law")
                }

                forAll(Gen.const(transformedParser) -> "transformed parser RHS") { transformedInternal =>
                    internal.generate.parse(internal.input) shouldBe transformedInternal.generate.parse(internal.input)
                }
            }
        }
    }*/

    // (p <|> pure(f)) <*> q ==> (p <*> q) <|> (f <$> q)
    /*property("first parser law RHS-to-LHS should work") {
        implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny
        import ParsleyInternalUnwrapped._
        forAll(ParserGen.parserLaw1RhsToLhs(false) -> "parser") { case (parser, _) =>
            forAll(parser.generate -> "original parser RHS") { internal =>
                val transformedParser = internal match {
                    case par @ Or(Ap(p, q), Map(_, f), leftParser) => Ap(Or(p, Pure(f), leftParser), q)(q.evPrettyPrint, par.evPrettyPrint)
                    case _ => fail("Parser did not match pattern of parser law")
                }

                forAll(Gen.const(transformedParser) -> "transformed parser LHS") { transformedInternal =>
                    internal.generate.parse(internal.input) shouldBe transformedInternal.generate.parse(internal.input)
                }
            }
        }
    }*/

    // atomic(p *> q) <|> (r *> q) === (atomic(p) <|> r) *> q
    // p = char('a'), r = pure(()), q = char('a')
    // input is "a"
    // the left-one succeeds, the right one fails
    /*property("second parser law should work") {
        implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny
        import ParsleyInternalUnwrapped._
        forAll(ParserGen.parserLaw2(false) -> "parser") { case (parser, _) =>
            forAll(parser.generate -> "original parser LHS") { internal =>
                val transformedParser = internal match {
                    case Or(Atomic(Then(p, q)), Then(r, _), leftParser) => Then(Sum(Atomic(p)(p.evPrettyPrint), r, leftParser)(p.evPrettyPrint, r.evPrettyPrint), q)(q.evPrettyPrint)
                    case _ => fail("Parser did not match pattern of parser law")
                }

                forAll(Gen.const(transformedParser) -> "transformed parser RHS") { transformedInternal =>
                    internal.generate.parse(internal.input) shouldBe transformedInternal.generate.parse(internal.input)
                }
            }
        }
    }*/
}
