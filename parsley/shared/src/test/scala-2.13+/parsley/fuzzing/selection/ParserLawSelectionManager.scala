package parsley.fuzzing.selection

import org.scalacheck.Gen

import parsley.fuzzing.ParsleyInternal

// Not at all ready -- shrinking needs to be supported for parser laws,
// selecting specific combinators needs to be supported somehow by guiding types
// since not all combinators are defined for all types, laws which contain the
// same combinator twice needs some way to be represented, etc.
class ParserLawSelectionManager extends SelectionManager {
    type Context = Unit
    
    override def initialContext: Context = ()
    override def genParser[T](allGen: Seq[Class[_ <: ParsleyInternal[T]]], ctx: Context): Gen[(Option[Class[_ <: ParsleyInternal[T]]], Context)] =
        Gen.frequency(((1 -> Gen.const(None)) +: allGen.map { g => 2 -> Gen.const(Some(g)) }): _*).map((_, ctx))
}
