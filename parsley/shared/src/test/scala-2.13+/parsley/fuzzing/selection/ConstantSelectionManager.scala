package parsley.fuzzing.selection

import org.scalacheck.Gen

import parsley.fuzzing.ParsleyInternal

class ConstantSelectionManager extends SelectionManager {
    type Context = Unit
    
    override def initialContext: Context = ()
    override def genParser[T](allGen: Seq[Class[_ <: ParsleyInternal[T]]], ctx: Context): Gen[(Option[Class[_ <: ParsleyInternal[T]]], Context)] =
        Gen.frequency(((1 -> Gen.const(None)) +: allGen.map { g => 2 -> Gen.const(Some(g)) }): _*).map((_, ctx))
}
