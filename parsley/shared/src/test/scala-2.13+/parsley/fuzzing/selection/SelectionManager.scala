package parsley.fuzzing.selection

import org.scalacheck.Gen

import parsley.fuzzing.ParsleyInternal

trait SelectionManager {
    type Context

    def initialContext: Context
    def genParser[T](allGen: Seq[Class[_ <: ParsleyInternal[T]]], ctx: Context): Gen[(Option[Class[_ <: ParsleyInternal[T]]], Context)]
}
