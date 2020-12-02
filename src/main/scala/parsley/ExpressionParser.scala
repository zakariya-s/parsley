package parsley

import scala.annotation.tailrec
import parsley.ExpressionParser._
import parsley.Combinator._
import parsley.XCompat._

/** This class is used to generate efficient expression parsers given a table of operators
  * in operator of operator precedence and an atomic value that represents the smallest
  * part of the expression. Caters to unary and binary operators of different associativities.
  */
final class ExpressionParser[-A, +B] private (atom: =>Parsley[A], table: Levels[A, B])
{
    private def convertOperators[A, B](atom: Parsley[A], opList: Ops[A, B])(implicit wrap: A => B): Parsley[B] = opList match
    {
        case Lefts(ops @ _*) => chainl1(atom, choice(ops: _*))
        case Rights(ops @ _*) => chainr1(atom, choice(ops: _*))
        case Prefixes(ops @ _*) => chainPre(choice(ops: _*), atom.map(wrap))
        // FIXME: Postfix operators which are similar to binary ops may fail, how can we work around this?
        case Postfixes(ops @ _*) => chainPost(atom.map(wrap), choice(ops: _*))
    }

    @tailrec private def crushLevels[A, B](atom: Parsley[A], lvls: Levels[A, B]): Parsley[B] = lvls match
    {
        case NoLevel(ev) => ev.substituteCo[Parsley](atom)
        case Level(ops, lvls) => crushLevels(convertOperators(atom, ops)(ops.wrap), lvls)
    }

    /** The expression parser generated by this generator. */
    lazy val expr: Parsley[B] = crushLevels(atom, table)
}

object ExpressionParser
{
    /** This is used to build an expression parser for a monolithic type. Levels are specified from strongest
     * to weakest.
     * @tparam A The type of the monolithic tree
     * @param atom The atomic unit of the expression, for instance numbers/variables
     * @param table A table of operators. Table is ordered highest precedence to lowest precedence.
     *              Each list in the table corresponds to operators of the same precedence level.
     * @return A parser for the described expression language
     */
    def apply[A](atom: =>Parsley[A], table: MonoOps[A]*) = new ExpressionParser(atom, table.foldRight(Levels.empty[A])(Level.apply[A, A, A]))
    /** This is used to build an expression parser for a multi-layered expression tree type. Levels are specified
     * from strongest to weakest.
     * @tparam A The type of the atomic unit of the expression
     * @tparam B The type of the resulting parse tree (outermost operations)
     * @param atom The atomic unit of the expression
     * @param table A table of operators. Table is ordered highest precedence to lowest precedence.
     *              See [[Levels]] and it's subtypes for a description of how the types work.
     * @return A parser for the described expression language
     */
    def apply[A, B](atom: =>Parsley[A], table: Levels[A, B]) = new ExpressionParser(atom, table)
    /** Denotes the associativity of an operator, either `AssocLeft` or `AssocRight`. */
    sealed trait Assoc
    case object AssocLeft extends Assoc
    case object AssocRight extends Assoc
    /** Denotes the fixity of an operator, either `Prefix` or `Postfix`. */
    sealed trait Fixity
    case object Prefix extends Fixity
    case object Postfix extends Fixity

    type MonoOps[A] = Ops[A, A]
    /** A list of operators on the same precedence level. Note operators of different fixities cannot
      * mix on the same level of indentation. Either `Lefts` which is a list of infix left-assocative
      * operators, `Rights` which is a list of infix right-associative operators, `Prefixes` which is
      * a list of prefixing unary operators or `Postfixes` a list of postfixing unary operators.
      *
      * Each list of operators will also require a wrapping function of type `A => B`. This allows it
      * to convert values of type `A` into values of type `B` when there is no more operation to perform.
      * @tparam A The type of the layer below
      * @tparam B The type of this layer
      */
    sealed trait Ops[-A, B]
    {
        private [ExpressionParser] val wrap: A => B
    }
    case class Lefts[-A, B](ops: Parsley[(B, A) => B]*)(override implicit val wrap: A => B) extends Ops[A, B]
    case class Rights[-A, B](ops: Parsley[(A, B) => B]*)(override implicit val wrap: A => B) extends Ops[A, B]
    case class Prefixes[-A, B](ops: Parsley[B => B]*)(override implicit val wrap: A => B) extends Ops[A, B]
    case class Postfixes[-A, B](ops: Parsley[B => B]*)(override implicit val wrap: A => B) extends Ops[A, B]

    object Infixes
    {
        /**
         * This is used to more succinctly describe binary precedence levels for monolithic types
         * (where all levels result in the same type). It represents either left or right associative
         * binary operations by providing an associativity.
         * @param assoc The associativity of the operation
         * @param ops The operators present on this level
         */
        def apply[A](assoc: Assoc, ops: Parsley[(A, A) => A]*): MonoOps[A] = assoc match
        {
            case AssocLeft => Lefts[A, A](ops: _*)
            case AssocRight => Rights[A, A](ops: _*)
        }
    }

    object Unaries
    {
        /**
         * This is used to more succinctly describe unary precedence levels for monolithic types
         * (where all levels result in the same type). It represents either prefix or postfix
         * unary operations by providing a fixity.
         * @param fixity The fixity of the operation
         * @param ops The operators present on this level
         */
        def apply[A](fixity: Fixity, ops: Parsley[A => A]*): MonoOps[A] = fixity match
        {
            case Prefix => Prefixes[A, A](ops: _*)
            case Postfix => Postfixes[A, A](ops: _*)
        }
    }

    /**
     * For more complex expression parser types `Levels` can be used to
     * describe the precedence table whilst preserving the intermediate
     * structure between each level.
     * @tparam A The base type accepted by this list of levels
     * @tparam B The type of structure produced by the list of levels
     */
    sealed trait Levels[-A, +B]
    /**
     * This represents a single new level of the hierarchy, with stronger
     * precedence than its tail.
     * @tparam A The base type accepted by this layer
     * @tparam B The intermediate type that will be provided to the next layer
     * @tparam C The type of structure produced by the next layers
     * @param ops The operators accepted at this level
     * @param lvls The next, weaker, levels in the precedence table
     * @return A larger precedence table transforming atoms of type `A` into
    *          a structure of type `C`.
     */
    final case class Level[-A, B, +C](ops: Ops[A, B], lvls: Levels[B, C]) extends Levels[A, C]
    private final case class NoLevel[A, B](ev: A =:= B) extends Levels[A, B]
    object Levels
    {
        /**
         * This represents the end of a precedence table. It will not
         * touch the structure in any way.
         * @tparam A The type of the structure to be produced by the table.
         */
        def empty[A]: Levels[A, A] = NoLevel(refl[A])
    }

    implicit class LevelBuilder[B, +C](lvls: Levels[B, C])
    {
        def +:[A](lvl: Ops[A, B]): Levels[A, C] = Level(lvl, lvls)
    }
}