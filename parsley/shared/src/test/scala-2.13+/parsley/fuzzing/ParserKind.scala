package parsley.fuzzing

sealed trait ParserKind

object ParserKind {
    case object Leaf extends ParserKind
    case object Then extends ParserKind
    case object ThenDiscard extends ParserKind
    case object Or extends ParserKind
    case object Map extends ParserKind
    case object LookAhead extends ParserKind
    case object IfS extends ParserKind
    case object Ap extends ParserKind
    case object ReverseAp extends ParserKind
    case object Branch extends ParserKind
    case object Filter extends ParserKind

    case object Sum extends ParserKind
    case object Zip extends ParserKind
    case object Many extends ParserKind
    case object Cons extends ParserKind
    case object NotFollowedBy extends ParserKind
    case object Optional extends ParserKind
}
