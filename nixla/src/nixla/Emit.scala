package nixla

import NixExpr.*, StrPart.*, Binding.*, Param.*

/** Precedence-aware pretty printer: NixExpr => Nix source text.
  *
  * Precedence numbers follow the Nix manual; LOWER binds TIGHTER.
  * We parenthesize a child exactly when its precedence is looser than
  * what its context allows, so emitted code stays human-readable.
  */
object Emit:

  def apply(e: NixExpr): String = emit(e, 0, TOP) + "\n"

  private val TOP = 15
  private val IND = "  "

  // op -> (precedence, associativity)  L / R / N(one)
  private val ops: Map[String, (Int, Char)] = Map(
    "++" -> (5, 'R'),
    "*"  -> (6, 'L'), "/" -> (6, 'L'),
    "+"  -> (7, 'L'), "-" -> (7, 'L'),
    "//" -> (9, 'R'),
    "<"  -> (10, 'N'), ">" -> (10, 'N'), "<=" -> (10, 'N'), ">=" -> (10, 'N'),
    "==" -> (11, 'N'), "!=" -> (11, 'N'),
    "&&" -> (12, 'L'), "||" -> (13, 'L'),
    "->" -> (14, 'R')
  )

  private def prec(e: NixExpr): Int = e match
    case NInt(_) | NFloat(_) | NBool(_) | NNull | NStr(_) | NIndentStr(_) |
         NPath(_) | NIdent(_) | NList(_) | NAttrSet(_, _) => 0
    case NSelect(_, _, _)         => 1
    case NApply(_, _)             => 2
    case NUnary("-", _)           => 3
    case NHasAttr(_, _)           => 4
    case NUnary(_, _)             => 8
    case NBinOp(op, _, _)         => ops(op)._1
    case _                        => TOP // lambda / let / if / with / assert

  private def emit(e: NixExpr, ind: Int, allowed: Int): String =
    val body = raw(e, ind)
    if prec(e) > allowed then s"($body)" else body

  private def pad(ind: Int) = IND * ind

  private def raw(e: NixExpr, ind: Int): String = e match
    case NInt(v)    => v.toString
    case NFloat(v)  => v.toString
    case NBool(v)   => v.toString
    case NNull      => "null"
    case NIdent(n)  => n
    case NPath(p)   => p

    case NStr(parts) =>
      "\"" + parts.map {
        case Lit(s)    => escape(s)
        case Interp(x) => "${" + emit(x, ind, TOP) + "}"
      }.mkString + "\""

    case NIndentStr(parts) =>
      val inner = parts.map {
        case Lit(s)    => s.replace("''", "'''").linesWithSeparators
                           .map(l => if l.trim.isEmpty then l else pad(ind + 1) + l).mkString
        case Interp(x) => "${" + emit(x, ind + 1, TOP) + "}"
      }.mkString
      s"''\n$inner${pad(ind)}''"

    case NList(Nil)   => "[ ]"
    case NList(items) if items.sizeIs <= 3 && items.forall(isSmall) =>
      items.map(emit(_, ind, 1)).mkString("[ ", " ", " ]") // list elems: select-level
    case NList(items) =>
      val body = items.map(i => pad(ind + 1) + emit(i, ind + 1, 1)).mkString("\n")
      s"[\n$body\n${pad(ind)}]"

    case NAttrSet(r, Nil)      => if r then "rec { }" else "{ }"
    case NAttrSet(r, bindings) =>
      val kw   = if r then "rec " else ""
      val body = bindings.map(b => pad(ind + 1) + binding(b, ind + 1)).mkString("\n")
      s"$kw{\n$body\n${pad(ind)}}"

    case NLambda(p, body) =>
      s"${param(p, ind)}:${lambdaBody(body, ind)}"

    case NApply(f, a) => s"${emit(f, ind, 2)} ${emit(a, ind, 1)}"

    case NLet(bindings, body) =>
      val bs = bindings.map(b => pad(ind + 1) + binding(b, ind + 1)).mkString("\n")
      s"let\n$bs\n${pad(ind)}in\n${pad(ind + 1)}${emit(body, ind + 1, TOP)}"

    case NIf(c, t, f) =>
      s"if ${emit(c, ind, TOP)} then ${emit(t, ind, TOP)} else ${emit(f, ind, TOP)}"

    case NWith(env, body)   => s"with ${emit(env, ind, TOP)}; ${emit(body, ind, TOP)}"
    case NAssert(c, body)   => s"assert ${emit(c, ind, TOP)}; ${emit(body, ind, TOP)}"

    case NSelect(x, path, or) =>
      val base = s"${emit(x, ind, 1)}.${attrPath(path)}"
      or.fold(base)(d => s"$base or ${emit(d, ind, 1)}")

    case NHasAttr(x, path)  => s"${emit(x, ind, 3)} ? ${attrPath(path)}"
    case NUnary(op, x)      => s"$op${emit(x, ind, prec(e) - 1)}"

    case NBinOp(op, l, r) =>
      val (p, assoc) = ops(op)
      val lp = if assoc == 'L' then p else p - 1
      val rp = if assoc == 'R' then p else p - 1
      s"${emit(l, ind, lp)} $op ${emit(r, ind, rp)}"

  private def lambdaBody(body: NixExpr, ind: Int): String = body match
    case _: NLambda | _: NAttrSet | _: NLet => "\n" + pad(ind + 1) + emit(body, ind + 1, TOP)
    case _                                  => " " + emit(body, ind, TOP)

  private def binding(b: Binding, ind: Int): String = b match
    case Bind(path, v)          => s"${attrPath(path)} = ${emit(v, ind, TOP)};"
    case Inherit(None, ns)      => s"inherit ${ns.mkString(" ")};"
    case Inherit(Some(from), ns) => s"inherit (${emit(from, ind, TOP)}) ${ns.mkString(" ")};"

  private def param(p: Param, ind: Int): String = p match
    case Simple(n) => n
    case Destructure(fields, ellipsis, bound) =>
      val fs = fields.map { (n, d) => d.fold(n)(x => s"$n ? ${emit(x, ind, TOP)}") } ++
        (if ellipsis then List("...") else Nil)
      val set = fs.mkString("{ ", ", ", " }")
      bound.fold(set)(b => s"$set @ $b")

  private val identRe = "[a-zA-Z_][a-zA-Z0-9_'-]*".r
  private def attrPath(path: List[String]): String =
    path.map(k => if identRe.matches(k) then k else "\"" + escape(k) + "\"").mkString(".")

  private def isSmall(e: NixExpr): Boolean = e match
    case NInt(_) | NFloat(_) | NBool(_) | NNull | NIdent(_) | NPath(_) => true
    case NStr(List(Lit(s)))                                           => s.length < 30
    case NSelect(x, _, None)                                          => isSmall(x)
    case _                                                            => false

  private def escape(s: String): String =
    s.flatMap {
      case '"'  => "\\\""
      case '\\' => "\\\\"
      case '\n' => "\\n"
      case '\t' => "\\t"
      case '$'  => "\\$" // conservative: avoids accidental ${
      case c    => c.toString
    }
