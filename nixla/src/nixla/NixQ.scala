package nixla

import nixla.cst.*

/** Grafting engine behind the `nixq` quasiquote (layer beside 3-4 in
  * DESIGN.md).
  *
  * The skeleton (the literal parts of the interpolation) is parsed with
  * markers standing in for the splices; splice values are Pretty-rendered,
  * re-parsed (so they carry real trivia), and grafted where their marker
  * landed:
  *
  *   - expression position: `x = ${value};`
  *   - attribute name:      `${name} = 1;`  -> a real Nix dynamic attr
  *   - inside a string:     `"echo ${msg}"` -> Nix string interpolation
  *   - inside a path:       `./src/${name}`
  *
  * Everything else (splice in a comment, in a URI, as a lambda parameter,
  * as an inherit name, fused into an identifier) is rejected — at Scala
  * compile time, because the macro dry-runs this very code with dummy
  * splices.
  */
final case class GraftError(message: String) extends RuntimeException(message)

object NixQ:
  import SyntaxKind.*

  private[nixla] def marker(i: Int): String  = s"__nixla_q${i}__"
  private val markerRe                       = "__nixla_q([0-9]+)__".r
  private[nixla] def skeletonOf(parts: List[String]): String =
    parts.init.zipWithIndex.map((p, i) => p + marker(i)).mkString + parts.last

  /** compile-time dry run: same path as graft, with dummy splices */
  private[nixla] def validate(parts: List[String]): Unit =
    graft(parts, List.fill(parts.length - 1)(Build.ident("nixla_dummy")))

  def graft(parts: List[String], splices: List[GreenNode]): Nix[NAny] =
    require(splices.length == parts.length - 1, "nixq: splice arity mismatch")
    val root = Parser.parseGreen(skeletonOf(parts))
    // Pretty-render then re-parse each splice so grafted subtrees carry real
    // whitespace trivia: the skeleton stays verbatim, grafts are canonical.
    val prepared = splices.toVector.map { g =>
      val txt = Pretty(Build.parenIf(g, 1)).trim
      exprOf(Parser.parseGreen(txt))
    }
    val out = rw(root, prepared)
    scanLeftover(out)
    Nix.raw(exprOf(out))

  /** the expression child of a root node (top-level comments are dropped;
    * quasiquotes denote expressions, not files)
    */
  private def exprOf(root: GreenNode): GreenNode =
    root.children.collectFirst { case n: GreenNode => n }
      .getOrElse(throw GraftError("nixq: empty quasiquote"))

  private def markerIx(text: String): Option[Int] = text match
    case markerRe(i) => Some(i.toInt)
    case _           => None

  private def identMarker(n: GreenNode): Option[Int] =
    if n.kind != N_IDENT then None
    else n.children.collectFirst { case t: GreenToken if t.kind == TK_IDENT => t.text }
      .flatMap(markerIx)

  private def rw(n: GreenNode, sp: Vector[GreenNode]): GreenNode =
    var seenNode = false
    val children = n.children.flatMap { child =>
      child match
        case c: GreenNode =>
          val isParamPosition = n.kind == N_LAMBDA && !seenNode
          seenNode = true
          identMarker(c) match
            case Some(i) if !isParamPosition => Vector[GreenChild](sp(i))
            case _                           => Vector[GreenChild](rw(c, sp))
        case t: GreenToken if t.kind == TK_IDENT && n.kind == N_ATTRPATH =>
          markerIx(t.text) match
            case Some(i) => Vector[GreenChild](Build.dynAttr(sp(i)))
            case None    => Vector[GreenChild](t)
        case t: GreenToken
            if (t.kind == TK_STR_CONTENT || t.kind == TK_PATH) &&
              markerRe.findFirstIn(t.text).isDefined =>
          splitTok(t, sp)
        case t: GreenToken => Vector[GreenChild](t)
    }
    GreenNode(n.kind, children)

  private def splitTok(t: GreenToken, sp: Vector[GreenNode]): Vector[GreenChild] =
    val out  = Vector.newBuilder[GreenChild]
    var last = 0
    for m <- markerRe.findAllMatchIn(t.text) do
      if m.start > last then out += GreenToken(t.kind, t.text.substring(last, m.start))
      out += Build.interp(sp(m.group(1).toInt))
      last = m.end
    if last < t.text.length then out += GreenToken(t.kind, t.text.substring(last))
    out.result()

  private def scanLeftover(n: GreenNode): Unit =
    n.children.foreach {
      case t: GreenToken if markerRe.findFirstIn(t.text).isDefined =>
        val where = t.kind match
          case TK_COMMENT => "inside a comment"
          case TK_URI     => "inside a URI literal (quote it as a string)"
          case TK_IDENT   => "fused into an identifier or in an unsupported binding position"
          case k          => s"in unsupported position ($k)"
        throw GraftError(s"nixq: splice $where")
      case c: GreenNode => scanLeftover(c)
      case _            => ()
    }
