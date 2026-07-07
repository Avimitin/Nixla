package nixla.cst

/** Canonical printer: a pure function of the green tree.
  *
  * Contract (DESIGN.md): deterministic, documented style, stable within a
  * nixla version — deliberately NOT chasing nixfmt-rfc-style. Whitespace
  * trivia is regenerated; comments are preserved (own line in block
  * contexts, inline elsewhere). Parenthesization is structural: N_PAREN
  * nodes exist in the tree (parser keeps them; builders insert them), the
  * printer never invents or drops parens.
  */
object Pretty:
  import SyntaxKind.*

  private val IND = "  "

  def apply(n: GreenNode): String = n.kind match
    case N_ROOT =>
      val exprs = n.children.collect {
        case c: GreenNode                                 => go(c, 0)
        case t: GreenToken if t.kind == TK_COMMENT        => t.text
      }
      exprs.mkString("\n") + "\n"
    case _ => go(n, 0) + "\n"

  private def pad(ind: Int) = IND * ind

  /** significant children: whitespace dropped, comments kept */
  private def sig(n: GreenNode): Vector[GreenChild] = n.children.filter {
    case t: GreenToken => t.kind != TK_WHITESPACE
    case _             => true
  }

  private def nodes(n: GreenNode): Vector[GreenNode] =
    n.children.collect { case c: GreenNode => c }

  private def hasToken(n: GreenNode, k: SyntaxKind): Boolean =
    n.children.exists { case t: GreenToken => t.kind == k; case _ => false }

  private def comments(n: GreenNode): Vector[GreenToken] =
    n.children.collect { case t: GreenToken if t.kind == TK_COMMENT => t }

  private def go(n: GreenNode, ind: Int): String = n.kind match
    case N_IDENT | N_LITERAL =>
      n.children.collect { case t: GreenToken => t.text }.mkString

    case N_PATH =>
      sig(n).map {
        case t: GreenToken => t.text
        case c: GreenNode  => go(c, ind)
      }.mkString

    case N_STRING | N_IND_STRING =>
      // content tokens verbatim; nested expressions pretty-printed
      n.children.map {
        case t: GreenToken => t.text
        case c: GreenNode  => go(c, ind)
      }.mkString

    case N_INTERP | N_DYNAMIC =>
      s"$${${goExpr(n, ind)}}"

    case N_ATTRPATH =>
      sig(n).map {
        case t: GreenToken => t.text
        case c: GreenNode  => go(c, ind)
      }.mkString

    case N_LIST =>
      val items = nodes(n)
      val cs    = comments(n)
      if items.isEmpty && cs.isEmpty then "[ ]"
      else if cs.isEmpty && items.sizeIs <= 3 && items.forall(small) then
        items.map(go(_, ind)).mkString("[ ", " ", " ]")
      else
        val body = sig(n).drop(1).dropRight(1).map {
          case t: GreenToken => pad(ind + 1) + t.text // comment
          case c: GreenNode  => pad(ind + 1) + go(c, ind + 1)
        }
        s"[\n${body.mkString("\n")}\n${pad(ind)}]"

    case N_ATTRSET =>
      val kw       = if hasToken(n, TK_KW_REC) then "rec " else ""
      val bindings = sig(n).filter {
        case t: GreenToken => t.kind == TK_COMMENT
        case _             => true
      }.collect {
        case t: GreenToken if t.kind == TK_COMMENT => pad(ind + 1) + t.text
        case c: GreenNode                          => pad(ind + 1) + go(c, ind + 1)
      }
      if bindings.isEmpty then s"$kw{ }"
      else s"$kw{\n${bindings.mkString("\n")}\n${pad(ind)}}"

    case N_BINDING =>
      val ns = nodes(n)
      s"${go(ns.head, ind)} = ${go(ns.last, ind)};"

    case N_INHERIT =>
      val parts = sig(n).init.map { // drop the ';'
        case t: GreenToken => t.text
        case c: GreenNode  => go(c, ind)
      }
      parts.mkString(" ") + ";"

    case N_INHERIT_FROM =>
      s"(${goExpr(n, ind)})"

    case N_SELECT =>
      val ns   = nodes(n)
      val base = s"${go(ns(0), ind)}.${go(ns(1), ind)}"
      if ns.sizeIs > 2 then s"$base or ${go(ns(2), ind)}" else base

    case N_HAS_ATTR =>
      val ns = nodes(n)
      s"${go(ns(0), ind)} ? ${go(ns(1), ind)}"

    case N_APPLY =>
      nodes(n).map(go(_, ind)).mkString(" ")

    case N_LAMBDA =>
      val ns    = nodes(n)
      val param = go(ns.head, ind)
      val body  = ns.last
      body.kind match
        case N_LAMBDA | N_ATTRSET | N_LET =>
          s"$param:\n${pad(ind + 1)}${go(body, ind + 1)}"
        case _ => s"$param: ${go(body, ind)}"

    case N_PATTERN =>
      val binds   = nodes(n).filter(_.kind == N_PAT_BIND)
      val entries = nodes(n).filter(_.kind == N_PAT_ENTRY).map(go(_, ind))
      val ellipsis = hasToken(n, TK_ELLIPSIS)
      val inner    = (entries ++ (if ellipsis then Vector("...") else Vector.empty))
        .mkString(", ")
      val braced = if inner.isEmpty then "{ }" else s"{ $inner }"
      // leading `x @` vs trailing `@ x` — recover from child order
      n.children.headOption match
        case Some(b: GreenNode) if b.kind == N_PAT_BIND => s"${patBindName(b)} @ $braced"
        case _ =>
          binds.headOption match
            case Some(b) => s"$braced @ ${patBindName(b)}"
            case None    => braced

    case N_PAT_ENTRY =>
      val name = n.children.collectFirst { case t: GreenToken if t.kind == TK_IDENT => t.text }.get
      nodes(n).headOption match
        case Some(d) => s"$name ? ${go(d, ind)}"
        case None    => name

    case N_LET =>
      val bs = sig(n).collect {
        case t: GreenToken if t.kind == TK_COMMENT     => pad(ind + 1) + t.text
        case c: GreenNode if c.kind == N_BINDING || c.kind == N_INHERIT =>
          pad(ind + 1) + go(c, ind + 1)
      }
      val body = nodes(n).last
      s"let\n${bs.mkString("\n")}\n${pad(ind)}in\n${pad(ind + 1)}${go(body, ind + 1)}"

    case N_IF =>
      val ns = nodes(n)
      s"if ${go(ns(0), ind)} then ${go(ns(1), ind)} else ${go(ns(2), ind)}"

    case N_WITH =>
      val ns = nodes(n)
      s"with ${go(ns(0), ind)}; ${go(ns(1), ind)}"

    case N_ASSERT =>
      val ns = nodes(n)
      s"assert ${go(ns(0), ind)}; ${go(ns(1), ind)}"

    case N_UNARY =>
      val op = n.children.collectFirst {
        case t: GreenToken if t.kind == TK_MINUS || t.kind == TK_NOT => t.text
      }.get
      s"$op${go(nodes(n).head, ind)}"

    case N_BINOP =>
      val ns = nodes(n)
      val op = n.children.collectFirst {
        case t: GreenToken if !t.kind.isTrivia && t.kind != TK_EOF && Build.binOpInfo.values.exists(_._1 == t.kind) => t.text
      }.get
      s"${go(ns(0), ind)} $op ${go(ns(1), ind)}"

    case N_PAREN =>
      s"(${goExpr(n, ind)})"

    case other =>
      throw IllegalStateException(s"Pretty: unexpected node kind $other")

  /** the single expression child of wrapper nodes (paren, interp, dynamic) */
  private def goExpr(n: GreenNode, ind: Int): String =
    go(nodes(n).head, ind)

  private def patBindName(b: GreenNode): String =
    b.children.collectFirst { case t: GreenToken if t.kind == TK_IDENT => t.text }.get

  private def small(n: GreenNode): Boolean = n.kind match
    case N_IDENT | N_LITERAL => true
    case N_PATH              => nodes(n).isEmpty
    case N_STRING            => n.width <= 32 && nodes(n).isEmpty
    case N_SELECT            => nodes(n).sizeIs <= 2 && small(nodes(n).head)
    case _                   => false
