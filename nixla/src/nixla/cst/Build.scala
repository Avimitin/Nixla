package nixla.cst

/** Construction of synthesized (trivia-free) green trees.
  *
  * Builders insert N_PAREN nodes wherever Nix's precedence demands them, so a
  * built tree is always structurally unambiguous. Built trees carry no
  * whitespace: `Pretty` is their serializer (`text` on a synthesized tree is
  * not meaningful output; see DESIGN.md).
  */
object Build:
  import SyntaxKind.*

  private def tk(k: SyntaxKind, s: String): GreenToken   = GreenToken(k, s)
  private def nd(k: SyntaxKind)(cs: GreenChild*): GreenNode = GreenNode(k, cs.toVector)

  private[cst] val binOpInfo: Map[String, (SyntaxKind, Int, Char)] = Map(
    "++" -> (TK_CONCAT, 5, 'R'),
    "*"  -> (TK_STAR, 6, 'L'), "/" -> (TK_SLASH, 6, 'L'),
    "+"  -> (TK_PLUS, 7, 'L'), "-" -> (TK_MINUS, 7, 'L'),
    "//" -> (TK_UPDATE, 9, 'R'),
    "<"  -> (TK_LT, 10, 'N'), "<=" -> (TK_LE, 10, 'N'),
    ">"  -> (TK_GT, 10, 'N'), ">=" -> (TK_GE, 10, 'N'),
    "==" -> (TK_EQ, 11, 'N'), "!=" -> (TK_NE, 11, 'N'),
    "&&" -> (TK_AND, 12, 'L'), "||" -> (TK_OR_OP, 13, 'L'),
    "->" -> (TK_IMPL, 14, 'R'))

  private val tokenPrec: Map[SyntaxKind, Int] =
    binOpInfo.values.map((k, p, _) => k -> p).toMap

  /** precedence of a green expression node; lower binds tighter */
  def prec(n: GreenNode): Int = n.kind match
    case N_IDENT | N_LITERAL | N_STRING | N_IND_STRING | N_PATH | N_LIST |
        N_ATTRSET | N_PAREN => 0
    case N_SELECT   => 1
    case N_APPLY    => 2
    case N_HAS_ATTR => 4
    case N_UNARY =>
      n.children.headOption match
        case Some(t: GreenToken) if t.kind == TK_MINUS => 3
        case _                                         => 8
    case N_BINOP =>
      n.children.collectFirst { case t: GreenToken if tokenPrec.contains(t.kind) => tokenPrec(t.kind) }
        .getOrElse(15)
    case _ => 15 // lambda / let / if / with / assert

  def paren(e: GreenNode): GreenNode =
    nd(N_PAREN)(tk(TK_LPAREN, "("), e, tk(TK_RPAREN, ")"))

  def parenIf(e: GreenNode, allowed: Int): GreenNode =
    if prec(e) > allowed then paren(e) else e

  // ---- atoms -------------------------------------------------------------
  def ident(name: String): GreenNode = nd(N_IDENT)(tk(TK_IDENT, name))
  def int(v: Long): GreenNode        = nd(N_LITERAL)(tk(TK_INT, v.toString))
  def float(v: Double): GreenNode    = nd(N_LITERAL)(tk(TK_FLOAT, v.toString))
  def bool(b: Boolean): GreenNode    = ident(b.toString) // true/false are plain identifiers in Nix
  def nullE: GreenNode               = ident("null")
  def path(p: String): GreenNode     = nd(N_PATH)(tk(TK_PATH, p))

  def escape(s: String): String = s.flatMap {
    case '"'  => "\\\""
    case '\\' => "\\\\"
    case '\n' => "\\n"
    case '\t' => "\\t"
    case '\r' => "\\r"
    case '$'  => "\\$" // conservative: literal text can never open an interpolation
    case c    => c.toString
  }

  def str(s: String): GreenNode =
    if s.isEmpty then nd(N_STRING)(tk(TK_DQUOTE, "\""), tk(TK_DQUOTE, "\""))
    else nd(N_STRING)(tk(TK_DQUOTE, "\""), tk(TK_STR_CONTENT, escape(s)), tk(TK_DQUOTE, "\""))

  def interp(e: GreenNode): GreenNode =
    nd(N_INTERP)(tk(TK_INTERP_OPEN, "${"), e, tk(TK_RBRACE, "}"))

  /** "…${…}…" — String parts are literal text, GreenNode parts interpolate */
  def strParts(parts: (String | GreenNode)*): GreenNode =
    val inner: Seq[GreenChild] = parts.collect {
      case s: String if s.nonEmpty => tk(TK_STR_CONTENT, escape(s))
      case e: GreenNode            => interp(e)
    }
    GreenNode(N_STRING, (tk(TK_DQUOTE, "\"") +: inner.toVector) :+ tk(TK_DQUOTE, "\""))

  // ---- compounds ----------------------------------------------------------
  def list(items: GreenNode*): GreenNode =
    val inner: Seq[GreenChild] = items.map(parenIf(_, 1)) // list elements bind at select level
    GreenNode(N_LIST, (tk(TK_LBRACK, "[") +: inner.toVector) :+ tk(TK_RBRACK, "]"))

  private val identRe = "[a-zA-Z_][a-zA-Z0-9_'-]*".r
  private def attrName(name: String): GreenChild =
    if identRe.matches(name) then tk(TK_IDENT, name) else str(name)

  def attrpath(names: String*): GreenNode =
    val cs = names.zipWithIndex.flatMap { (n, ix) =>
      val name = attrName(n)
      if ix == 0 then Vector(name) else Vector(tk(TK_DOT, "."), name)
    }
    GreenNode(N_ATTRPATH, cs.toVector)

  def dynAttr(e: GreenNode): GreenNode =
    nd(N_DYNAMIC)(tk(TK_INTERP_OPEN, "${"), e, tk(TK_RBRACE, "}"))

  def binding(path: Seq[String], value: GreenNode): GreenNode =
    nd(N_BINDING)(attrpath(path*), tk(TK_ASSIGN, "="), value, tk(TK_SEMI, ";"))

  def bindingDyn(name: GreenNode, value: GreenNode): GreenNode =
    nd(N_BINDING)(GreenNode(N_ATTRPATH, Vector(dynAttr(name))),
      tk(TK_ASSIGN, "="), value, tk(TK_SEMI, ";"))

  def inherit(names: String*): GreenNode =
    GreenNode(N_INHERIT,
      (tk(TK_KW_INHERIT, "inherit") +: names.toVector.map(attrName)) :+ tk(TK_SEMI, ";"))

  def inheritFrom(from: GreenNode, names: String*): GreenNode =
    val fromNode = nd(N_INHERIT_FROM)(tk(TK_LPAREN, "("), from, tk(TK_RPAREN, ")"))
    GreenNode(N_INHERIT,
      (Vector[GreenChild](tk(TK_KW_INHERIT, "inherit"), fromNode) ++ names.map(attrName)) :+ tk(TK_SEMI, ";"))

  def attrs(bindings: GreenNode*): GreenNode =
    GreenNode(N_ATTRSET, (tk(TK_LBRACE, "{") +: bindings.toVector) :+ tk(TK_RBRACE, "}"))

  def recAttrs(bindings: GreenNode*): GreenNode =
    GreenNode(N_ATTRSET,
      (Vector[GreenChild](tk(TK_KW_REC, "rec"), tk(TK_LBRACE, "{")) ++ bindings) :+ tk(TK_RBRACE, "}"))

  // ---- functions -----------------------------------------------------------
  def lambda(param: String, body: GreenNode): GreenNode =
    nd(N_LAMBDA)(ident(param), tk(TK_COLON, ":"), body)

  def patternLambda(
      fields: Seq[(String, Option[GreenNode])],
      ellipsis: Boolean,
      bound: Option[String],
      body: GreenNode
  ): GreenNode =
    val entries = fields.map { (n, d) =>
      d match
        case None    => nd(N_PAT_ENTRY)(tk(TK_IDENT, n))
        case Some(v) => nd(N_PAT_ENTRY)(tk(TK_IDENT, n), tk(TK_QUESTION, "?"), v)
    }
    val inner = entries.flatMap(e => Vector[GreenChild](e, tk(TK_COMMA, ","))) match
      case cs if ellipsis => cs :+ tk(TK_ELLIPSIS, "...")
      case cs             => cs.dropRight(1) // no trailing comma
    val braced = (tk(TK_LBRACE, "{") +: inner.toVector) :+ tk(TK_RBRACE, "}")
    val patCs = bound match
      case Some(b) => braced ++ Vector[GreenChild](nd(N_PAT_BIND)(tk(TK_AT, "@"), tk(TK_IDENT, b)))
      case None    => braced
    nd(N_LAMBDA)(GreenNode(N_PATTERN, patCs), tk(TK_COLON, ":"), body)

  def app(f: GreenNode, arg: GreenNode): GreenNode =
    nd(N_APPLY)(parenIf(f, 2), parenIf(arg, 1))

  def app(f: GreenNode, args: GreenNode*): GreenNode =
    args.foldLeft(f)(app)

  def select(e: GreenNode, names: String*): GreenNode =
    nd(N_SELECT)(parenIf(e, 1), tk(TK_DOT, "."), attrpath(names*))

  def selectOr(e: GreenNode, names: Seq[String], default: GreenNode): GreenNode =
    nd(N_SELECT)(parenIf(e, 1), tk(TK_DOT, "."), attrpath(names*),
      tk(TK_KW_OR, "or"), parenIf(default, 1))

  def hasAttr(e: GreenNode, names: String*): GreenNode =
    nd(N_HAS_ATTR)(parenIf(e, 3), tk(TK_QUESTION, "?"), attrpath(names*))

  def unary(op: String, e: GreenNode): GreenNode =
    val (k, p) = if op == "-" then (TK_MINUS, 3) else (TK_NOT, 8)
    nd(N_UNARY)(tk(k, op), parenIf(e, p - 1))

  def binop(op: String, l: GreenNode, r: GreenNode): GreenNode =
    val (k, p, assoc) = binOpInfo(op)
    val lp            = if assoc == 'L' then p else p - 1
    val rp            = if assoc == 'R' then p else p - 1
    nd(N_BINOP)(parenIf(l, lp), tk(k, op), parenIf(r, rp))

  // ---- keyword forms --------------------------------------------------------
  def letIn(bindings: Seq[GreenNode], body: GreenNode): GreenNode =
    GreenNode(N_LET,
      (tk(TK_KW_LET, "let") +: bindings.toVector) ++
        Vector[GreenChild](tk(TK_KW_IN, "in"), body))

  def ifThenElse(c: GreenNode, t: GreenNode, e: GreenNode): GreenNode =
    nd(N_IF)(tk(TK_KW_IF, "if"), c, tk(TK_KW_THEN, "then"), t, tk(TK_KW_ELSE, "else"), e)

  def withE(env: GreenNode, body: GreenNode): GreenNode =
    nd(N_WITH)(tk(TK_KW_WITH, "with"), env, tk(TK_SEMI, ";"), body)

  def assertE(cond: GreenNode, body: GreenNode): GreenNode =
    nd(N_ASSERT)(tk(TK_KW_ASSERT, "assert"), cond, tk(TK_SEMI, ";"), body)
