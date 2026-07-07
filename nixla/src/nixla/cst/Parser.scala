package nixla.cst

import scala.collection.mutable.ArrayBuffer

/** Recursive-descent parser producing lossless green trees.
  *
  * Conventions (see DESIGN.md):
  *  - every parse method is entered with the cursor on a significant
  *    (non-trivia) token; callers absorb leading trivia into their own node;
  *  - within a node, trivia preceding each expected token belongs to it;
  *  - trailing trivia after a node's last token is left for an ancestor;
  *  - fail-fast: ParseError with the offending token's offset;
  *  - backtracking (lambda pattern vs attrset) resets the token index —
  *    sound because green construction is pure.
  */
object Parser:
  def parse(src: String): SyntaxNode =
    new SyntaxNode(parseGreen(src), None, 0)

  def parseGreen(src: String): GreenNode =
    new Parser(Lexer.lex(src)).parseRoot()

final class Parser private (tokens: Vector[Token]):
  import SyntaxKind.*

  private type Buf = ArrayBuffer[GreenChild]
  private var i = 0

  private def cur: Token                = tokens(i)
  private def err(msg: String): Nothing = throw ParseError(msg, cur.offset)

  private def green(t: Token): GreenToken   = GreenToken(t.kind, t.text)
  private def node(k: SyntaxKind, buf: Buf) = GreenNode(k, buf.toVector)

  private def absorb(buf: Buf): Unit =
    while cur.isTrivia do { buf += green(cur); i += 1 }

  private def bump(buf: Buf): Unit = { buf += green(cur); i += 1 }

  private def found: String =
    if cur.kind == SyntaxKind.TK_EOF then "end of input" else s"'${cur.text}'"

  private def expect(k: SyntaxKind, what: String, buf: Buf): Unit =
    absorb(buf)
    if cur.kind != k then err(s"expected $what, found $found")
    bump(buf)

  /** index of the next significant token at or after `from` */
  private def sig(from: Int): Int =
    var j = from
    while tokens(j).isTrivia do j += 1
    j

  // ======================================================================
  def parseRoot(): GreenNode =
    val buf = ArrayBuffer.empty[GreenChild]
    absorb(buf)
    buf += parseExpr()
    absorb(buf)
    if cur.kind != TK_EOF then err("trailing input")
    node(N_ROOT, buf)

  /** full expression: keyword forms, lambdas, then the operator ladder */
  private def parseExpr(): GreenNode = cur.kind match
    case TK_KW_LET    => parseLet()
    case TK_KW_IF     => parseIf()
    case TK_KW_WITH   => parseKwSemi(TK_KW_WITH, "with", N_WITH)
    case TK_KW_ASSERT => parseKwSemi(TK_KW_ASSERT, "assert", N_ASSERT)
    case TK_IDENT if tokens(sig(i + 1)).kind == TK_COLON => parseLambda()
    case TK_IDENT if tokens(sig(i + 1)).kind == TK_AT    => parseLambda()
    case TK_LBRACE =>
      tryParseLambda() match
        case Some(lam) => lam
        case None      => parseOp(14)
    case _ => parseOp(14)

  private def parseKwSemi(kw: SyntaxKind, name: String, kind: SyntaxKind): GreenNode =
    val buf = ArrayBuffer.empty[GreenChild]
    bump(buf) // keyword
    absorb(buf)
    buf += parseExpr()
    expect(TK_SEMI, s"';' after $name", buf)
    absorb(buf)
    buf += parseExpr()
    node(kind, buf)

  private def parseLet(): GreenNode =
    val buf = ArrayBuffer.empty[GreenChild]
    bump(buf) // let
    absorb(buf)
    while cur.kind != TK_KW_IN do
      buf += parseBinding()
      absorb(buf)
    bump(buf) // in
    absorb(buf)
    buf += parseExpr()
    node(N_LET, buf)

  private def parseIf(): GreenNode =
    val buf = ArrayBuffer.empty[GreenChild]
    bump(buf) // if
    absorb(buf)
    buf += parseExpr()
    expect(TK_KW_THEN, "'then'", buf)
    absorb(buf)
    buf += parseExpr()
    expect(TK_KW_ELSE, "'else'", buf)
    absorb(buf)
    buf += parseExpr()
    node(N_IF, buf)

  // ---- lambdas ---------------------------------------------------------
  /** entered on IDENT (with `:` or `@` next) or via tryParseLambda on `{` */
  private def parseLambda(): GreenNode =
    val buf = ArrayBuffer.empty[GreenChild]
    if cur.kind == TK_IDENT && tokens(sig(i + 1)).kind == TK_COLON then
      val id = ArrayBuffer.empty[GreenChild]
      bump(id)
      buf += node(N_IDENT, id)
    else if cur.kind == TK_IDENT then // IDENT @ { pattern }
      val bind = ArrayBuffer.empty[GreenChild]
      bump(bind) // ident
      absorb(bind)
      bump(bind) // @
      val pat = ArrayBuffer.empty[GreenChild]
      pat += node(N_PAT_BIND, bind)
      absorb(pat)
      if cur.kind != TK_LBRACE then err("expected '{' after '@'")
      parsePatternInto(pat)
      buf += node(N_PATTERN, pat)
    else // { pattern } [@ ident]
      val pat = ArrayBuffer.empty[GreenChild]
      parsePatternInto(pat)
      val save = i
      val tmp  = ArrayBuffer.empty[GreenChild]
      absorb(tmp)
      if cur.kind == TK_AT then
        val bind = ArrayBuffer.empty[GreenChild]
        bump(bind) // @
        absorb(bind)
        if cur.kind != TK_IDENT then err("expected identifier after '@'")
        bump(bind)
        pat ++= tmp
        pat += node(N_PAT_BIND, bind)
      else i = save
      buf += node(N_PATTERN, pat)
    expect(TK_COLON, "':' after lambda parameter", buf)
    absorb(buf)
    buf += parseExpr()
    node(N_LAMBDA, buf)

  /** consumes `{ a, b ? default, ... }` into `pat`; throws on malformed */
  private def parsePatternInto(pat: Buf): Unit =
    bump(pat) // {
    absorb(pat)
    var done = cur.kind == TK_RBRACE
    while !done do
      if cur.kind == TK_ELLIPSIS then { bump(pat); done = true }
      else
        val entry = ArrayBuffer.empty[GreenChild]
        if cur.kind != TK_IDENT then err("expected identifier in pattern")
        bump(entry)
        val save = i
        val tmp  = ArrayBuffer.empty[GreenChild]
        absorb(tmp)
        if cur.kind == TK_QUESTION then
          entry ++= tmp
          bump(entry) // ?
          absorb(entry)
          entry += parseExpr()
        else i = save
        pat += node(N_PAT_ENTRY, entry)
        absorb(pat)
        if cur.kind == TK_COMMA then { bump(pat); absorb(pat) }
        else done = true
      absorb(pat)
    expect(TK_RBRACE, "'}' closing pattern", pat)

  /** distinguish `{ x, y }: e` (lambda) from `{ x = 1; }` (attrset) */
  private def tryParseLambda(): Option[GreenNode] =
    val save = i
    // cheap scan: a pattern's `{` is followed by `}`, `...`, or IDENT with
    // `,`/`?`/`}` after it — anything else is an attrset, skip the attempt
    val a = sig(i + 1)
    val plausible = tokens(a).kind match
      case TK_RBRACE   => true
      case TK_ELLIPSIS => true
      case TK_IDENT =>
        tokens(sig(a + 1)).kind match
          case TK_COMMA | TK_QUESTION | TK_RBRACE => true
          case _                                  => false
      case _ => false
    if !plausible then return None
    try
      val lam = parseLambda()
      Some(lam)
    catch
      case ParseError(_, _) => { i = save; None }

  // ---- operator ladder (precedence per the Nix manual; lower = tighter) --
  private val binOps: Map[SyntaxKind, (Int, Char)] = Map(
    TK_CONCAT -> (5, 'R'),
    TK_STAR   -> (6, 'L'), TK_SLASH -> (6, 'L'),
    TK_PLUS   -> (7, 'L'), TK_MINUS -> (7, 'L'),
    TK_UPDATE -> (9, 'R'),
    TK_LT -> (10, 'N'), TK_LE -> (10, 'N'), TK_GT -> (10, 'N'), TK_GE -> (10, 'N'),
    TK_EQ -> (11, 'N'), TK_NE -> (11, 'N'),
    TK_AND -> (12, 'L'), TK_OR_OP -> (13, 'L'), TK_IMPL -> (14, 'R'))

  private def parseOp(p: Int): GreenNode = p match
    case 1 => parseSelect()
    case 2 => parseApply()
    case 3 => // unary minus
      if cur.kind == TK_MINUS then
        val buf = ArrayBuffer.empty[GreenChild]
        bump(buf)
        absorb(buf)
        buf += parseOp(3)
        node(N_UNARY, buf)
      else parseOp(2)
    case 4 => // has-attr `e ? a.b` — non-associative
      var l    = parseOp(3)
      val save = i
      val tmp  = ArrayBuffer.empty[GreenChild]
      absorb(tmp)
      if cur.kind == TK_QUESTION then
        val buf = ArrayBuffer.empty[GreenChild]
        buf += l
        buf ++= tmp
        bump(buf) // ?
        absorb(buf)
        buf += parseAttrPath()
        l = node(N_HAS_ATTR, buf)
      else i = save
      l
    case 8 => // logical not
      if cur.kind == TK_NOT then
        val buf = ArrayBuffer.empty[GreenChild]
        bump(buf)
        absorb(buf)
        buf += parseOp(8)
        node(N_UNARY, buf)
      else parseOp(7)
    case p =>
      var l     = parseOp(p - 1)
      var going = true
      while going do
        val save = i
        val tmp  = ArrayBuffer.empty[GreenChild]
        absorb(tmp)
        binOps.get(cur.kind) match
          case Some((`p`, assoc)) =>
            val buf = ArrayBuffer.empty[GreenChild]
            buf += l
            buf ++= tmp
            bump(buf) // operator
            absorb(buf)
            buf += (if assoc == 'R' then parseOp(p) else parseOp(p - 1))
            l = node(N_BINOP, buf)
            if assoc != 'L' then going = false
          case _ =>
            i = save
            going = false
      l

  private def startsAtom: Boolean = cur.kind match
    case TK_IDENT | TK_INT | TK_FLOAT | TK_URI | TK_PATH | TK_HPATH | TK_SPATH |
        TK_DQUOTE | TK_IND_STR_OPEN | TK_LPAREN | TK_LBRACK | TK_LBRACE | TK_KW_REC => true
    case _ => false

  private def parseApply(): GreenNode =
    var l     = parseSelect()
    var going = true
    while going do
      val save = i
      val tmp  = ArrayBuffer.empty[GreenChild]
      absorb(tmp)
      if startsAtom then
        val buf = ArrayBuffer.empty[GreenChild]
        buf += l
        buf ++= tmp
        buf += parseSelect()
        l = node(N_APPLY, buf)
      else if cur.kind == TK_KW_OR then
        // legacy: `f or` applies the identifier `or` (contextual keyword)
        val buf = ArrayBuffer.empty[GreenChild]
        buf += l
        buf ++= tmp
        val id = ArrayBuffer.empty[GreenChild]
        bump(id)
        buf += node(N_IDENT, id)
        l = node(N_APPLY, buf)
      else
        i = save
        going = false
    l

  private def parseSelect(): GreenNode =
    val atom = parseAtom()
    val save = i
    val tmp  = ArrayBuffer.empty[GreenChild]
    absorb(tmp)
    if cur.kind == TK_DOT then
      val buf = ArrayBuffer.empty[GreenChild]
      buf += atom
      buf ++= tmp
      bump(buf) // .
      absorb(buf)
      buf += parseAttrPath()
      // optional `or default`; the default binds at select level
      val save2 = i
      val tmp2  = ArrayBuffer.empty[GreenChild]
      absorb(tmp2)
      if cur.kind == TK_KW_OR then
        buf ++= tmp2
        bump(buf) // or
        absorb(buf)
        buf += parseSelect()
      else i = save2
      node(N_SELECT, buf)
    else
      i = save
      atom

  private def parseAttrPath(): GreenNode =
    val buf = ArrayBuffer.empty[GreenChild]
    parseAttrName(buf)
    var going = true
    while going do
      val save = i
      val tmp  = ArrayBuffer.empty[GreenChild]
      absorb(tmp)
      if cur.kind == TK_DOT then
        buf ++= tmp
        bump(buf) // .
        absorb(buf)
        parseAttrName(buf)
      else
        i = save
        going = false
    node(N_ATTRPATH, buf)

  private def parseAttrName(buf: Buf): Unit = cur.kind match
    case TK_IDENT | TK_KW_OR => bump(buf)
    case TK_DQUOTE           => buf += parseString()
    case TK_INTERP_OPEN =>
      val dyn = ArrayBuffer.empty[GreenChild]
      bump(dyn) // ${
      absorb(dyn)
      dyn += parseExpr()
      expect(TK_RBRACE, "'}' closing dynamic attribute", dyn)
      buf += node(N_DYNAMIC, dyn)
    case _ => err("expected attribute name")

  private def parseAtom(): GreenNode = cur.kind match
    case TK_IDENT =>
      val buf = ArrayBuffer.empty[GreenChild]
      bump(buf)
      node(N_IDENT, buf)
    case TK_INT | TK_FLOAT | TK_URI =>
      val buf = ArrayBuffer.empty[GreenChild]
      bump(buf)
      node(N_LITERAL, buf)
    case TK_PATH | TK_HPATH =>
      val buf = ArrayBuffer.empty[GreenChild]
      bump(buf)
      // interpolated path pieces are adjacent by lexer construction — no
      // trivia can separate them, so no absorb here
      while cur.kind == TK_PATH || cur.kind == TK_INTERP_OPEN do
        if cur.kind == TK_PATH then bump(buf) else buf += parseInterp()
      node(N_PATH, buf)
    case TK_SPATH =>
      val buf = ArrayBuffer.empty[GreenChild]
      bump(buf)
      node(N_PATH, buf)
    case TK_DQUOTE       => parseString()
    case TK_IND_STR_OPEN => parseIndString()
    case TK_LPAREN =>
      val buf = ArrayBuffer.empty[GreenChild]
      bump(buf)
      absorb(buf)
      buf += parseExpr()
      expect(TK_RPAREN, "')'", buf)
      node(N_PAREN, buf)
    case TK_LBRACK =>
      val buf = ArrayBuffer.empty[GreenChild]
      bump(buf)
      absorb(buf)
      while cur.kind != TK_RBRACK do
        buf += parseSelect() // list elements bind at select level
        absorb(buf)
      bump(buf) // ]
      node(N_LIST, buf)
    case TK_KW_REC =>
      val buf = ArrayBuffer.empty[GreenChild]
      bump(buf) // rec
      absorb(buf)
      if cur.kind != TK_LBRACE then err("expected '{' after rec")
      parseAttrSetInto(buf)
      node(N_ATTRSET, buf)
    case TK_LBRACE =>
      val buf = ArrayBuffer.empty[GreenChild]
      parseAttrSetInto(buf)
      node(N_ATTRSET, buf)
    case _ => err("expected expression")

  private def parseAttrSetInto(buf: Buf): Unit =
    bump(buf) // {
    absorb(buf)
    while cur.kind != TK_RBRACE do
      buf += parseBinding()
      absorb(buf)
    bump(buf) // }

  private def parseBinding(): GreenNode =
    if cur.kind == TK_KW_INHERIT then
      val buf = ArrayBuffer.empty[GreenChild]
      bump(buf) // inherit
      absorb(buf)
      if cur.kind == TK_LPAREN then
        val from = ArrayBuffer.empty[GreenChild]
        bump(from) // (
        absorb(from)
        from += parseExpr()
        expect(TK_RPAREN, "')'", from)
        buf += node(N_INHERIT_FROM, from)
        absorb(buf)
      while cur.kind != TK_SEMI do
        cur.kind match
          case TK_IDENT | TK_KW_OR => bump(buf)
          case TK_DQUOTE           => buf += parseString()
          case _                   => err("expected name in inherit")
        absorb(buf)
      bump(buf) // ;
      node(N_INHERIT, buf)
    else
      val buf = ArrayBuffer.empty[GreenChild]
      buf += parseAttrPath()
      expect(TK_ASSIGN, "'='", buf)
      absorb(buf)
      buf += parseExpr()
      expect(TK_SEMI, "';'", buf)
      node(N_BINDING, buf)

  // ---- strings (no trivia inside; lexer guarantees the token shapes) ----
  private def parseString(): GreenNode =
    val buf = ArrayBuffer.empty[GreenChild]
    bump(buf) // "
    while cur.kind != TK_DQUOTE do
      cur.kind match
        case TK_STR_CONTENT => bump(buf)
        case TK_INTERP_OPEN => buf += parseInterp()
        case _              => err("unterminated string")
    bump(buf) // "
    node(N_STRING, buf)

  private def parseIndString(): GreenNode =
    val buf = ArrayBuffer.empty[GreenChild]
    bump(buf) // ''
    while cur.kind != TK_IND_STR_CLOSE do
      cur.kind match
        case TK_STR_CONTENT => bump(buf)
        case TK_INTERP_OPEN => buf += parseInterp()
        case _              => err("unterminated indented string")
    bump(buf) // ''
    node(N_IND_STRING, buf)

  private def parseInterp(): GreenNode =
    val buf = ArrayBuffer.empty[GreenChild]
    bump(buf) // ${
    absorb(buf)
    buf += parseExpr()
    expect(TK_RBRACE, "'}' closing interpolation", buf)
    node(N_INTERP, buf)
