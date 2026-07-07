package nixla.cst

/** Every token and node kind in the Nix concrete syntax tree.
  *
  * Tokens (TK_*) carry text; nodes (N_*) carry children. Trivia
  * (whitespace, comments) are ordinary tokens — that is what makes the tree
  * lossless by construction.
  */
enum SyntaxKind:
  // ---- trivia ----
  case TK_WHITESPACE, TK_COMMENT
  // ---- literals & names ----
  case TK_IDENT, TK_INT, TK_FLOAT, TK_URI
  case TK_PATH, TK_HPATH, TK_SPATH // ./x  ~/x  <nixpkgs>
  // ---- keywords ----
  case TK_KW_IF, TK_KW_THEN, TK_KW_ELSE, TK_KW_ASSERT, TK_KW_WITH
  case TK_KW_LET, TK_KW_IN, TK_KW_REC, TK_KW_INHERIT, TK_KW_OR
  // ---- string pieces ----
  case TK_DQUOTE                            // "
  case TK_IND_STR_OPEN, TK_IND_STR_CLOSE    // ''
  case TK_STR_CONTENT                       // raw content run (escapes verbatim)
  case TK_INTERP_OPEN                       // ${
  // ---- punctuation & operators ----
  case TK_LBRACE, TK_RBRACE, TK_LBRACK, TK_RBRACK, TK_LPAREN, TK_RPAREN
  case TK_SEMI, TK_COMMA, TK_DOT, TK_ELLIPSIS, TK_AT, TK_COLON, TK_QUESTION, TK_ASSIGN
  case TK_PLUS, TK_MINUS, TK_STAR, TK_SLASH, TK_CONCAT, TK_UPDATE, TK_NOT
  case TK_LT, TK_LE, TK_GT, TK_GE, TK_EQ, TK_NE, TK_AND, TK_OR_OP, TK_IMPL
  case TK_EOF
  // ---- nodes ----
  case N_ROOT
  case N_IDENT, N_LITERAL, N_PATH          // wrap a single token, so "expression = node"
  case N_STRING, N_IND_STRING, N_INTERP    // "…${…}…"  ''…''  ${…}
  case N_LIST, N_ATTRSET, N_BINDING, N_INHERIT, N_INHERIT_FROM
  case N_ATTRPATH, N_DYNAMIC               // a.b."c".${d} ; ${d}
  case N_SELECT, N_HAS_ATTR, N_APPLY, N_PAREN
  case N_LAMBDA, N_PATTERN, N_PAT_ENTRY, N_PAT_BIND
  case N_LET, N_IF, N_WITH, N_ASSERT
  case N_UNARY, N_BINOP
  case N_ERROR // reserved for error tolerance; unused while the parser is fail-fast

  def isTrivia: Boolean = this == TK_WHITESPACE || this == TK_COMMENT
  def isToken: Boolean  = ordinal <= TK_EOF.ordinal
  def isNode: Boolean   = !isToken
