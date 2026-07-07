package nixla.cst

import java.util.regex.Pattern
import scala.collection.mutable.ArrayBuffer

final case class Token(kind: SyntaxKind, text: String, offset: Int):
  def isTrivia: Boolean = kind.isTrivia

/** Mode-stack lexer, faithful to Nix's flex rules: at each position the
  * longest match wins (this is what makes `a/b` a path, `a / b` a division,
  * and `f:x` a URI — exactly as Nix lexes them).
  *
  * Modes: Default (with brace depth), inside "…" strings, inside ''…''
  * indented strings. `${` pushes a Default frame; its matching `}` pops back.
  */
object Lexer:
  import SyntaxKind.*

  private val keywords = Map(
    "if" -> TK_KW_IF, "then" -> TK_KW_THEN, "else" -> TK_KW_ELSE,
    "assert" -> TK_KW_ASSERT, "with" -> TK_KW_WITH, "let" -> TK_KW_LET,
    "in" -> TK_KW_IN, "rec" -> TK_KW_REC, "inherit" -> TK_KW_INHERIT,
    "or" -> TK_KW_OR)

  // regexes transcribed from nix's lexer.l (order here = tie-break priority)
  private val table: List[(Pattern, SyntaxKind)] = List(
    Pattern.compile("""[ \t\r\n]+""")                                   -> TK_WHITESPACE,
    Pattern.compile("""(?s)#[^\n\r]*|/\*.*?\*/""")                      -> TK_COMMENT,
    Pattern.compile("""(([1-9][0-9]*\.[0-9]*)|(0?\.[0-9]+))([Ee][+-]?[0-9]+)?""") -> TK_FLOAT,
    Pattern.compile("""[0-9]+""")                                       -> TK_INT,
    Pattern.compile("""[a-zA-Z0-9._+\-]*(/[a-zA-Z0-9._+\-]+)+""")       -> TK_PATH,
    Pattern.compile("""~(/[a-zA-Z0-9._+\-]+)+""")                       -> TK_HPATH,
    Pattern.compile("""<[a-zA-Z0-9._+\-]+(/[a-zA-Z0-9._+\-]+)*>""")     -> TK_SPATH,
    Pattern.compile("""[a-zA-Z][a-zA-Z0-9+.\-]*:[a-zA-Z0-9%/?:@&=+$,_.!~*'\-]+""") -> TK_URI,
    Pattern.compile("""[a-zA-Z_][a-zA-Z0-9_'\-]*""")                    -> TK_IDENT)

  // fixed-text operators, longest first within each starting character
  private val operators: List[(String, SyntaxKind)] = List(
    "..." -> TK_ELLIPSIS, "//" -> TK_UPDATE, "++" -> TK_CONCAT,
    "->" -> TK_IMPL, "<=" -> TK_LE, ">=" -> TK_GE, "==" -> TK_EQ,
    "!=" -> TK_NE, "&&" -> TK_AND, "||" -> TK_OR_OP,
    "." -> TK_DOT, ";" -> TK_SEMI, "," -> TK_COMMA, ":" -> TK_COLON,
    "?" -> TK_QUESTION, "@" -> TK_AT, "=" -> TK_ASSIGN,
    "(" -> TK_LPAREN, ")" -> TK_RPAREN, "[" -> TK_LBRACK, "]" -> TK_RBRACK,
    "+" -> TK_PLUS, "-" -> TK_MINUS, "*" -> TK_STAR, "/" -> TK_SLASH,
    "<" -> TK_LT, ">" -> TK_GT, "!" -> TK_NOT)

  private enum Frame:
    case Default(depth: Int)
    case Str
    case IndStr

  def lex(src: String): Vector[Token] =
    import Frame.*
    val out   = ArrayBuffer.empty[Token]
    var pos   = 0
    var stack = List[Frame](Default(0))

    def err(msg: String): Nothing = throw ParseError(msg, pos)
    def emit(kind: SyntaxKind, text: String): Unit = {
      out += Token(kind, text, pos)
      pos += text.length
    }
    def startsWith(s: String): Boolean = src.startsWith(s, pos)

    def lexDefault(depth: Int): Unit =
      if startsWith("${") then {
        emit(TK_INTERP_OPEN, "${")
        stack = Default(0) :: stack
      } else if startsWith("''") then {
        emit(TK_IND_STR_OPEN, "''")
        stack = IndStr :: stack
      } else if startsWith("\"") then {
        emit(TK_DQUOTE, "\"")
        stack = Str :: stack
      } else if startsWith("{") then {
        emit(TK_LBRACE, "{")
        stack = Default(depth + 1) :: stack.tail
      } else if startsWith("}") then {
        if depth > 0 then {
          emit(TK_RBRACE, "}")
          stack = Default(depth - 1) :: stack.tail
        } else if stack.tail.nonEmpty then {
          emit(TK_RBRACE, "}") // closes a ${…} interpolation
          stack = stack.tail
        } else err("unbalanced '}'")
      } else {
        // longest match across the regex table, ties broken by table order
        var bestLen  = 0
        var bestKind = TK_EOF
        for (p, kind) <- table do {
          val m = p.matcher(src)
          m.region(pos, src.length)
          if m.lookingAt() && m.end - pos > bestLen then {
            bestLen = m.end - pos
            bestKind = kind
          }
        }
        val opHit = operators.find((s, _) => startsWith(s))
        opHit match
          case Some((s, k)) if s.length >= bestLen => emit(k, s)
          case _ if bestLen > 0 =>
            val text = src.substring(pos, pos + bestLen)
            val kind = if bestKind == TK_IDENT then keywords.getOrElse(text, TK_IDENT) else bestKind
            emit(kind, text)
          case _ => err(s"unexpected character '${src(pos)}'")
      }

    def lexStr(): Unit =
      val start = pos
      val sb    = new StringBuilder
      var done  = false
      while !done do {
        if pos + sb.length >= src.length then err("unterminated string")
        val at = start + sb.length
        if src.startsWith("\"", at) || (src.startsWith("${", at)) then done = true
        else if src.startsWith("\\", at) then {
          if at + 1 >= src.length then err("unterminated escape")
          sb.append(src.substring(at, at + 2))
        } else sb.append(src(at))
      }
      if sb.nonEmpty then emit(TK_STR_CONTENT, sb.toString)
      if startsWith("\"") then {
        emit(TK_DQUOTE, "\"")
        stack = stack.tail
      } else {
        emit(TK_INTERP_OPEN, "${")
        stack = Frame.Default(0) :: stack
      }

    def lexIndStr(): Unit =
      val start = pos
      val sb    = new StringBuilder
      var done  = false
      while !done do {
        val at = start + sb.length
        if at >= src.length then err("unterminated indented string")
        if src.startsWith("''", at) then {
          // ''' -> escaped ''  |  ''$ -> escaped $  |  ''\X -> escaped X  |  '' -> close
          if src.startsWith("'''", at) then sb.append("'''")
          else if src.startsWith("''$", at) then sb.append("''$")
          else if src.startsWith("''\\", at) then {
            if at + 3 > src.length then err("unterminated escape")
            sb.append(src.substring(at, (at + 4).min(src.length)))
          } else done = true
        } else if src.startsWith("${", at) then done = true
        else sb.append(src(at))
      }
      if sb.nonEmpty then emit(TK_STR_CONTENT, sb.toString)
      if startsWith("''") then {
        emit(TK_IND_STR_CLOSE, "''")
        stack = stack.tail
      } else {
        emit(TK_INTERP_OPEN, "${")
        stack = Frame.Default(0) :: stack
      }

    while pos < src.length do
      stack.head match
        case Default(d) => lexDefault(d)
        case Str        => lexStr()
        case IndStr     => lexIndStr()

    stack match
      case Default(0) :: Nil => ()
      case Str :: _          => err("unterminated string")
      case IndStr :: _       => err("unterminated indented string")
      case _                 => err("unclosed '{' or '${'")

    out += Token(TK_EOF, "", pos)
    out.toVector
