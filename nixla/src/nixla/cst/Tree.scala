package nixla.cst

/** Green tree: immutable, position-free, the single source of truth.
  *
  * The lossless invariant is structural: `text` concatenates every descendant
  * token's text in order, so parse(src).text == src cannot be violated by
  * a forgetful printer — only by a lexer that drops characters, which the
  * property tests guard.
  */
type GreenChild = GreenToken | GreenNode

final case class GreenToken(kind: SyntaxKind, text: String):
  def width: Int = text.length

final case class GreenNode(kind: SyntaxKind, children: Vector[GreenChild]):
  lazy val width: Int = children.iterator.map {
    case t: GreenToken => t.width
    case n: GreenNode  => n.width
  }.sum

  def text: String =
    val sb = new StringBuilder(width)
    def go(c: GreenChild): Unit = c match
      case t: GreenToken => sb.append(t.text)
      case n: GreenNode  => n.children.foreach(go)
    children.foreach(go)
    sb.toString

/** Red tree: a lazy overlay adding absolute offsets and parent links.
  * Purely derived from the green tree; carries no data of its own.
  */
final case class Span(start: Int, end: Int):
  def length: Int = end - start

final class SyntaxNode(val green: GreenNode, val parent: Option[SyntaxNode], val offset: Int):
  def kind: SyntaxKind = green.kind
  def span: Span       = Span(offset, offset + green.width)
  def text: String     = green.text

  def children: Vector[SyntaxNode | SyntaxToken] =
    var off = offset
    green.children.map {
      case t: GreenToken =>
        val r = SyntaxToken(t, this, off)
        off += t.width
        r
      case n: GreenNode =>
        val r = new SyntaxNode(n, Some(this), off)
        off += n.width
        r
    }

  def childNodes: Vector[SyntaxNode] =
    children.collect { case n: SyntaxNode => n }

  def tokens: Vector[SyntaxToken] =
    children.collect { case t: SyntaxToken => t }

  /** innermost node whose span contains `pos` */
  def find(pos: Int): SyntaxNode =
    childNodes.find(n => n.span.start <= pos && pos < n.span.end) match
      case Some(n) => n.find(pos)
      case None    => this

  override def toString = s"SyntaxNode($kind, $span)"

final case class SyntaxToken(green: GreenToken, parent: SyntaxNode, offset: Int):
  def kind: SyntaxKind = green.kind
  def span: Span       = Span(offset, offset + green.width)
  def text: String     = green.text

/** Parse failure: fail-fast with a byte offset; render() maps it to
  * line/column against the original source.
  */
final case class ParseError(message: String, offset: Int) extends RuntimeException(message):
  def render(src: String): String =
    val before = src.take(offset)
    val line   = before.count(_ == '\n') + 1
    val col    = offset - (before.lastIndexOf('\n') + 1) + 1
    val lineText = src.linesWithSeparators.drop(line - 1).nextOption().getOrElse("").stripLineEnd
    s"parse error at $line:$col: $message\n  $lineText\n  ${" " * (col - 1)}^"
