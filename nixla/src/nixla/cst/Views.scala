package nixla.cst

/** Typed views: zero-cost wrappers over red nodes (layer 2 in DESIGN.md).
  * They duplicate no data — every accessor reads the underlying tree.
  */
object views:
  import SyntaxKind.*

  final case class AttrSet(node: SyntaxNode):
    def isRec: Boolean = node.tokens.exists(_.kind == TK_KW_REC)
    def bindings: Vector[Binding] =
      node.childNodes.filter(_.kind == N_BINDING).map(Binding.apply)
    def inherits: Vector[SyntaxNode] = node.childNodes.filter(_.kind == N_INHERIT)
    /** binding value for a static single-segment attr name, if present */
    def get(name: String): Option[SyntaxNode] =
      bindings.find(_.pathNames == Vector(name)).map(_.value)

  final case class Binding(node: SyntaxNode):
    def attrpath: SyntaxNode = node.childNodes.head
    def value: SyntaxNode    = node.childNodes.last
    /** static path segments; dynamic/interpolated segments appear as their text */
    def pathNames: Vector[String] =
      attrpath.children.collect {
        case t: SyntaxToken if t.kind == TK_IDENT || t.kind == TK_KW_OR => t.text
        case n: SyntaxNode if n.kind == N_STRING || n.kind == N_DYNAMIC => n.text
      }

  final case class Lambda(node: SyntaxNode):
    def param: SyntaxNode = node.childNodes.head
    def body: SyntaxNode  = node.childNodes.last
    def isPattern: Boolean = param.kind == N_PATTERN
    def patternFields: Vector[String] =
      if !isPattern then Vector.empty
      else param.childNodes.filter(_.kind == N_PAT_ENTRY).flatMap(
        _.tokens.collectFirst { case t if t.kind == TK_IDENT => t.text })

  final case class Apply(node: SyntaxNode):
    def fn: SyntaxNode  = node.childNodes.head
    def arg: SyntaxNode = node.childNodes.last

  final case class Select(node: SyntaxNode):
    def base: SyntaxNode           = node.childNodes.head
    def attrpath: SyntaxNode       = node.childNodes(1)
    def default: Option[SyntaxNode] = node.childNodes.lift(2)

  final case class Let(node: SyntaxNode):
    def bindings: Vector[Binding] =
      node.childNodes.filter(_.kind == N_BINDING).map(Binding.apply)
    def body: SyntaxNode = node.childNodes.last

  extension (n: SyntaxNode)
    def asAttrSet: Option[AttrSet] = Option.when(n.kind == N_ATTRSET)(AttrSet(n))
    def asBinding: Option[Binding] = Option.when(n.kind == N_BINDING)(Binding(n))
    def asLambda: Option[Lambda]   = Option.when(n.kind == N_LAMBDA)(Lambda(n))
    def asApply: Option[Apply]     = Option.when(n.kind == N_APPLY)(Apply(n))
    def asSelect: Option[Select]   = Option.when(n.kind == N_SELECT)(Select(n))
    def asLet: Option[Let]         = Option.when(n.kind == N_LET)(Let(n))

/** Structural rewriting over green trees: bottom-up, pure, minimal —
  * untouched subtrees are shared, so `text` diffs stay minimal.
  */
object Rewrite:
  def apply(n: GreenNode)(f: PartialFunction[GreenNode, GreenNode]): GreenNode =
    val rebuilt = GreenNode(n.kind, n.children.map {
      case c: GreenNode  => apply(c)(f)
      case t: GreenToken => t
    })
    f.applyOrElse(rebuilt, identity[GreenNode])
