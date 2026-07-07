package scalanix

/** The core Nix expression AST. This is the load-bearing type of the whole
  * pipeline: the parser produces it, the DSL builds it, the emitter consumes it.
  */
enum NixExpr:
  case NInt(value: Long)
  case NFloat(value: Double)
  case NBool(value: Boolean)
  case NNull
  case NStr(parts: List[StrPart])       // "foo ${bar}"
  case NIndentStr(parts: List[StrPart]) // ''...''
  case NPath(value: String)             // ./foo, /etc/x, <nixpkgs>
  case NIdent(name: String)
  case NList(items: List[NixExpr])
  case NAttrSet(rec: Boolean, bindings: List[Binding])
  case NLambda(param: Param, body: NixExpr)
  case NApply(fn: NixExpr, arg: NixExpr)
  case NLet(bindings: List[Binding], body: NixExpr)
  case NIf(cond: NixExpr, thenB: NixExpr, elseB: NixExpr)
  case NWith(env: NixExpr, body: NixExpr)
  case NAssert(cond: NixExpr, body: NixExpr)
  case NSelect(expr: NixExpr, path: List[String], orDefault: Option[NixExpr])
  case NHasAttr(expr: NixExpr, path: List[String])
  case NUnary(op: String, e: NixExpr)          // "-", "!"
  case NBinOp(op: String, lhs: NixExpr, rhs: NixExpr)

enum StrPart:
  case Lit(s: String)
  case Interp(e: NixExpr)

enum Binding:
  case Bind(path: List[String], value: NixExpr)          // a.b.c = v;
  case Inherit(from: Option[NixExpr], names: List[String]) // inherit (e) a b;

enum Param:
  case Simple(name: String)                                          // x: ...
  case Destructure(                                                  // { a, b ? d, ... } @ args:
      fields: List[(String, Option[NixExpr])],
      ellipsis: Boolean,
      bound: Option[String]
  )
