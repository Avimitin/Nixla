package scalanix

import NixExpr.*, StrPart.*, Binding.*, Param.*

/** Sugar so Scala reads like Nix. This is "Route A": Scala as the frontend,
  * Nix as the compile target.
  */
object dsl:

  // -- literals lift automatically ------------------------------------------
  given Conversion[String, NixExpr]  = s => NStr(List(Lit(s)))
  given Conversion[Int, NixExpr]     = i => NInt(i)
  given Conversion[Long, NixExpr]    = NInt(_)
  given Conversion[Boolean, NixExpr] = NBool(_)
  given Conversion[Double, NixExpr]  = NFloat(_)

  val nixNull: NixExpr = NNull

  // -- constructors ----------------------------------------------------------
  def id(name: String): NixExpr    = NIdent(name)
  def path(p: String): NixExpr     = NPath(p)
  def list(xs: NixExpr*): NixExpr  = NList(xs.toList)

  def attrs(bs: (String, NixExpr)*): NixExpr    = NAttrSet(false, bs.toList.map(bind))
  def recAttrs(bs: (String, NixExpr)*): NixExpr = NAttrSet(true, bs.toList.map(bind))
  private def bind(kv: (String, NixExpr)): Binding = Bind(splitKey(kv._1), kv._2)
  // "a.b.c" as a key becomes the attrpath a.b.c ; quote a segment to keep dots
  private def splitKey(k: String): List[String] =
    if k.startsWith("\"") then List(k.drop(1).dropRight(1)) else k.split('.').toList

  def let(bs: (String, NixExpr)*)(body: NixExpr): NixExpr =
    NLet(bs.toList.map(bind), body)

  def lam(p: String)(body: NixExpr => NixExpr): NixExpr =
    NLambda(Simple(p), body(NIdent(p)))

  /** { pkgs, lib, ... }: style destructuring lambda */
  def lamSet(fields: String*)(body: NixExpr): NixExpr =
    NLambda(Destructure(fields.toList.map(_ -> None), ellipsis = true, bound = None), body)

  def iff(c: NixExpr)(t: NixExpr)(f: NixExpr): NixExpr = NIf(c, t, f)
  def withE(env: NixExpr)(body: NixExpr): NixExpr      = NWith(env, body)
  def inherit(names: String*): Binding                 = Inherit(None, names.toList)
  def inheritFrom(e: NixExpr)(names: String*): Binding = Inherit(Some(e), names.toList)

  /** nix"…${expr}…" string interpolator: Scala interpolation becomes Nix interpolation */
  extension (sc: StringContext)
    def nix(args: NixExpr*): NixExpr =
      val parts = sc.parts.toList
      val mixed = parts.zipAll(args.map(Interp(_)), "", null).flatMap { (lit, in) =>
        List(Lit(lit)) ++ Option(in)
      }
      NStr(mixed.filter { case Lit("") => false; case _ => true })

  extension (e: NixExpr)
    /** function application: f(x) emits `f x` */
    def apply(arg: NixExpr): NixExpr = NApply(e, arg)
    /** attribute selection: e / "a.b" emits `e.a.b` */
    def /(attr: String): NixExpr = e match
      case NSelect(x, p, None) => NSelect(x, p ++ attr.split('.').toList, None)
      case _                   => NSelect(e, attr.split('.').toList, None)
    def orElse(default: NixExpr): NixExpr = e match
      case NSelect(x, p, None) => NSelect(x, p, Some(default))
      case _                   => e
    def ++(rhs: NixExpr): NixExpr = NBinOp("++", e, rhs)
    def +(rhs: NixExpr): NixExpr  = NBinOp("+", e, rhs)
    def merge(rhs: NixExpr): NixExpr = NBinOp("//", e, rhs) // the Nix `//` operator
    def ===(rhs: NixExpr): NixExpr   = NBinOp("==", e, rhs)
    def emitted: String = Emit(e)
