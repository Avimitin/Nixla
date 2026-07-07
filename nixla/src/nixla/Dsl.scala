package nixla

import nixla.cst.*
import scala.quoted.*

/** The user-facing surface: `import nixla.dsl.*`
  *
  *   val pkgs  = ref("pkgs")
  *   val shell = pkgs.mkShell(packages = List(pkgs.mill, pkgs.jdk21).nix)
  *   val f     = fn { (self, nixpkgs) => (devShells = ...).nix }
  */
object dsl:

  def ref(name: String): Ref = Ref.of(name)

  def lit[A](a: A)(using ta: ToNix[A]): Nix[ta.Out] = ta.toNix(a)

  def path(p: String): Nix[NPath] = Nix(Build.path(p))

  /** simple-parameter lambda: `lam("x") { x => x.someAttr }` emits `x: …` */
  def lam[R](name: String)(f: Ref => R)(using tn: ToNix[R]): Nix[NAny] =
    Nix(Build.lambda(name, tn.toNix(f(Ref.of(name))).green))

  /** `value.nix` — lift any encodable Scala value into a typed Nix expression */
  extension [A](a: A)(using ta: ToNix[A]) def nix: Nix[ta.Out] = ta.toNix(a)

  /** `nix"echo ${jdk.version}"` — Scala interpolation becomes Nix interpolation */
  extension (sc: StringContext)
    def nix(args: NixArg*): Nix[NStr] =
      val parts: Seq[String | GreenNode] = sc.parts.zipWithIndex.flatMap { (lit, ix) =>
        val splice = if ix < args.length then Seq(argGreen(args(ix))) else Seq.empty
        (Seq(lit).filter(_.nonEmpty): Seq[String | GreenNode]) ++ splice
      }
      Nix(Build.strParts(parts*))

  // literals lift implicitly where a Nix[…] is expected
  given Conversion[String, Nix[NStr]]   = s => Nix(Build.str(s))
  given Conversion[Int, Nix[NInt]]      = i => Nix(Build.int(i))
  given Conversion[Boolean, Nix[NBool]] = b => Nix(Build.bool(b))

  /** destructuring lambda whose Nix pattern names are the SCALA parameter
    * names: `fn { (self, nixpkgs) => … }` emits `{ self, nixpkgs, ... }: …`.
    * Hygiene comes for free — the binders are Scala binders.
    */
  inline def fn[R](inline f: Ref => R)(using tn: ToNix[R]): Nix[NAny] =
    ${ FnMacros.impl1('f, 'tn) }
  inline def fn[R](inline f: (Ref, Ref) => R)(using tn: ToNix[R]): Nix[NAny] =
    ${ FnMacros.impl2('f, 'tn) }
  inline def fn[R](inline f: (Ref, Ref, Ref) => R)(using tn: ToNix[R]): Nix[NAny] =
    ${ FnMacros.impl3('f, 'tn) }
  inline def fn[R](inline f: (Ref, Ref, Ref, Ref) => R)(using tn: ToNix[R]): Nix[NAny] =
    ${ FnMacros.impl4('f, 'tn) }

  /** Nix quasiquote, parsed at Scala compile time by nixla's own parser.
    *
    *   nixq"""{ pkgs ? import <nixpkgs> { } }: ${shell}"""
    *
    * `${…}` is a Scala splice (any ToNix value); write `$${…}` for a literal
    * Nix interpolation. Syntax errors — and splices in impossible positions —
    * are Scala compile errors.
    */
  extension (inline sc: StringContext)
    inline def nixq(inline args: Any*): Nix[NAny] = ${ NixQMacro.impl('sc, 'args) }

private[nixla] object NixQMacro:
  import nixla.cst.ParseError

  def impl(sc: Expr[StringContext], args: Expr[Seq[Any]])(using Quotes): Expr[Nix[NAny]] =
    import quotes.reflect.*
    val parts: List[String] = sc match
      case '{ StringContext(${ Varargs(ps) }*) } => ps.toList.map(_.valueOrAbort)
      case _ => report.errorAndAbort("nixq must be applied to a literal string interpolation")

    // stage 1 of the meta compiler: OUR parser runs inside scalac
    try NixQ.validate(parts)
    catch
      case e: ParseError => report.errorAndAbort(s"nixq: ${e.render(NixQ.skeletonOf(parts))}")
      case e: GraftError => report.errorAndAbort(e.getMessage)

    val spliceExprs: List[Expr[cst.GreenNode]] = args match
      case Varargs(as) =>
        as.toList.map { a =>
          a.asTerm.tpe.widen.asType match
            case '[t] =>
              val at = a.asExprOf[t]
              Expr.summon[ToNix[t]] match
                case Some(tn) => '{ $tn.toNix($at).green }
                case None =>
                  report.errorAndAbort(
                    s"nixq splice: no ToNix[${Type.show[t]}] instance for this value", a)
        }
      case _ => report.errorAndAbort("nixq: unexpected splice arguments")

    '{ NixQ.graft(${ Expr(parts) }, ${ Expr.ofList(spliceExprs) }) }

private[nixla] object FnMacros:

  def pattern(names: List[String], body: GreenNode): Nix[NAny] =
    Nix(Build.patternLambda(names.map(_ -> None), ellipsis = true, bound = None, body))

  private def paramNames(f: Expr[?], arity: Int)(using Quotes): List[String] =
    import quotes.reflect.*
    def go(t: Term): List[String] = t match
      case Inlined(_, _, inner) => go(inner)
      case Block(Nil, inner)    => go(inner)
      case Lambda(params, _)    => params.map(_.name)
      case _ =>
        report.errorAndAbort(
          s"fn expects a literal lambda of $arity parameter(s), e.g. fn { (pkgs, lib) => ... }")
    go(f.asTerm)

  def impl1[R: Type](f: Expr[Ref => R], tn: Expr[ToNix[R]])(using Quotes): Expr[Nix[NAny]] =
    val ns = paramNames(f, 1)
    val n0 = Expr(ns(0))
    '{ pattern(List($n0), $tn.toNix($f(Ref.of($n0))).green) }

  def impl2[R: Type](f: Expr[(Ref, Ref) => R], tn: Expr[ToNix[R]])(using Quotes): Expr[Nix[NAny]] =
    val ns       = paramNames(f, 2)
    val (n0, n1) = (Expr(ns(0)), Expr(ns(1)))
    '{ pattern(List($n0, $n1), $tn.toNix($f(Ref.of($n0), Ref.of($n1))).green) }

  def impl3[R: Type](f: Expr[(Ref, Ref, Ref) => R], tn: Expr[ToNix[R]])(using Quotes): Expr[Nix[NAny]] =
    val ns           = paramNames(f, 3)
    val (n0, n1, n2) = (Expr(ns(0)), Expr(ns(1)), Expr(ns(2)))
    '{ pattern(List($n0, $n1, $n2), $tn.toNix($f(Ref.of($n0), Ref.of($n1), Ref.of($n2))).green) }

  def impl4[R: Type](f: Expr[(Ref, Ref, Ref, Ref) => R], tn: Expr[ToNix[R]])(using Quotes): Expr[Nix[NAny]] =
    val ns = paramNames(f, 4)
    val (n0, n1, n2, n3) = (Expr(ns(0)), Expr(ns(1)), Expr(ns(2)), Expr(ns(3)))
    '{
      pattern(
        List($n0, $n1, $n2, $n3),
        $tn.toNix($f(Ref.of($n0), Ref.of($n1), Ref.of($n2), Ref.of($n3))).green)
    }
