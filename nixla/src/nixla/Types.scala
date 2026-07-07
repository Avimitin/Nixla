package nixla

import nixla.cst.*
import scala.language.dynamics

/** Phantom kinds for Nix values (layer 3 in DESIGN.md). These types have no
  * runtime inhabitants — they only classify `Nix[T]` expressions so that
  * ill-kinded Nix is a Scala type error.
  */
sealed trait NAny
sealed trait NStr   extends NAny
sealed trait NInt   extends NAny
sealed trait NFloat extends NAny
sealed trait NBool  extends NAny
sealed trait NNull  extends NAny
sealed trait NPath  extends NAny
sealed trait NAttrs extends NAny
sealed trait NList[+E <: NAny] extends NAny
sealed trait NFn[-A <: NAny, +B <: NAny] extends NAny

/** A typed Nix expression: a phantom-typed wrapper around a green tree. */
final case class Nix[+T <: NAny](green: GreenNode):
  /** deterministic canonical source (Pretty) */
  def render: String = Pretty(green)
  /** verbatim source — meaningful for parsed/quasiquoted trees, where the
    * skeleton's original bytes (comments, layout) are preserved
    */
  def text: String = green.text

object Nix:
  def raw(green: GreenNode): Nix[NAny] = Nix[NAny](green)

/** Argument magnet for fluent call sites: anything that is already Nix, a
  * Ref, or a scalar literal.
  */
type NixArg = Nix[?] | Ref | String | Int | Long | Boolean | Double

private[nixla] def argGreen(a: NixArg): GreenNode = a match
  case n: Nix[?]  => n.green
  case r: Ref     => r.green
  case s: String  => Build.str(s)
  case i: Int     => Build.int(i)
  case l: Long    => Build.int(l)
  case b: Boolean => Build.bool(b)
  case d: Double  => Build.float(d)

/** An untyped Nix reference with fluent selection and application:
  *
  *   pkgs.legacyPackages.`x86_64-linux`.jdk21   // selection chain
  *   pkgs.mkShell(shell)                        // application
  *   pkgs.mkShell(packages = ..., shellHook = ...) // named args -> attrset
  *
  * Deliberately dynamic: Nix itself is dynamically scoped/typed at this
  * level, and the phantom layer regains safety where shapes are known.
  */
final case class Ref(green: GreenNode) extends Dynamic:
  def selectDynamic(name: String): Ref =
    Ref(Build.select(green, name))

  def applyDynamic(name: String)(args: NixArg*): Ref =
    Ref(Build.app(Build.select(green, name), args.map(argGreen)*))

  /** named arguments become a single attrset argument */
  def applyDynamicNamed(name: String)(args: (String, NixArg)*): Ref =
    val bindings = args.map((k, v) => Build.binding(Seq(k), argGreen(v)))
    Ref(Build.app(Build.select(green, name), Build.attrs(bindings*)))

  /** direct application: ref(arg1)(arg2)… */
  def apply(args: NixArg*): Ref =
    Ref(Build.app(green, args.map(argGreen)*))

  def nix: Nix[NAny] = Nix[NAny](green)
  def render: String = Pretty(green)

object Ref:
  def of(name: String): Ref = Ref(Build.ident(name))

// ---- typed operators -------------------------------------------------------
extension (e: Nix[NStr])
  @scala.annotation.targetName("strConcat")
  def +(o: Nix[NStr]): Nix[NStr] = Nix(Build.binop("+", e.green, o.green))

extension (e: Nix[NInt])
  @scala.annotation.targetName("intAdd")
  def +(o: Nix[NInt]): Nix[NInt] = Nix(Build.binop("+", e.green, o.green))
  @scala.annotation.targetName("intSub")
  def -(o: Nix[NInt]): Nix[NInt] = Nix(Build.binop("-", e.green, o.green))
  @scala.annotation.targetName("intMul")
  def *(o: Nix[NInt]): Nix[NInt] = Nix(Build.binop("*", e.green, o.green))

extension (e: Nix[NAttrs])
  /** the Nix `//` update operator */
  def merge(o: Nix[NAttrs]): Nix[NAttrs] = Nix(Build.binop("//", e.green, o.green))
  def sel(path: String*): Ref            = Ref(Build.select(e.green, path*))

extension [E <: NAny](e: Nix[NList[E]])
  @scala.annotation.targetName("listConcat")
  def ++(o: Nix[NList[E]]): Nix[NList[E]] = Nix(Build.binop("++", e.green, o.green))
