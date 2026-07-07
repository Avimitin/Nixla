package nixla

import nixla.cst.*
import scala.compiletime.{constValueTuple, erasedValue, summonInline}
import scala.deriving.Mirror
import scala.NamedTuple.NamedTuple

/** Encoding of Scala values as Nix expressions (layer 4 in DESIGN.md).
  *
  * Case classes and named tuples derive to attrsets via Mirror; maps are
  * emitted with sorted keys so output stays deterministic.
  */
trait ToNix[A]:
  type Out <: NAny
  def toNix(a: A): Nix[Out]

object ToNix:
  type Aux[A, O <: NAny] = ToNix[A] { type Out = O }

  def instance[A, O <: NAny](f: A => GreenNode): Aux[A, O] = new ToNix[A]:
    type Out = O
    def toNix(a: A): Nix[O] = Nix[O](f(a))

  given str: Aux[String, NStr]      = instance(Build.str)
  given int: Aux[Int, NInt]         = instance(i => Build.int(i))
  given long: Aux[Long, NInt]       = instance(Build.int)
  given bool: Aux[Boolean, NBool]   = instance(Build.bool)
  given double: Aux[Double, NFloat] = instance(Build.float)

  given nixIdentity[T <: NAny]: Aux[Nix[T], T] = new ToNix[Nix[T]]:
    type Out = T
    def toNix(a: Nix[T]): Nix[T] = a

  given ref: Aux[Ref, NAny] = instance(_.green)

  given list[A](using ta: ToNix[A]): Aux[List[A], NList[ta.Out]] =
    instance(as => Build.list(as.map(a => ta.toNix(a).green)*))

  given seq[A](using ta: ToNix[A]): Aux[Seq[A], NList[ta.Out]] =
    instance(as => Build.list(as.map(a => ta.toNix(a).green)*))

  given option[A](using ta: ToNix[A]): Aux[Option[A], NAny] =
    instance {
      case Some(a) => ta.toNix(a).green
      case None    => Build.nullE
    }

  /** sorted keys: deterministic emit is a contract, not a suggestion */
  given map[A](using ta: ToNix[A]): Aux[Map[String, A], NAttrs] =
    instance { m =>
      val bindings = m.keys.toSeq.sorted.map(k => Build.binding(Seq(k), ta.toNix(m(k)).green))
      Build.attrs(bindings*)
    }

  private inline def summonAll[T <: Tuple]: List[ToNix[?]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   => summonInline[ToNix[h]] :: summonAll[t]

  private def product(labels: List[String], instances: List[ToNix[?]], values: List[Any]): GreenNode =
    val bindings = labels.lazyZip(instances).lazyZip(values).map { (label, tn, value) =>
      Build.binding(Seq(label), tn.asInstanceOf[ToNix[Any]].toNix(value).green)
    }
    Build.attrs(bindings*)

  /** `case class Shell(packages: List[Ref]) derives ToNix` */
  inline given derived[A <: Product](using m: Mirror.ProductOf[A]): Aux[A, NAttrs] =
    val labels    = constValueTuple[m.MirroredElemLabels].toList.map(_.toString)
    val instances = summonAll[m.MirroredElemTypes]
    instance(a => product(labels, instances, a.productIterator.toList))

  /** named tuples are attrsets: (pname = "hello", version = "2.13").nix */
  inline given namedTuple[N <: Tuple, V <: Tuple]: Aux[NamedTuple[N, V], NAttrs] =
    val labels    = constValueTuple[N].toList.map(_.toString)
    val instances = summonAll[V]
    instance(a => product(labels, instances, a.toTuple.productIterator.toList))

