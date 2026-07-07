package scalanix

import scala.language.implicitConversions
import dsl.{*, given}
import NixExpr.*, Binding.*, Param.*

@main def demo(): Unit =

  // ============================================================
  // 1. Route A — write Nix *in Scala*: build a flake with the DSL
  // ============================================================
  val pkgs = id("pkgs")
  val devShell = attrs(
    "description" -> "A Scala dev shell, generated from Scala 3",
    "inputs" -> attrs(
      "nixpkgs.url" -> "github:NixOS/nixpkgs/nixos-25.05"
    ),
    "outputs" -> NLambda(
      Destructure(List("self" -> None, "nixpkgs" -> None), false, None),
      let(
        "system" -> "x86_64-linux",
        "pkgs"   -> NSelect(id("nixpkgs"), List("legacyPackages", "x86_64-linux"), None),
        "jdk"    -> (pkgs / "jdk21")
      )(
        attrs(
          "devShells.x86_64-linux.default" -> (pkgs / "mkShell")(attrs(
            "packages" -> list(pkgs / "scala-cli", pkgs / "sbt", id("jdk")),
            "JAVA_HOME" -> nix"${id("jdk")}/lib/openjdk",
            "shellHook" -> nix"echo Welcome, JDK is ${id("jdk") / "version"}"
          ))
        )
      )
    )
  )

  println("═══ 1. DSL-built flake.nix ═══")
  print(devShell.emitted)

  // ============================================================
  // 2. Route B — parse real Nix, transform the AST, re-emit
  // ============================================================
  val source = """
    # a tiny derivation
    { stdenv, fetchurl, lib, enableDocs ? false }:
    stdenv.mkDerivation rec {
      pname = "hello";
      version = "2.12.1";
      src = fetchurl {
        url = "mirror://gnu/hello/hello-${version}.tar.gz";
        sha256 = "086vqwk2wl8zfs47sq2xpjc9k066ilmb8z6dn0q6ymwjzlm196cd";
      };
      doCheck = true;
      meta = with lib; {
        description = "A program that produces a familiar, friendly greeting";
        license = licenses.gpl3Plus;
        platforms = platforms.all;
      };
    }
  """

  val ast = Parser(source).parse()

  // AST transformation: bump the version and flip doCheck — plain pattern matching
  def rewrite(e: NixExpr): NixExpr = e match
    case NAttrSet(r, bs) =>
      NAttrSet(r, bs.map {
        case Bind(List("version"), _)     => Bind(List("version"), NStr(List(StrPart.Lit("2.13.0"))))
        case Bind(List("doCheck"), _)     => Bind(List("doCheck"), NBool(false))
        case Bind(p, v)                   => Bind(p, rewrite(v))
        case other                        => other
      })
    case NLambda(p, b)  => NLambda(p, rewrite(b))
    case NApply(f, a)   => NApply(rewrite(f), rewrite(a))
    case NWith(e1, e2)  => NWith(rewrite(e1), rewrite(e2))
    case other          => other

  println()
  println("═══ 2. parsed → transformed (version bump, doCheck off) → re-emitted ═══")
  print(Emit(rewrite(ast)))

  // ============================================================
  // 3. Round-trip sanity: parse(emit(ast)) == ast
  // ============================================================
  val emitted   = Emit(rewrite(ast))
  val reparsed  = Parser(emitted).parse()
  val stable    = Emit(reparsed) == emitted
  println()
  println(s"═══ 3. round-trip fixpoint: parse(emit(x)) emits identically → $stable ═══")
