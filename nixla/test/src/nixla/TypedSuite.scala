package nixla

import scala.language.implicitConversions
import nixla.dsl.{*, given}

class TypedSuite extends munit.FunSuite:

  def rendered(n: Nix[?] | Ref)(using munit.Location): String = n match
    case n: Nix[?] => n.render.trim
    case r: Ref    => r.render.trim

  test("ToNix: scalars, lists, maps (sorted keys)") {
    assertEquals("hi".nix.render.trim, "\"hi\"")
    assertEquals(42.nix.render.trim, "42")
    assertEquals(true.nix.render.trim, "true")
    assertEquals(List(1, 2, 3).nix.render.trim, "[ 1 2 3 ]")
    assertEquals(Option.empty[Int].nix.render.trim, "null")
    assertEquals(
      Map("zeta" -> 1, "alpha" -> 2).nix.render.trim,
      """|{
         |  alpha = 2;
         |  zeta = 1;
         |}""".stripMargin
    )
  }

  test("ToNix: case class derivation") {
    case class Meta(description: String, homepage: String) derives ToNix
    assertEquals(
      Meta("a tool", "https://example.org").nix.render.trim,
      """|{
         |  description = "a tool";
         |  homepage = "https://example.org";
         |}""".stripMargin
    )
  }

  test("ToNix: nested case classes") {
    case class Inner(x: Int) derives ToNix
    case class Outer(name: String, inner: Inner) derives ToNix
    assertEquals(
      Outer("o", Inner(7)).nix.render.trim,
      """|{
         |  name = "o";
         |  inner = {
         |    x = 7;
         |  };
         |}""".stripMargin
    )
  }

  test("ToNix: named tuples are attrsets") {
    val shell = (pname = "hello", version = "2.13.0", doCheck = false)
    assertEquals(
      shell.nix.render.trim,
      """|{
         |  pname = "hello";
         |  version = "2.13.0";
         |  doCheck = false;
         |}""".stripMargin
    )
  }

  test("Ref: dynamic selection and application") {
    val pkgs = ref("pkgs")
    assertEquals(rendered(pkgs.legacyPackages.`x86_64-linux`.jdk21),
      "pkgs.legacyPackages.x86_64-linux.jdk21")
    assertEquals(rendered(pkgs.mkShell(ref("shell"))), "pkgs.mkShell shell")
    assertEquals(rendered(pkgs.lib.mkIf(true, ref("cfg"))), "pkgs.lib.mkIf true cfg")
  }

  test("Ref: named arguments become an attrset argument") {
    val pkgs = ref("pkgs")
    assertEquals(
      rendered(pkgs.mkShell(packages = List(1).nix, shellHook = "echo hi")),
      """|pkgs.mkShell {
         |  packages = [ 1 ];
         |  shellHook = "echo hi";
         |}""".stripMargin
    )
  }

  test("nix interpolator: Scala splices become Nix interpolation") {
    val jdk = ref("jdk")
    assertEquals(nix"echo JDK is ${jdk.version}".render.trim,
      "\"echo JDK is ${jdk.version}\"")
    assertEquals(nix"a $$literal".render.trim, "\"a \\$literal\"") // $$ escapes
  }

  test("fn macro: Nix pattern names are the Scala parameter names") {
    val outputs = fn { (self, nixpkgs) =>
      nixpkgs.legacyPackages.`x86_64-linux`.mkShell(name = "dev")
    }
    assertEquals(
      outputs.render.trim,
      """|{ self, nixpkgs, ... }: nixpkgs.legacyPackages.x86_64-linux.mkShell {
         |  name = "dev";
         |}""".stripMargin
    )
  }

  test("fn macro: single parameter, ToNix result") {
    case class Cfg(enable: Boolean) derives ToNix
    val f = fn { pkgs => Cfg(enable = true) }
    assertEquals(
      f.render.trim,
      """|{ pkgs, ... }:
         |  {
         |    enable = true;
         |  }""".stripMargin
    )
  }

  test("typed operators: well-kinded combinations only") {
    val a: Nix[NInt] = 1
    val b: Nix[NInt] = 2
    assertEquals((a + b * b).render.trim, "1 + 2 * 2")
    val s: Nix[NStr] = "x"
    assertEquals((s + s).render.trim, "\"x\" + \"x\"")
    val l = List(1).nix ++ List(2).nix
    assertEquals(l.render.trim, "[ 1 ] ++ [ 2 ]")
    val m = Map("a" -> 1).nix.merge(Map("b" -> 2).nix)
    // (does not compile, and that is the point):
    //   a + s ; l ++ m ; s * b
    assert(m.render.contains("//"))
  }

  test("lam: simple-parameter lambda") {
    assertEquals(lam("x") { x => x.out }.render.trim, "x: x.out")
  }

  test("end to end: a flake built from typed pieces parses back losslessly") {
    val flake = (
      description = "typed flake",
      outputs = fn { (self, nixpkgs) =>
        val pkgs = nixpkgs.legacyPackages.`x86_64-linux`
        (devShells = (default = pkgs.mkShell(packages = List(1).nix).nix).nix).nix
      }
    ).nix
    val text = flake.render
    // canonical output parses, and pretty is a fixpoint on it
    assertEquals(nixla.cst.Pretty(nixla.cst.Parser.parseGreen(text)), text)
  }
