package scalanix

import scala.language.implicitConversions
import dsl.{*, given}
import NixExpr.*

class RoundTripSuite extends munit.FunSuite:

  /** emit → parse → emit must be a fixpoint */
  def fix(src: String)(using munit.Location): Unit =
    val once  = Emit(Parser(src).parse())
    val twice = Emit(Parser(once).parse())
    assertEquals(twice, once)

  test("literals survive the round trip") {
    for src <- List("42", "3.14", "true", "null", "\"hello\"", "./foo/bar", "[ 1 2 3 ]")
    do fix(src)
  }

  test("operator precedence needs no spurious parens") {
    assertEquals(Emit(Parser("1 + 2 * 3").parse()).trim, "1 + 2 * 3")
    assertEquals(Emit(Parser("(1 + 2) * 3").parse()).trim, "(1 + 2) * 3")
    // ++ is right-assoc: re-emitting must keep grouping
    assertEquals(Emit(Parser("(a ++ b) ++ c").parse()).trim, "(a ++ b) ++ c")
    assertEquals(Emit(Parser("a ++ b ++ c").parse()).trim, "a ++ b ++ c")
  }

  test("lambdas, let, with, inherit") {
    fix("{ a, b ? false, ... } @ args: a b")
    fix("let x = 1; y = x; in x + y")
    fix("with pkgs; [ hello cowsay ]")
    fix("{ inherit (pkgs) stdenv lib; }")
  }

  test("string interpolation round-trips") {
    fix("\"pre ${toString x} post\"")
  }

  test("select with or-default") {
    fix("attrs.a.b or defaultValue")
  }

  test("DSL emits expected Nix") {
    val e = let("pkgs" -> (id("nixpkgs") / "legacyPackages.x86_64-linux"))(
      (id("pkgs") / "mkShell")(attrs("packages" -> list(id("pkgs") / "hello")))
    )
    assertEquals(
      Emit(e),
      """|let
         |  pkgs = nixpkgs.legacyPackages.x86_64-linux;
         |in
         |  pkgs.mkShell {
         |    packages = [ pkgs.hello ];
         |  }
         |""".stripMargin
    )
  }

  test("interpolator: scala ${} becomes nix ${}") {
    val jdk = id("jdk")
    assertEquals(Emit(nix"${jdk}/bin/java").trim, "\"${jdk}/bin/java\"")
  }
