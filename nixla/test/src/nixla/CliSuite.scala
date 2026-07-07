package nixla

import scala.language.implicitConversions
import nixla.dsl.{*, given}

/** M6 testkit: rendered output is verified by the REAL Nix CLI.
  * Skipped gracefully where `nix-instantiate` is absent; the dev-shell
  * flake guarantees it during development.
  *
  * TODO (decision #4): differential fuzzing — generate random trees,
  * Pretty-render, and compare our parser's acceptance against
  * `nix-instantiate --parse` over nixpkgs corpora.
  */
class CliSuite extends munit.FunSuite:

  override def munitIgnore: Boolean = !NixCli.available

  test("typed flake passes the real Nix parser") {
    val flake = (
      description = "nixla verified flake",
      outputs = fn { (self, nixpkgs) =>
        val pkgs = nixpkgs.legacyPackages.`x86_64-linux`
        (devShells = (default = pkgs.mkShell(packages = List(1).nix).nix).nix).nix
      }
    ).nix
    assertEquals(flake.check(), Right(()))
  }

  test("quasiquoted expression passes the real Nix parser") {
    val shell = ref("pkgs").mkShell(packages = List(1, 2).nix)
    val q = nixq"""{ pkgs ? import <nixpkgs> { } }: ${shell.nix}"""
    assertEquals(NixCli.parseCheck(q.text), Right(()))
    assertEquals(NixCli.parseCheck(q.render), Right(()))
  }

  test("nix evaluates what we build: semantics, not just syntax") {
    assertEquals(NixCli.eval((1.nix + 2.nix * 3.nix).render), Right("7"))
    assertEquals(NixCli.eval(nix"a${"b"}c".render), Right("\"abc\""))
    assertEquals(
      NixCli.eval(Map("x" -> 1).nix.merge(Map("y" -> 2).nix).render),
      Right("{ x = 1; y = 2; }"))
  }

  test("escaping survives the full pipeline") {
    val nasty = "quote\" backslash\\ dollar${ newline\n tab\t"
    NixCli.eval(nasty.nix.render) match
      case Right(printed) => assert(printed.contains("dollar"), printed)
      case Left(e)        => fail(e)
  }

  test("the gate actually rejects garbage") {
    assert(NixCli.parseCheck("1 +").isLeft)
  }
