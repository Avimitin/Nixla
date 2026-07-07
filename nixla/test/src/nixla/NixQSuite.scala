package nixla

import scala.language.implicitConversions
import nixla.dsl.{*, given}

class NixQSuite extends munit.FunSuite:

  test("expression splice: ToNix values graft in") {
    val q = nixq"{ x = ${1}; y = ${"two"}; }"
    assertEquals(q.text, """{ x = 1; y = "two"; }""")
  }

  test("skeleton stays verbatim: comments and layout survive") {
    val jdk = ref("pkgs").jdk21
    val q = nixq"""{
  # toolchain
  jdk = ${jdk};
}"""
    assertEquals(q.text,
      """|{
         |  # toolchain
         |  jdk = pkgs.jdk21;
         |}""".stripMargin)
  }

  test("splice inside a Nix string becomes interpolation") {
    val v = ref("jdk").version
    assertEquals(nixq""""echo ${v}"""".text, "\"echo ${jdk.version}\"")
  }

  test("$$ escapes to a literal Nix interpolation") {
    val q = nixq""""at $${builtins.currentTime}""""
    assertEquals(q.text, "\"at ${builtins.currentTime}\"")
  }

  test("splice as an attribute name becomes a dynamic attr") {
    val q = nixq"{ ${"dyn-key"} = 1; }"
    assertEquals(q.text, """{ ${"dyn-key"} = 1; }""")
  }

  test("splice inside a path") {
    val name = ref("pname")
    assertEquals(nixq"./src/${name}".text, "./src/${pname}")
  }

  test("splices compose: nixq inside nixq, DSL inside nixq") {
    val inner = nixq"{ enable = ${true}; }"
    val outer = nixq"{ services.foo = ${inner}; }"
    // the splice is Pretty-rendered (canonical, multi-line attrset) while the
    // skeleton stays verbatim — exactly the DESIGN.md graft contract
    assertEquals(outer.text, "{ services.foo = {\n  enable = true;\n}; }")

    val shell = ref("pkgs").mkShell(packages = List(1, 2).nix)
    val flake = nixq"""{ pkgs ? import <nixpkgs> { } }: ${shell.nix}"""
    assert(flake.text.contains("pkgs.mkShell"), flake.text)
    // and the result still parses + prettys as a fixpoint
    val p = nixla.cst.Pretty(nixla.cst.Parser.parseGreen(flake.text))
    assertEquals(nixla.cst.Pretty(nixla.cst.Parser.parseGreen(p)), p)
  }

  test("operator splice gets protective parens") {
    val sum = 1.nix + 2.nix
    assertEquals(nixq"10 - ${sum}".text, "10 - (1 + 2)")
  }

  test("compile error: bad Nix syntax in the skeleton") {
    val errors = compileErrors("""import nixla.dsl.*; nixq"1 +"""")
    assert(errors.contains("nixq"), errors)
    assert(errors.contains("expected expression"), errors)
  }

  test("compile error: splice in a comment is rejected") {
    val errors = compileErrors("""import nixla.dsl.*; nixq"1 # see ${2}"""")
    assert(errors.contains("comment"), errors)
  }

  test("compile error: splice as lambda parameter is rejected") {
    val errors = compileErrors("""import nixla.dsl.*; nixq"${1}: 2"""")
    assert(errors.nonEmpty, errors)
  }
