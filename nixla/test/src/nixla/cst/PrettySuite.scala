package nixla.cst

import Build as B
import views.*

class PrettySuite extends munit.FunSuite:

  /** pretty is a fixpoint over parse: pretty(parse(pretty(parse(s)))) stable */
  def stable(src: String)(using munit.Location): Unit =
    val once = Pretty(Parser.parseGreen(src))
    assertEquals(Pretty(Parser.parseGreen(once)), once)

  test("golden: builders produce canonical Nix") {
    val shell = B.attrs(
      B.binding(Seq("packages"), B.list(B.select(B.ident("pkgs"), "mill"), B.ident("jdk"))),
      B.binding(Seq("shellHook"), B.strParts("echo ", B.select(B.ident("jdk"), "version")))
    )
    assertEquals(
      Pretty(shell),
      """|{
         |  packages = [ pkgs.mill jdk ];
         |  shellHook = "echo ${jdk.version}";
         |}
         |""".stripMargin
    )
  }

  test("golden: let / lambda / apply layout") {
    val e = B.patternLambda(
      Seq("pkgs" -> None), ellipsis = true, bound = None,
      B.letIn(
        Seq(B.binding(Seq("jdk"), B.select(B.ident("pkgs"), "jdk21"))),
        B.app(B.select(B.ident("pkgs"), "mkShell"), B.attrs(B.binding(Seq("x"), B.int(1))))
      )
    )
    assertEquals(
      Pretty(e),
      """|{ pkgs, ... }:
         |  let
         |    jdk = pkgs.jdk21;
         |  in
         |    pkgs.mkShell {
         |      x = 1;
         |    }
         |""".stripMargin
    )
  }

  test("builders parenthesize by precedence, never redundantly") {
    assertEquals(Pretty(B.binop("*", B.binop("+", B.int(1), B.int(2)), B.int(3))).trim,
      "(1 + 2) * 3")
    assertEquals(Pretty(B.binop("+", B.int(1), B.binop("*", B.int(2), B.int(3)))).trim,
      "1 + 2 * 3")
    // ++ is right-associative
    assertEquals(Pretty(B.binop("++", B.binop("++", B.ident("a"), B.ident("b")), B.ident("c"))).trim,
      "(a ++ b) ++ c")
    assertEquals(Pretty(B.binop("++", B.ident("a"), B.binop("++", B.ident("b"), B.ident("c")))).trim,
      "a ++ b ++ c")
    // application of a lambda needs parens
    assertEquals(Pretty(B.app(B.lambda("x", B.ident("x")), B.int(1))).trim, "(x: x) 1")
  }

  test("pretty is a fixpoint over parse") {
    List(
      "1 + 2 * 3",
      "{ a = 1; b = \"x\"; }",
      "let x = 1; in x",
      "{ pkgs, ... }: pkgs.mkShell { packages = [ pkgs.hello ]; }",
      "if a then b else c",
      "with pkgs; [ hello ]",
      "rec { x = y; inherit (pkgs) lib; }",
      "a.b.c or d",
      "f or",
      "./a/${x}/b",
      "\"interp ${a.b} tail\""
    ).foreach(stable)
  }

  test("pretty preserves comments in block positions") {
    val src = "{\n# keep me\na = 1;\n}"
    val out = Pretty(Parser.parseGreen(src))
    assert(out.contains("# keep me"), out)
    stable(src)
  }

  test("rewrite is surgical: untouched bytes identical, comments survive") {
    val src =
      """{
        |  pname = "hello";  # classic
        |  version = "2.12.1";
        |}
        |""".stripMargin
    val g = Parser.parseGreen(src)
    val out = Rewrite(g) {
      case b if b.kind == SyntaxKind.N_BINDING &&
        b.text.startsWith("version") =>
        GreenNode(b.kind, b.children.map {
          case n: GreenNode if n.kind != SyntaxKind.N_ATTRPATH => B.str("2.13.0")
          case other                                           => other
        })
    }.text
    assertEquals(out, src.replace("2.12.1", "2.13.0"))
  }

  test("views: attrset / binding / lambda accessors") {
    val root = Parser.parse("{ pkgs, lib, ... }: rec { pname = \"x\"; meta.desc = \"d\"; }")
    val lam  = root.childNodes.head.asLambda.get
    assertEquals(lam.isPattern, true)
    assertEquals(lam.patternFields, Vector("pkgs", "lib"))
    val set = lam.body.asAttrSet.get
    assertEquals(set.isRec, true)
    assertEquals(set.bindings.map(_.pathNames), Vector(Vector("pname"), Vector("meta", "desc")))
    assertEquals(set.get("pname").map(_.text), Some("\"x\""))
  }
