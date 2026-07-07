package nixla.cst

import SyntaxKind.*

class CstSuite extends munit.FunSuite:

  /** THE invariant: parse then print reproduces the input byte-for-byte. */
  def lossless(src: String)(using munit.Location): Unit =
    assertEquals(Parser.parseGreen(src).text, src)

  test("lossless: literals and atoms") {
    List("42", "3.14", ".5", "1.", "true", "null", "x", "x'y-z",
      "\"hello\"", "./foo/bar", "/etc/nixos", "~/dot/files", "<nixpkgs/lib>",
      "https://example.com/a?b=c", "[ 1 2 3 ]", "[ ]", "{ }")
      .foreach(lossless)
  }

  test("lossless: comments and whitespace survive everywhere") {
    lossless("# leading\n1 + /* mid */ 2  # trailing comment\n")
    lossless("{\n  # doc for a\n  a = 1; # same line\n  /* block\n     comment */\n  b = 2;\n}")
    lossless("  \n\t 42 \n\n")
  }

  test("lossless: strings with escapes and interpolation") {
    lossless(""""a\"b\\c\n${x}tail"""")
    lossless(""""${a}${b."c"}"""")
    lossless("\"nested ${\"inner ${deep}\"} out\"")
  }

  test("lossless: indented strings with all escape forms") {
    lossless("''\n  plain\n  interp ${x}\n  esc-quote ''' esc-dollar ''$ esc-interp ''${x}\n''")
    lossless("''''") // empty indented string
  }

  test("lossless: lambdas and patterns") {
    lossless("x: y: x + y")
    lossless("{ a, b ? 1, ... }: a")
    lossless("{ a , b ?  1 , ... } : a") // exotic spacing
    lossless("args @ { a, b }: a")
    lossless("{ a, b } @ args: a")
    lossless("{}: 1")
  }

  test("lossless: attrsets, inherit, dynamic attrs") {
    lossless("{ a.b.c = 1; \"d e\" = 2; ${f} = 3; }")
    lossless("rec { x = y; inherit a; inherit (pkgs) stdenv lib; inherit \"q\"; }")
  }

  test("lossless: let / if / with / assert / operators") {
    lossless("let x = 1; y = x; in x + y * 2")
    lossless("if a == b then -1 else !c || d -> e")
    lossless("with pkgs; assert x >= 2; [ hello cowsay ]")
    lossless("a // b // { x = 1; }")
    lossless("a.b.c or d.e ? f.g")
  }

  test("lossless: a realistic flake") {
    lossless("""{
  description = "demo";
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  outputs = { self, nixpkgs }:
    let pkgs = nixpkgs.legacyPackages.x86_64-linux; in {
      devShells.x86_64-linux.default = pkgs.mkShell {
        packages = [ pkgs.mill pkgs.jdk21 ];  # toolchain
        shellHook = ''
          echo "welcome to ${pkgs.mill.name}"
        '';
      };
    };
}
""")
  }

  test("lexer: longest match — paths, division, URIs") {
    def kinds(src: String): List[SyntaxKind] =
      Lexer.lex(src).filterNot(t => t.isTrivia || t.kind == TK_EOF).map(_.kind).toList
    assertEquals(kinds("a/b"), List(TK_PATH))              // no spaces: path
    assertEquals(kinds("a / b"), List(TK_IDENT, TK_SLASH, TK_IDENT))
    assertEquals(kinds("a//b"), List(TK_IDENT, TK_UPDATE, TK_IDENT))
    assertEquals(kinds("f:x"), List(TK_URI))               // yes, really
    assertEquals(kinds("f: x"), List(TK_IDENT, TK_COLON, TK_IDENT))
    assertEquals(kinds("1.5"), List(TK_FLOAT))
    assertEquals(kinds("1.5/x"), List(TK_PATH))
  }

  test("structure: node kinds land where expected") {
    def rootKind(src: String): SyntaxKind =
      Parser.parse(src).childNodes.head.kind
    assertEquals(rootKind("{ a = 1; }"), N_ATTRSET)
    assertEquals(rootKind("{ a }: 1"), N_LAMBDA)
    assertEquals(rootKind("{}: 1"), N_LAMBDA)
    assertEquals(rootKind("{}"), N_ATTRSET)
    assertEquals(rootKind("f x"), N_APPLY)
    assertEquals(rootKind("a.b or c"), N_SELECT)
    assertEquals(rootKind("-x"), N_UNARY)
    assertEquals(rootKind("a ? b"), N_HAS_ATTR)
  }

  test("errors: fail fast with a position") {
    def failsAt(src: String): Int =
      intercept[ParseError](Parser.parseGreen(src)).offset
    assert(failsAt("1 +") >= 2)
    assertEquals(failsAt("{ a = 1 }"), 8)       // missing semicolon
    assert(failsAt("\"unterminated") >= 0)
    val e = intercept[ParseError](Parser.parseGreen("let x = 1;\nin x ++ "))
    assert(e.render("let x = 1;\nin x ++ ").contains("2:"), e.render("let x = 1;\nin x ++ "))
  }

  test("red tree: spans and find") {
    val root = Parser.parse("{ a = 12; }")
    val n    = root.find(6) // inside `12`
    assertEquals(n.kind, N_LITERAL)
    assertEquals(n.span, Span(6, 8))
    assertEquals(n.text, "12")
  }
