# nixla

**Nix + Scala.** Write Nix in Scala 3: a lossless Nix CST, a deterministic
parser and pretty-printer, a phantom-typed eDSL, and compile-time-checked
Nix quasiquotes — verified end-to-end by the real Nix CLI.

```
Scala compiler ──▶ nixla parser ──▶ deterministic emit ──▶ Nix CLI
 (types)           (syntax, at       (pretty | lossless)   (parse + eval,
                    compile time)                            test time)
```

## A taste

```scala
import nixla.dsl.{*, given}

// typed pieces: named tuples are attrsets, Scala params are Nix patterns
val flake = (
  description = "dev shell",
  outputs = fn { (self, nixpkgs) =>
    val pkgs = nixpkgs.legacyPackages.`x86_64-linux`
    (devShells = (default = pkgs.mkShell(
      packages  = List(pkgs.mill, pkgs.jdk21).nix,
      shellHook = nix"echo JDK is ${pkgs.jdk21.version}",
    ).nix).nix).nix
  }
).nix

// quasiquotes: literal Nix, parsed by nixla's parser INSIDE scalac —
// a syntax error here is a Scala compile error
val shell = pkgs.mkShell(packages = List(pkgs.hello).nix)
val q = nixq"""{ pkgs ? import <nixpkgs> { } }: ${shell.nix}"""

flake.render    // canonical Nix source
q.text          // verbatim skeleton + canonically-rendered grafts
flake.check()   // Right(()) — the real Nix parser agrees
```

Case classes work too: `case class Meta(description: String) derives ToNix`.

## Layers

| Layer | Where | What |
|---|---|---|
| 4 | `ToNix` | Mirror derivation: case classes / named tuples → attrsets |
| 3 | `Nix[T]`, `Ref`, `fn`, `nixq` | phantom types, dynamic refs, macros |
| 2 | `cst.views`, `cst.Rewrite` | typed accessors, surgical green rewriting |
| 1 | `cst.SyntaxNode` | red tree: spans, parents, `find(offset)` |
| 0 | `cst.GreenNode` | lossless green tree — the single source of truth |

Guarantees, tested:

- **lossless**: `parse(src).text == src`, byte for byte, comments included
- **deterministic**: same input → same tree → same output; `Map` keys sorted
- **pretty is a fixpoint**: `pretty(parse(pretty(parse(s)))) == pretty(parse(s))`
- **surgical rewrites**: untouched subtrees keep their exact bytes
- **compile-time syntax**: `nixq"1 +"` does not compile
- **CLI-verified**: rendered output parses and evaluates under `nix-instantiate`

See [DESIGN.md](./DESIGN.md) for the architecture and decision log.

## Develop

The toolchain (Mill, JDK 21, metals, scalafmt) is pinned by the flake;
JVM dependencies (munit) come from Maven via Mill/Coursier.

```console
$ nix develop
$ mill nixla.test   # 5 suites: CST, Pretty, typed layer, nixq, Nix-CLI gate
$ mill nixla.run    # demo: build + surgical rewrite
```

## Status

- [x] M1 — lossless CST core (green/red trees, mode-stack lexer, parser)
- [x] M2 — parser completion (interpolated paths, contextual `or`, errors)
- [x] M3 — Pretty printer, views, builders; legacy AST retired
- [x] M4 — typed layer (`Nix[T]`, `Ref`/Dynamic, `fn` macro, `ToNix`)
- [x] M5 — `nixq` quasiquotes (compile-time parse, typed splices, `$${}`)
- [x] M6 — testkit (`NixCli` parse/eval gates)
- [ ] TODO: differential fuzzing against `nix-instantiate --parse` (nixpkgs corpora)
- [ ] TODO: indent-aware graft rendering; splice typing by position
- [ ] TODO: publish to Maven (`mill.publish`)
