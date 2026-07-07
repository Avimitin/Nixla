# nixla

**Nix + Scala.** Write Nix in Scala 3: a Nix expression AST, a
recursive-descent parser, and a precedence-aware emitter that generates pure,
human-readable Nix.

```
 .nix text ──parse──▶  NixExpr AST  ──emit──▶  pure Nix text
                          ▲
                          │ build directly
                     Scala 3 eDSL
```

## Layout

| Path | Role |
|---|---|
| `nixla/src/nixla/Ast.scala` | `enum NixExpr` — the core AST |
| `nixla/src/nixla/Emit.scala` | precedence-aware pretty printer (AST → Nix) |
| `nixla/src/nixla/Dsl.scala` | Scala-facing sugar: `attrs`, `let`, `lam`, `nix"…"` interpolator |
| `nixla/src/nixla/Parser.scala` | recursive-descent parser for a practical Nix subset |
| `nixla/src/nixla/Main.scala` | demo: DSL → flake, parse → transform → re-emit |
| `nixla/test/src/…` | munit round-trip and emission tests |

## Develop

The toolchain (Mill, JDK 21, metals, scalafmt) is pinned by the flake:

```console
$ nix develop          # or use direnv: `use flake`
$ mill nixla.run    # run the demo
$ mill nixla.test   # round-trip test suite
```

Third-party JVM dependencies (munit) are resolved by Mill/Coursier from
Maven Central; the flake pins everything else.

## Verifying emitted Nix

The emitter's output is valid by construction, but the real oracle is nix
itself:

```console
$ mill nixla.run > demo.out   # then feed sections to:
$ nix-instantiate --parse <file>.nix
```

See [DESIGN.md](./DESIGN.md) for the architecture, layer model, and decision log.

## Status / roadmap

- [x] AST covering the full expression language
- [x] emitter with correct operator precedence & associativity
- [x] parser subset: literals, `"…${…}…"`, paths, lists, (rec) attrsets,
      `inherit`, `let`/`if`/`with`/`assert`, destructuring lambdas, `or`, `?`,
      all binary operators, comments
- [ ] `''indented strings''`, dynamic attrs `${…} = …`, `<search/paths>`
- [ ] property-based differential testing against `nix-instantiate --parse`
- [ ] Wadler-style layout (paiges) for width-aware line breaking
- [ ] typed eDSL layer (`NixExpr[NixStr]`, phantom types)
