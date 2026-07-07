# scala-nix

Write Nix in Scala 3: a Nix expression AST, a recursive-descent parser, and a
precedence-aware emitter that generates pure, human-readable Nix.

```
 .nix text ──parse──▶  NixExpr AST  ──emit──▶  pure Nix text
                          ▲
                          │ build directly
                     Scala 3 eDSL
```

## Layout

| Path | Role |
|---|---|
| `scalanix/src/scalanix/Ast.scala` | `enum NixExpr` — the core AST |
| `scalanix/src/scalanix/Emit.scala` | precedence-aware pretty printer (AST → Nix) |
| `scalanix/src/scalanix/Dsl.scala` | Scala-facing sugar: `attrs`, `let`, `lam`, `nix"…"` interpolator |
| `scalanix/src/scalanix/Parser.scala` | recursive-descent parser for a practical Nix subset |
| `scalanix/src/scalanix/Main.scala` | demo: DSL → flake, parse → transform → re-emit |
| `scalanix/test/src/…` | munit round-trip and emission tests |

## Develop

The toolchain (Mill, JDK 21, metals, scalafmt) is pinned by the flake:

```console
$ nix develop          # or use direnv: `use flake`
$ mill scalanix.run    # run the demo
$ mill scalanix.test   # round-trip test suite
```

Third-party JVM dependencies (munit) are resolved by Mill/Coursier from
Maven Central; the flake pins everything else.

## Verifying emitted Nix

The emitter's output is valid by construction, but the real oracle is nix
itself:

```console
$ mill scalanix.run > demo.out   # then feed sections to:
$ nix-instantiate --parse <file>.nix
```

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
