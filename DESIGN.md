# nixla — Design

**Nix + Scala.** A Scala 3 library for writing Nix: a lossless Nix CST,
a deterministic parser and emitter, a typed eDSL, and compile-time-checked
Nix quasiquotes.

This document records the agreed architecture and the decisions behind it.
It is normative: code that contradicts it is a bug in one of the two.

## Goals

1. **Lossless CST** — parsing a file and printing it back yields the input,
   byte for byte, including comments and whitespace.
2. **Deterministic parse and emit** — same input, same tree; same tree, same
   output. No ambiguity, no iteration-order dependence, no randomness.
3. **Scala-first user interface** — users write Scala functions with typed
   arguments that build Nix expressions; verbatim Nix lives in quasiquotes;
   the two compose.
4. **Staged verification** ("the meta compiler") — Scala types, then nixla's
   parser at Scala compile time, then the real Nix CLI as the final oracle at
   test time.

## Non-goals

- **No CLI / binary.** nixla is a library for writing Nix from Scala, not a
  replacement for the `nix` command.
- **No nixfmt-rfc-style conformance.** The pretty-printer emits readable,
  deterministic code in *nixla's* documented style. Upstream formatter styles
  change; we do not chase them.
- **No Nix evaluator** (for now). Semantic questions are delegated to the Nix
  CLI.

## Architecture

```
 Layer 4  ToNix derivation      case classes / named tuples -> Nix[Attrs]
 Layer 3  Nix[T] phantom eDSL   typed construction, user-facing functions
 Layer 2  typed views           AttrSet, Lambda, ... (zero-cost wrappers)
 Layer 1  red tree              SyntaxNode: offsets, parents, spans
 Layer 0  green tree            GreenNode/GreenToken: the only source of truth
```

Each layer speaks only to the one below. The quasiquote macro (`nixq`) sits
beside layers 3–4 and produces layer-0 trees through the same parser users
call at runtime.

### Layer 0 — green tree (lossless by construction)

- `GreenToken(kind, text)` and `GreenNode(kind, children)` where children are
  `GreenToken | GreenNode`, in source order.
- **Trivia (whitespace, comments) are ordinary tokens** in the tree, sitting
  between siblings. Nothing is "attached"; nothing can be lost.
- The lossless invariant is structural: `tree.text` is the concatenation of
  all descendant token texts, so `parse(src).text == src` holds by
  construction and is sealed by a property test.
- Trees are immutable; edits rebuild the spine from the changed node to the
  root. Untouched subtrees keep their exact bytes → minimal diffs.

Trivia placement convention (determinism): leading trivia belongs to the
*parent*; within a node, trivia preceding each expected token belongs to that
node. Trailing trivia after a node's last significant token belongs to the
ancestor that consumes the next significant token.

### Layer 1 — red tree

`SyntaxNode(green, parent, offset)` computed on demand: absolute spans,
parent navigation, `find(offset)`. Needed for error messages and for mapping
quasiquote parse errors onto Scala compile-error positions.

### Parser

- Separate **lexer** with mode stack (default / `"…"` string / `''…''`
  indented string), faithful to Nix's flex rules: longest match wins.
  This gets path-vs-division, URI literals, and `f:x`-is-a-URI right by
  construction.
- Recursive descent over the token stream, building green nodes.
- **Fail-fast** with `ParseError(message, offset)`; `N_ERROR` kind is reserved
  for future error tolerance but unused.
- Backtracking only where Nix's grammar demands lookahead
  (`{ a, b }: …` pattern vs. `{ a = 1; }` attrset), implemented by index
  reset — cheap because green construction is pure.

### Emit

Two functions, two contracts:

- `text` — identity. Lossless reproduction of whatever the tree holds.
- `pretty` — canonical printer. A pure function of the tree; documented
  style; golden-tested; stable within a nixla version. Synthesized nodes
  (DSL-built or spliced) are rendered by `pretty` at their insertion column;
  verbatim regions keep their bytes.

### Layers 3–4 — typed eDSL

- `Nix[T]` phantom-typed wrappers over view construction: `Nix[Str]`,
  `Nix[Int]`, `Nix[Attrs]`, `Nix[Drv]`, … Ill-kinded Nix is a Scala type
  error. Attrset row-typing is out of scope initially.
- `Ref extends Dynamic` for fluent selection/application:
  `pkgs.legacyPackages.jdk21`, `pkgs.mkShell(...)`.
- `fn { (self, nixpkgs) => … }` — a macro reads the Scala lambda's parameter
  names to build the Nix destructuring pattern. Hygiene comes from Scala's
  own binders.
- `ToNix[A]` type class with `Mirror`-based derivation: case classes and
  named tuples become attrsets (`derives ToNix`). ToNix *produces* phantom-
  typed values; the two are one layered system, not alternatives.

### Quasiquotes (`nixq`)

```scala
val file = nixq"""
  { pkgs ? import <nixpkgs> {} }:
  ${jvmShell(pkgs.jdk21)}
"""
```

- `${scalaExpr}` — **Scala splice** (first-class; Scala is the host).
- `$${…}` — escaped: literal Nix interpolation `${…}` in the output.
- The macro parses the skeleton **at Scala compile time** with nixla's own
  parser; syntax errors become Scala compile errors positioned at the
  offending column (red-tree spans → `Position`).
- Splices are typed by grammatical position: expression (`Nix[T]`/`ToNix`),
  bindings-plural (`Seq[Binding]`, spliced with `*`), attr name
  (`Nix[Str]` → dynamic attr), inside-string (`Nix[Str]` → interpolation).
- Skeleton bytes stay verbatim; splices are `pretty`-rendered at the graft
  point.

### Verification pipeline

```
Scala compiler ─▶ nixla parser ─▶ deterministic emit ─▶ Nix CLI
 (types)          (syntax, at       (pretty/lossless)    (parse + eval,
                   compile time)                          test time)
```

- The macro **never** shells out to `nix` — compilation stays hermetic.
- `nixla.testkit` provides fixtures that run `nix-instantiate --parse` and
  optionally `nix eval` against rendered output; the flake guarantees the
  `nix` binary in the dev shell.
- Runtime API: `render` (pure) and `check(): Either[NixError, Unit]` (CLI).
- Known limitation: `with` defeats static scope-checking of splices. We warn
  on definitely-unbound only; the CLI gate is the authority.

## Milestones

- **M1 — CST core**: green/red trees, lexer, parser rewrite, lossless
  property test. ✅
- **M2 — parser completion**: interpolated paths (`./a/${b}`), contextual
  `or`, trailing-slash/comment errors, better messages. ✅
- **M3 — emit**: `pretty` canonical printer, views, builders with
  precedence-correct auto-parens; legacy AST deleted at parity. ✅
- **M4 — typed layer**: `Nix[T]`, `Ref`/`Dynamic`, `fn` macro, `ToNix`
  derivation (case classes + named tuples). ✅
- **M5 — quasiquotes**: `nixq` macro over M1–M4; compile-time parse, four
  graft positions, `$${}` escape, dry-run positional validation. ✅
- **M6 — testkit**: `NixCli` parse/eval gates, `Nix#check()`. ✅
  **TODO** (deferred by decision #4): differential fuzzing of parser/emitter
  against `nix-instantiate --parse` with generated ASTs and nixpkgs corpora.
  **TODO**: indent-aware graft rendering (grafts currently render at column
  0); per-position splice typing (expression vs string vs attr name).

## Decision log

| # | Decision | Rationale |
|---|---|---|
| 1 | Lossless CST, rowan-style (AST = views over green tree) | serious round-trip tooling; one source of truth |
| 2 | Deterministic parse & emit as a contract | reproducibility is the point of Nix |
| 3 | No nixfmt-rfc-style guarantee; own documented style | upstream style is a moving target |
| 4 | Differential testing = TODO at M6 | integration-test timing question, not architecture |
| 5 | ToNix layered on phantom types (not competing routes) | ToNix is the high-level trait; phantom types are token-level |
| 6 | Library only, no CLI binary | nixla writes Nix; it does not replace `nix` |
| 7 | `${}` = Scala splice; `$${}` = literal Nix interpolation | Scala is the host language and first-class citizen |
| 8 | Scala 3.7.x (leave 3.3 LTS) | named tuples for ToNix; scalac won't break Scala 3 syntax |
| 9 | Fail-fast parser now; `N_ERROR` reserved | codegen library first; IDE tolerance can come later |
| 10 | Macro never invokes Nix CLI | hermetic compilation; CLI verification lives in testkit |
