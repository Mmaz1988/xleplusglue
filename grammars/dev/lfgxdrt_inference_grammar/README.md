# LFGxDRT inference grammar

This directory is a development copy of `grammars/demo/fracas_inference_grammar`
intended for the LFGxDRT parsing and reasoning path.

Use this grammar path when selecting it explicitly:

```text
./grammars/dev/lfgxdrt_inference_grammar/main_lfgxdrt_inference_grammar.lfg.glue
```

Runtime settings expected by this grammar:

```tcl
set semParser 2
set processDRT 1
set mcEncoding 1
```

Notes:

- `main_lfgxdrt_inference_grammar.lfg.glue` is the canonical editable source.
- The copied FRACAS `.lfg.glue` sources have been converted from
  Prolog-style DRT constructors such as `lam`, `app`, `merge`, `drs`, `pred`,
  `rel`, and `eq` to the LFGxDRT literal notation used in
  `grammars/dev/glue-basic-drt.lfg.glue`.
- The pure `.lfg` files are generated/runtime artifacts copied only for layout
  compatibility; they are not maintained by hand here.
- No `*.fileindexdir` cache is kept in this directory.
- The grammar has not been validated with a fresh XLE run in this setup.
