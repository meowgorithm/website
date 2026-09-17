# AGENTS.md

Personal website ("rocha"): a Haskell webserver that serves HTML pages generated server-side, plus a small TypeScript/SCSS frontend compiled by Vite.

## Two toolchains

This repo intentionally mixes two build systems. Both must work for a full build:

1. **Haskell server** (`app/`, built with cabal, executable `webserver`)
2. **Frontend assets** (`js/`, `css/` → compiled by Vite into `static/`)

## Commands

### Frontend
```sh
npm install        # first time
npm run build      # vite build (outputs static/main.js and static/main.css)
npm start          # vite build --watch (dev mode, same output dir)
```

### Server
```sh
nix develop        # pinned dev environment (GHC 9.10.3, HLS, cabal, nodejs_22, zlib)
cabal build        # inside nix develop
```

### Running in dev
```sh
ROCHA_DEBUG=true cabal run webserver
```
`ROCHA_DEBUG=true` makes `main` spawn `npm install && npm start` (Vite watch build) concurrently with the server, so CSS/JS rebuild automatically. Config is env vars with the `ROCHA_` prefix (`ROCHA_PORT`, default 8000; `ROCHA_DEBUG`, default false).

### Production build
```sh
nix build          # produces a derivation with the binary + static assets
docker build .     # multi-stage: static Haskell binary (Alpine) + Vite assets
```

## Gotchas

- **`static/` is generated and gitignored.** Vite outputs `static/main.css` and `static/main.js` there (unhashed filenames configured in `vite.config.ts`, because the server does its own hashing, see below). Don't commit it; don't expect it to exist after a fresh clone — run `npm run build` first.
- **GHC version is pinned to 9.10.3** via `Dockerfile` (`haskell:9.10.3`), `flake.nix` (`ghc9103` from locked nixpkgs-unstable, since stable branches don't ship 9.10.3), and `cabal.project.freeze` (which pins `base ==4.20.2.0`). Mismatched GHCs make cabal fail to resolve dependencies (freeze conflicts) and HLS cradles error out. Keep all three in sync; regenerate the freeze file (`cabal freeze`) rather than hand-editing it. `cabal.project` also pins a Hackage `index-state` for reproducible resolution. GHC >= 9.10 requires `ghc-options: -threaded` on the executable (Warp's timer manager errors otherwise at request time).
- **Cache busting via content hashes**: on startup (non-debug mode), `app/Static.hs` SHA256-hashes `static/main.css` and `static/main.js` (7-char hash) and writes copies like `static/main.abc1234.css`. The `page` template in `Main.hs` looks up hashed names from a `Map FilePath Text` (original path → hashed path), falling back to `/static/main.css` when the map is empty (debug mode / error pages). If you add new hashed assets, add them to `staticFilesSources` in `runWebserver` and look them up in `page`.
- **No tests exist.** There is no test-suite in `rocha.cabal` and no frontend test setup. Don't look for one; don't invent one without asking.
- **`CHANGELOG.md` is a stale template** (leftover from a differently-named project). Version in the cabal file is `0.1.0.0` and unused.
- **`nix build` uses `buildNpmPackage` with a pinned `npmDepsHash`** in `flake.nix`. If you change `package.json`/`package-lock.json`, you must update that hash (replace with a fake hash to get the correct one from the build error).
- **Flake builds only see git-tracked files.** `src = ./.` in a flake copies tracked files only, so new build inputs (like `vite.config.ts`) must be `git add`ed before `nix build` can use them.

## Code conventions

- Haskell: 4-space indentation, `-Wall` enabled, `GHC2021`, per-module LANGUAGE pragmas (`OverloadedStrings`, `OverloadedRecordDot` + `NoFieldSelectors` so `cfg.port` works on records).
- All HTML is generated with blaze-html in `Main.hs`; there are no template files. Pages use the shared `page` shell (head/stylesheet/script) and error pages (`404`, `403`, `500`) are thrown as custom `Except` exceptions handled by `S.defaultHandler`.
- Logging goes to stderr via the tiny `Log` module (`[INFO]`, `[WARN]`, `[FATAL]` prefixes); Warp request logging via `logStdout`/`logStdoutDev` depending on debug mode.
- Frontend entry point is `js/main.ts`, which only imports `css/main.scss`; SCSS is split into `reset`/`mixins`/`fonts` modules used via `@use`.
