# dev-tool.project GHC 9.4.8 Migration

This document describes the dependency updates required for dev-tool.project when updating to GHC 9.4.8.

## Summary of Changes

All development tools needed version updates to be compatible with GHC 9.4.8:

- **ormolu**: `0.4.0.0` → `0.5.3.0`
- **hlint**: `3.5.0` → `3.6.1`
- **stan**: `0.2.1.0` (no change needed)
- **prune-juice**: `0.7` (no change needed)

## Detailed Conflicts and Fixes

### ormolu: 0.4.0.0 → 0.5.3.0

**Problem:**
```
Error: [Cabal-7107]
Could not resolve dependencies:
[__1] rejecting: ghc-lib-parser-9.12.2.20250421 (conflict: ormolu => ghc-lib-parser>=9.2 && <9.3)
[__2] rejecting: base-4.17.2.1/installed-4.17.2.1 (conflict: ghc-lib-parser => base>=4.14 && <4.17)
```

**Root cause:**
- ormolu 0.4.0.0 requires ghc-lib-parser 9.2.x (constraint: `>=9.2 && <9.3`)
- ghc-lib-parser 9.2.x requires base 4.14-4.16
- GHC 9.4.8 comes with base 4.17.2.1, which is incompatible

**Solution:**
Updated to ormolu 0.5.3.0, which uses ghc-lib-parser 9.4.x

**Why this version:**
- ormolu 0.5.1.0 introduced ghc-lib-parser 9.4 support
- ormolu 0.5.3.0 is the latest in the 0.5.x series with bug fixes
- Uses ghc-lib-parser-9.4.8.20231111, perfectly compatible with GHC 9.4.8

---

### hlint: 3.5.0 → 3.6.1

**Problem:**
```
Error: [Cabal-7107]
Could not resolve dependencies:
[__0] rejecting: hlint; 3.10, 3.8, 3.6.1, 3.6, 3.5, 3.4.1, ... (constraint from dev-tool.project requires ==3.5.0)
```

**Root cause:**
- Version 3.5.0 doesn't exist on Hackage
- Only version 3.5 (without patch version) exists

**Solution:**
Updated to hlint 3.6.1

**Why this version:**
- hlint 3.5 introduced GHC 9.4 support (2022-09-20)
- hlint 3.6 was released on 2023-06-26
- hlint 3.6.1 is a stable bugfix release from 2023-07-03
- Uses ghc-lib-parser-9.6.7.20250325 (forward compatible)
- Fully supports GHC 9.4.8

---

### stan: 0.2.1.0 (no change)

**Status:** ✅ Already compatible with GHC 9.4.8

No dependency conflicts detected in dry run.

---

### prune-juice: 0.7 (no change)

**Status:** ✅ Already compatible with GHC 9.4.8

No dependency conflicts detected in dry run.

---

## Testing

All tools were verified with dry runs:

```bash
cabal --project-file=dev-tool.project install ormolu --dry-run      # ✅ Success
cabal --project-file=dev-tool.project install hlint --dry-run       # ✅ Success
cabal --project-file=dev-tool.project install stan --dry-run        # ✅ Success
cabal --project-file=dev-tool.project install prune-juice --dry-run # ✅ Success
```

## References

- Ormolu changelog: https://github.com/tweag/ormolu/blob/master/CHANGELOG.md
- HLint support for GHC 9.4: https://github.com/ndmitchell/hlint/issues/1413
- Stackage LTS 21 (GHC 9.4): https://www.stackage.org/blog/2023/06/announce-lts-21-nightly-ghc9.6
