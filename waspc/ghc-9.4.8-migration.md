# GHC 9.4.8 Migration

## Overview

This document tracks the migration from GHC 9.2.8 to GHC 9.4.8.

## Dependency Updates

The migration required updating several package constraints in `waspc.cabal` to resolve conflicts with GHC 9.4.8's boot libraries.

### 1. template-haskell: ^>= 2.18.0 → ^>= 2.19.0

**Location:** waspc.cabal:129

**Reason:**
- GHC 9.4.8 ships with template-haskell-2.19.0.0
- The previous constraint `^>= 2.18.0` means `>= 2.18.0 && < 2.19.0`, which excludes 2.19.0.0
- GHC to template-haskell mapping:
  - GHC 9.2.8 → template-haskell 2.18.0.0
  - GHC 9.4.8 → template-haskell 2.19.0.0

### 2. lens: ^>= 5.1 → ^>= 5.2

**Locations:** waspc.cabal:500, 534, 735 (3 occurrences)

**Reason:**
- lens-5.2+ has explicit support for template-haskell 2.19.0.0 (GHC 9.4)
- lens-5.1.x requires template-haskell < 2.19, which conflicts with GHC 9.4.8

### 3. aeson: ^>= 2.0 → ^>= 2.2

**Location:** waspc.cabal:126

**Reason:**
- GHC 9.4.8 ships with base-4.17.2.1 which includes ghc-prim-0.9.1
- aeson-2.0.x requires ghc-prim < 0.9, excluding version 0.9.1
- aeson-2.2.x supports ghc-prim-0.9.1 and is compatible with GHC 9.4.8

### 4. lens-aeson: ^>= 1.1.3 → ^>= 1.2

**Location:** waspc.cabal:535

**Reason:**
- lens-aeson-1.1.3 requires aeson < 2.1
- With aeson updated to 2.2, we need lens-aeson-1.2+ which supports aeson 2.2

## Configuration Changes

### cabal.project

Updated GHC version:
```
with-compiler: ghc-9.4.8
```

## Code Changes

### lens-aeson 1.2 Compatibility

The update from lens-aeson 1.1.3 to 1.2 required code changes due to API changes in how `_Object` works.

**Background:**
- lens-aeson 1.1.3 (used with aeson 2.0): `_Object` returned `HashMap Text Value`
- lens-aeson 1.2+ (used with aeson 2.2): `_Object` returns `KeyMap Value`

This breaking change in lens-aeson's API required updating code that used `_Object` with HashMap functions.

#### cli/src/Wasp/Cli/Command/Build.hs

**Issue:** Code used `HM.filterWithKey` on the result of `_Object`, which now returns `KeyMap` instead of `HashMap`.

**Changes:**

1. Updated imports (lines 11-12):
```haskell
-- Before:
import qualified Data.HashMap.Strict as HM

-- After:
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
```

2. Updated filterWithKey usage (lines 153-155):
```haskell
-- Before:
        & key "packages" . _Object
          %~ HM.filterWithKey
            (\packageLocation _ -> not $ isWaspConfigPackageLocation packageLocation)

-- After:
        & key "packages" . _Object
          %~ KM.filterWithKey
            (\packageLocation _ -> not $ isWaspConfigPackageLocation (Key.toString packageLocation))
```

3. Updated function signature (line 157):
```haskell
-- Before:
    isWaspConfigPackageLocation :: Text -> Bool

-- After:
    isWaspConfigPackageLocation :: String -> Bool
```

**Note:** The `Key` type from aeson needs to be converted to `String` using `Key.toString` before passing to `isWaspConfigPackageLocation`.

## Build Status

✅ Dependency resolution successful
✅ Full build complete: `cabal build --enable-tests --enable-benchmarks`

## Next Steps

- Run test suite to ensure functionality is preserved
