# Aeson 2.0 Breaking Changes & Fixes

## Key Breaking Changes

### 1. Object type is now abstract (KeyMap instead of HashMap)
- **Before:** `type Object = HashMap Text Value`
- **Now:** `type Object = KeyMap Value` (abstract type)
- **Fix:** Use `Data.Aeson.KeyMap` instead of `Data.HashMap.Strict`

### 2. Removed `Data.Aeson.Encode` module
- If code imports this, it will fail

### 3. Infinity encoding changed
- `Double`/`Float` infinities now encode as `"+inf"` and `"-inf"`

### 4. Behavioral changes
- `FromJSON ()` and `FromJSON (Proxy tag)` now accept any JSON value

## Known Issues in waspc

### src/Wasp/Util.hs:111 (ONLY LOCATION FOUND)
```haskell
-- Current (broken):
import qualified Data.HashMap.Strict as M
jsonSet key value (Aeson.Object o) = Aeson.Object $ M.insert key value o

-- Fix:
import qualified Data.Aeson.KeyMap as KM
jsonSet key value (Aeson.Object o) = Aeson.Object $ KM.insert key value o
```

Note: `KeyMap` API is similar to `HashMap`: insert, lookup, delete, etc.
Keys are `Key` type (convertible from Text via `fromText`).

## Search Summary

- ✅ No usage of removed `Data.Aeson.Encode` module
- ✅ All `withObject` usages are proper parser combinators (no changes needed)
- ⚠️  Only 1 location directly manipulates Object with HashMap operations
- ℹ️  Once build completes, may find additional issues with TH-derived instances
