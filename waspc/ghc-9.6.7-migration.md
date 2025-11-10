# GHC 9.6.7 Migration

## Overview

This document tracks the migration from GHC 9.4.8 to GHC 9.6.7.

## Dependency Updates

The migration required updating several package constraints in `waspc.cabal` to resolve conflicts with GHC 9.6.7's boot libraries.

### 1. template-haskell: ^>= 2.19.0 → ^>= 2.20.0
- GHC 9.6.7 ships with template-haskell-2.20.0.0
- The previous constraint `^>= 2.19.0` means `>= 2.19.0 && < 2.20.0`, which excludes 2.20.0.0

### 2. transformers: ^>= 0.5.6.2 → ^>= 0.6.1
- GHC 9.6.7 ships with transformers-0.6.1.0 (boot library)
- The previous constraint `^>= 0.5.6.2` means `>= 0.5.6.2 && < 0.6`, which excludes 0.6.x

### 3. mtl: ^>= 2.2.2 → ^>= 2.3
- GHC 9.6.7 ships with mtl-2.3.1 (boot library)
- The previous constraint `^>= 2.2.2` means `>= 2.2.2 && < 2.3`, which excludes 2.3.x
- mtl-2.3.x supports transformers-0.6 (older mtl versions require transformers < 0.6)

### 4. lsp: ^>= 1.4.0.0 → ^>= 1.6
- lsp-1.4.0.0 requires transformers < 0.6
- lsp-1.6.0.0 supports transformers >= 0.5.6 && < 0.7, compatible with transformers-0.6

### 5. lsp-types: ^>= 1.4.0.1 → ^>= 1.6
- lsp-1.6.0.0 requires lsp-types-1.6

## Configuration Changes

### cabal.project

Updated GHC version:
```
with-compiler: ghc-9.6.7
```

## Code Changes

### mtl 2.3 Compatibility

The update from mtl 2.2 to 2.3 required code changes due to the removal of base module re-exports.

**Background:**
- mtl 2.2.x re-exported functions from `Control.Monad`, `Control.Monad.Fix`, and `Data.Monoid` in various mtl modules
- mtl 2.3 removed these re-exports for PVP compliance
- Functions like `when`, `liftIO`, and `MonadIO` must now be imported directly from their base modules

#### 1. src/Wasp/Generator/NpmInstall.hs

**Issue:** `when` was imported from `Control.Monad.Except` but is no longer re-exported in mtl 2.3

**Fix:**
```haskell
-- Before:
import Control.Monad.Except (MonadError (throwError), runExceptT, when)

-- After:
import Control.Monad (when)
import Control.Monad.Except (MonadError (throwError), runExceptT)
```

#### 2. waspls/src/Wasp/LSP/Prisma/Analyze.hs

**Issue:** `liftIO` was imported from `Control.Monad.Cont` but is no longer re-exported in mtl 2.3

**Fix:**
```haskell
-- Before:
import Control.Monad.Cont (liftIO)

-- After:
import Control.Monad.IO.Class (liftIO)
```

#### 3. cli/src/Wasp/Cli/Command/CreateNewProject/AI.hs
#### 4. cli/src/Wasp/Cli/Command/Info.hs
#### 5. cli/src/Wasp/Cli/Command/BuildStart/Config.hs

**Issue:** `MonadIO` and `liftIO` were imported from `Control.Monad.Except` but are no longer re-exported in mtl 2.3

**Fix (same pattern for all three files):**
```haskell
-- Before:
import Control.Monad.Except (MonadError (throwError), MonadIO (liftIO))

-- After:
import Control.Monad.Except (MonadError (throwError))
import Control.Monad.IO.Class (MonadIO (liftIO))
```

### lsp 1.6 Compatibility

The update from lsp 1.4 to 1.6 required changes to logging setup.

#### waspls/src/Wasp/LSP/Server.hs

**Issue:** `setupLogger` function was removed from lsp in version 1.6. Logging is now handled automatically by `runServer` via `LogAction` from `co-log-core`.

**Fix:**
Made `setupLspLogger` a no-op since lsp 1.6's `runServer` handles logging automatically:

```haskell
-- Before:
setupLspLogger :: Maybe FilePath -> IO ()
setupLspLogger Nothing = pure ()
setupLspLogger (Just "[OUTPUT]") = LSP.setupLogger Nothing [] System.Log.Logger.DEBUG
setupLspLogger file = LSP.setupLogger file [] System.Log.Logger.DEBUG

-- After:
-- NOTE: In lsp 1.6+, logging is handled automatically by runServer via LogAction.
-- This function is kept as a no-op for compatibility but doesn't configure logging anymore.
setupLspLogger :: Maybe FilePath -> IO ()
setupLspLogger _ = pure ()
```

**Note:** The `System.Log.Logger` import may now be unused and could be removed if not used elsewhere.

## Build Status

✅ Dependency resolution successful
✅ Full build complete: `cabal build --enable-tests --enable-benchmarks`

## Next Steps

- Run test suite to ensure functionality is preserved
