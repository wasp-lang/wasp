# Phase 3: Haskell IR & Entry Point Detection

`WaspProjectMode`, `ModuleSpec`, `EntityDeclaration`, and `module.wasp.ts` detection.

## Goal

The compiler can distinguish between app projects (`main.wasp.ts`) and module projects (`module.wasp.ts`), and has dedicated Haskell types to represent a parsed module spec.

## Files to Create/Modify

| File | Change |
|---|---|
| `Wasp/Project/Common.hs` | Add `WaspProjectMode` type |
| `Wasp/Project/WaspFile.hs` | Detect `module.wasp.ts` → `ModuleMode` |
| New: `Wasp/AppSpec/Module.hs` | `ModuleSpec`, `EntityDeclaration` types |
| `Wasp/Project/Analyze.hs` | Branch: `analyzeAppProject` vs `analyzeModuleProject` |

## Details

### 1. `WaspProjectMode`

```haskell
-- Wasp/Project/Common.hs
data WaspProjectMode = AppMode | ModuleMode
  deriving (Show, Eq)
```

### 2. Entry Point Detection

```haskell
-- Wasp/Project/WaspFile.hs
findWaspFile :: Path' Abs (Dir WaspProjectDir)
             -> IO (Either String (WaspProjectMode, WaspFilePath))
```

Logic:
- `main.wasp.ts` or `main.wasp` present → `AppMode`
- `module.wasp.ts` present → `ModuleMode`
- Both present → error
- Neither present → error

### 3. `ModuleSpec` IR

```haskell
-- Wasp/AppSpec/Module.hs
data ModuleSpec = ModuleSpec
  { msPackageName    :: String
  , msEntities       :: [EntityDeclaration]
  , msRequiresAuth   :: Bool
  , msQueries        :: [(String, AS.Query.Query)]
  , msActions        :: [(String, AS.Action.Action)]
  , msPages          :: [(String, AS.Page.Page)]
  , msRoutes         :: [(String, AS.Route.Route)]
  , msApis           :: [(String, AS.Api.Api)]
  , msCruds          :: [(String, AS.Crud.Crud)]
  , msJobs           :: [(String, AS.Job.Job)]
  , msProvides       :: Map String Aeson.Value
  , msServerSetupFn  :: Maybe AS.ExtImport.ExtImport
  }

data EntityDeclaration = EntityDeclaration
  { edName   :: String
  , edFields :: Map String String  -- fieldName → Prisma field type string
  }
```

Operation types (`AS.Query.Query`, `AS.Action.Action`, etc.) are reused from app mode — no duplication.

### 4. `Analyze.hs` Branching

The analysis phase checks the project mode and either runs the existing app analysis or a new module analysis path that populates a `ModuleSpec`.

## Verification

- `findWaspFile` returns `ModuleMode` for a directory containing `module.wasp.ts`
- `findWaspFile` returns `AppMode` for a directory containing `main.wasp.ts`
- `findWaspFile` errors when both are present
- `ModuleSpec` can be constructed from parsed JSON output of `wasp-config`
