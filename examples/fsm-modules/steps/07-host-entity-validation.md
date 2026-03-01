# Phase 7: Host App Entity Validation

Validate entity maps when a host app `app.use()`s a module.

## Goal

When compiling a host app that uses modules, the compiler validates that the entity map satisfies the module's entity declarations. Clear error messages guide the developer to fix mismatches.

## Validation Rules

1. Every declared entity alias must be mapped in `entityMap`
2. Mapped host entities must exist in the host's `schema.prisma`
3. For each declared field, the host entity must have a field with a compatible base type
4. Base type comparison only — host modifiers like `@default`, `@unique` are ignored
5. The host entity may have additional fields not declared by the module
6. If the module `requiresAuth`, the host must have auth configured

## Files to Create/Modify

| File | Change |
|---|---|
| New: `Wasp/Validator/ModuleValidator.hs` | `validateModuleEntityMaps` |
| Existing validation pipeline | Call `validateModuleEntityMaps` during app compilation |

## Validation Logic

```haskell
validateModuleEntityMaps :: AppSpec -> [ModuleUse] -> Either [CompileError] ()
validateModuleEntityMaps spec moduleUses = do
  forM_ moduleUses $ \mu -> do
    let pkgName = muPackageName mu
    let entityMap = muEntityMap mu
    let entityDecls = muEntityDeclarations mu

    -- 1. Every declared entity must be mapped
    forM_ entityDecls $ \decl ->
      unless (Map.member (edName decl) entityMap) $
        throwError $ UnmappedEntityError pkgName (edName decl)

    -- 2. Mapped entities must exist in host schema
    forM_ (Map.toList entityMap) $ \(alias, realName) ->
      unless (entityExists spec realName) $
        throwError $ EntityNotFoundError pkgName alias realName

    -- 3. Field compatibility (base type only)
    forM_ entityDecls $ \decl ->
      case Map.lookup (edName decl) entityMap of
        Just realName -> validateFieldsCompatible pkgName (edName decl) realName
                           (edFields decl) (getEntityFields spec realName)
        Nothing -> pure ()

    -- 4. Auth requirement
    when (muRequiresAuth mu && not (hasAuth spec)) $
      throwError $ AuthRequiredError pkgName
```

## Error Messages

```
Error: Module '@myorg/payments-module' declares entity 'Subscription' but no mapping
was provided. Add 'Subscription' to the entityMap in app.use().

Error: Module '@myorg/payments-module' requires field 'stripeCustomerId: String' on
entity 'Subscription' (mapped to 'UserSubscription'), but 'UserSubscription' does not
have this field.

  entityMap: { Subscription: "UserSubscription" }
                               ^^^^^^^^^^^^^^^^^
  Add 'stripeCustomerId String' to your UserSubscription model in schema.prisma.

Error: Module '@myorg/payments-module' requires auth but app.auth() is not configured.
```

## Verification

- Missing entity map entry → clear error pointing to the missing alias
- Host entity missing a required field → error with field name and suggestion
- Host entity with extra fields → no error (superset is fine)
- Module requires auth but host has none → clear auth error
- All fields match → validation passes
