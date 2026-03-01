{{={= =}=}}
import { registerJob } from '{= jobExecutorImportPath =}'
{=& jobPerformFn.importStatement =}
{=& jobDefinition.importStatement =}

{=! The `as any` is needed because JobFn expects `{ entities: Entities }` where
    Entities extends Partial<PrismaDelegate>, but module operation types use
    OperationContext<Entities> which wraps entities with PrismaDelegate<E[K]>.
    The shapes are structurally incompatible at the type level even though
    they match at runtime. =}
registerJob({
  job: {= jobDefinition.importIdentifier =},
  jobFn: {= jobPerformFn.importIdentifier =} as any,
})
