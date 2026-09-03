/**
 * This module augments SDK's `Register` type with the user project types.
 */

// The import ensures the module is always loaded into the bundle.
// Otherwise, module augmentation can fail if it wasn't loaded.
import "wasp/types"

declare module "wasp/types" {
  interface Register {
    prismaSetupFn: typeof import('../../../../../src/features/db/prisma').setUpPrisma
    serverEnvValidationSchema: typeof import('../../../../../src/env').serverEnvValidationSchema
    clientEnvValidationSchema: typeof import('../../../../../src/env').clientEnvValidationSchema
    webSocketFn: typeof import('../../../../../src/features/chat/webSocket').chatWebSocket
    operations: {
      'getTasks': typeof import('../../../../../src/features/operations/queries').getTasks
      'getNumTasks': typeof import('../../../../../src/features/operations/queries').getNumTasks
      'getTask': typeof import('../../../../../src/features/operations/queries').getTask
      'getOldestTask': typeof import('../../../../../src/features/operations/getOldestTask').default
      'getSerializedObjects': typeof import('../../../../../src/features/operations/queries').getSerializedObjects
      'getTextUppercaseRequests': typeof import('../../../../../src/features/jobs/uppercaseText').getTextUppercaseRequests
      'getDate': typeof import('../../../../../src/rpcTests/operations/definitions').getDate
      'getAnythingNoAuth': typeof import('../../../../../src/rpcTests/operations/definitions').getAnythingNoAuth
      'getAnythingAuth': typeof import('../../../../../src/rpcTests/operations/definitions').getAnythingAuth
      'getTrueVoid': typeof import('../../../../../src/rpcTests/operations/definitions').getTrueVoid
      'getAnyNoAuth': typeof import('../../../../../src/rpcTests/operations/definitions').getAnyNoAuth
      'getAnyAuth': typeof import('../../../../../src/rpcTests/operations/definitions').getAnyAuth
      'getAnyToNumberSpecified': typeof import('../../../../../src/rpcTests/operations/definitions').getAnyToNumberSpecified
      'customSignup': typeof import('../../../../../src/features/auth/customSignup').customSignup
      'createTask': typeof import('../../../../../src/features/operations/actions').createTask
      'updateTaskIsDone': typeof import('../../../../../src/features/operations/actions').updateTaskIsDone
      'deleteCompletedTasks': typeof import('../../../../../src/features/operations/actions').deleteCompletedTasks
      'toggleAllTasks': typeof import('../../../../../src/features/operations/actions').toggleAllTasks
      'requestUppercaseText': typeof import('../../../../../src/features/jobs/uppercaseText').requestUppercaseText
      'testingAction': typeof import('../../../../../src/rpcTests/operations/server').testingAction
      'taskToTaskUnspecified': typeof import('../../../../../src/rpcTests/operations/definitions').taskToTaskUnspecified
      'taskToTaskSatisfies': typeof import('../../../../../src/rpcTests/operations/definitions').taskToTaskSatisfies
      'taskToTaskSpecified': typeof import('../../../../../src/rpcTests/operations/definitions').taskToTaskSpecified
      'voidToStringAuth': typeof import('../../../../../src/rpcTests/operations/definitions').voidToStringAuth
      'voidToStringNoAuth': typeof import('../../../../../src/rpcTests/operations/definitions').voidToStringNoAuth
      'unspecifiedToNumber': typeof import('../../../../../src/rpcTests/operations/definitions').unspecifiedToNumber
      'boolToStringAuth': typeof import('../../../../../src/rpcTests/operations/definitions').boolToStringAuth
      'boolToStringNoAuth': typeof import('../../../../../src/rpcTests/operations/definitions').boolToStringNoAuth
      'boolToVoidNoAuth': typeof import('../../../../../src/rpcTests/operations/definitions').boolToVoidNoAuth
      'boolToVoidAuth': typeof import('../../../../../src/rpcTests/operations/definitions').boolToVoidAuth
      'jsActionWithArgs': typeof import('../../../../../src/rpcTests/operations/jsDefinitions').jsActionWithArgs
    }
    crudOverrides: {
      'tasks': {
        GetAll: typeof import('../../../../../src/features/crud/crud').crudGetAllTasks
        Create: typeof import('../../../../../src/features/crud/crud').crudCreateTask
      }
      'taskVotes': {
      }
    }
  }
}
