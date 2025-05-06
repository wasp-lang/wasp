import type { getSerializedObjects } from 'wasp/client/operations'
import { QueryMetadata } from 'wasp/client/operations/rpc'
import { Equal, Expect } from '../testTypes/helpers'
import type { SERIALIZABLE_OBJECTS_FIXTURE } from './fixtures'

type TestCases = [
  Expect<
    Equal<
      typeof getSerializedObjects,
      QueryMetadata & (() => Promise<typeof SERIALIZABLE_OBJECTS_FIXTURE>)
    >
  >
]
