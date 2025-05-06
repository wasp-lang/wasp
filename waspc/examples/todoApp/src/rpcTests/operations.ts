import { GetSerializedObjects } from 'wasp/server/operations'
import { SERIALIZABLE_OBJECTS_FIXTURE } from './fixtures'

export const getSerializedObjects: GetSerializedObjects<
  void,
  typeof SERIALIZABLE_OBJECTS_FIXTURE
> = async () => SERIALIZABLE_OBJECTS_FIXTURE
