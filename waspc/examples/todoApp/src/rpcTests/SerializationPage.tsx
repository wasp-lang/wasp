import { expect } from 'chai'
import { useMemo } from 'react'
import { getSerializedObjects, useQuery } from 'wasp/client/operations'
import { SERIALIZABLE_OBJECTS_FIXTURE } from './fixtures'

export const SerializationPage = () => {
  const { data: serializedObjects } = useQuery(getSerializedObjects)

  const result = useMemo(() => {
    if (!serializedObjects) return

    try {
      expect(serializedObjects).to.deep.equal(SERIALIZABLE_OBJECTS_FIXTURE)
      return 'All serialized objects are of the expected types.'
    } catch (e) {
      console.error(e)
      return String(e)
    }
  }, [serializedObjects])

  return serializedObjects ? (
    <div>
      <label htmlFor="serializedObjects">Serialized objects</label>:
      <p id="serializedObjects">{result}</p>
    </div>
  ) : null
}
