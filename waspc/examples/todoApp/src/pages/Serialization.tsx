import { Prisma } from '@prisma/client'
import { getSerializedObjects, useQuery } from 'wasp/client/operations'

export const SerializationPage = () => {
  const { data: serializedObjects } = useQuery(getSerializedObjects)
  console.log({ serializedObjects })

  return serializedObjects ? (
    <div>
      <label htmlFor="serializedObjects">Serialized objects</label>:
      <pre id="serializedObjects">
        {Object.entries(serializedObjects || {}).every(
          ([key, value]): boolean => {
            switch (key as keyof NonNullable<typeof serializedObjects>) {
              case 'date':
                return value instanceof Date
              case 'set':
                return value instanceof Set
              case 'map':
                return value instanceof Map
              case 'decimal':
                return value instanceof Prisma.Decimal
              case 'regexp':
                return value instanceof RegExp
              default:
                return false
            }
          }
        )
          ? 'All serialized objects are of the expected types.'
          : 'Some serialized objects are not of the expected types.'}
      </pre>
    </div>
  ) : null
}
