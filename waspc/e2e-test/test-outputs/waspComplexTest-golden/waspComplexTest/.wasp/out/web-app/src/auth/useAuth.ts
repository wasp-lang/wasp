import { deserialize as superjsonDeserialize } from 'superjson'
import { useQuery } from '../queries'
import api, { handleApiError } from '../api'
import { HttpMethod } from '../types'
// todo(filip): turn into a proper import
import { type SanitizedUser as User } from '../../../server/src/_types/' 
import { addMetadataToQuery } from '../queries/core'

export default function useAuth(queryFnArgs?: unknown, config?: any) {
  return useQuery(getMe, queryFnArgs, config)
}

const getMePath = '/auth/me'
export async function getMe(): Promise<User | null> {
  try {
    const response = await api.get(getMePath)

    return superjsonDeserialize(response.data)
  } catch (error) {
    if (error.response?.status === 401) {
      return null
    } else {
      handleApiError(error)
    }
  }
}

addMetadataToQuery(getMe, {
  relativeQueryPath: getMePath,
  queryRoute: { method: HttpMethod.Get, path: getMePath },
  entitiesUsed: ['User'],
})
