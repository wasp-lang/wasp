import { deserialize as superjsonDeserialize } from 'superjson'
import { useQuery, buildAndRegisterQuery } from 'wasp/client/operations'
import type { QueryFunction, Query  } from 'wasp/client/operations/rpc'
import { api, handleApiError } from 'wasp/client/api'
import { HttpMethod } from 'wasp/client'
import type { AuthUser, AuthUserData } from '../server/auth/user.js'
import { makeAuthUserIfPossible } from '../auth/user.js'
import { UseQueryResult } from '@tanstack/react-query'

// PUBLIC API
export const getMe: Query<void, AuthUser | null> = createUserGetter()

// PUBLIC API
export default function useAuth(): UseQueryResult<AuthUser | null> {
  return useQuery(getMe)
}

function createUserGetter(): Query<void, AuthUser | null> {
  const getMeRelativePath = 'auth/me'
  const getMeRoute = { method: HttpMethod.Get, path: `/${getMeRelativePath}` }
  const getMe: QueryFunction<void, AuthUser | null> = async () =>  {
    try {
      const response = await api.get(getMeRoute.path)
      const userData = superjsonDeserialize<AuthUserData | null>(response.data)
      return makeAuthUserIfPossible(userData)
    } catch (error) {
      if (error.response?.status === 401) {
        return null
      } else {
        throw handleApiError(error)
      }
    }
  }
  
  return buildAndRegisterQuery(getMe, {
    queryCacheKey: [getMeRelativePath],
    queryRoute: getMeRoute,
    entitiesUsed: ['User'],
  })
}
