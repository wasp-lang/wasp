import { stripTrailingSlash } from '../universal/url.js'
import { env } from './env.js'

const apiUrl = stripTrailingSlash(env.REACT_APP_API_URL)

// PUBLIC API
export type ClientConfig = {
  apiUrl: string,
} 

// PUBLIC API
export const config: ClientConfig = {
  apiUrl,
}
