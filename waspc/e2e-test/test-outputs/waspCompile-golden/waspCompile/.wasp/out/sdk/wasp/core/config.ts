import { stripTrailingSlash } from 'wasp/universal/url'

const apiUrl = stripTrailingSlash(import.meta.env.REACT_APP_API_URL) || 'http://localhost:3001';

const config = {
  apiUrl,
}

export default config
