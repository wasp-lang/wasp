import { stripTrailingSlash } from "./universal/url";

const apiUrl = stripTrailingSlash(process.env.REACT_APP_API_URL) || 'http://localhost:3001';

const config = {
  apiUrl,
}

export default config
