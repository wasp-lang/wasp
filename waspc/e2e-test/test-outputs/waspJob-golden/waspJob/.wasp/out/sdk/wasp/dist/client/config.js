import { stripTrailingSlash } from '../universal/url.js';
import { env } from './env.js';
const apiUrl = stripTrailingSlash(env.REACT_APP_API_URL);
// PUBLIC API
export const config = {
    apiUrl,
};
//# sourceMappingURL=config.js.map