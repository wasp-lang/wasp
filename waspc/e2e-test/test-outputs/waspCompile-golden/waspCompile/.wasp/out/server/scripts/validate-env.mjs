import { isValidAbsoluteURL } from './shared/validators.mjs';

console.info("🔍 Validating environment variables...");
if (process.env.WASP_WEB_CLIENT_URL && !isValidAbsoluteURL(process.env.WASP_WEB_CLIENT_URL)) {
  throw 'Environment variable WASP_WEB_CLIENT_URL is not a valid absolute URL';
}
