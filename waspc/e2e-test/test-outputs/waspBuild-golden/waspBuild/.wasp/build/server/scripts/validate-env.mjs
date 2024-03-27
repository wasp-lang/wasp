import { throwIfNotValidAbsoluteURL } from 'wasp/universal/validators';

console.info("🔍 Validating environment variables...");
throwIfNotValidAbsoluteURL(process.env.WASP_WEB_CLIENT_URL, 'Environment variable WASP_WEB_CLIENT_URL');
throwIfNotValidAbsoluteURL(process.env.WASP_SERVER_URL, 'Environment variable WASP_SERVER_URL');
