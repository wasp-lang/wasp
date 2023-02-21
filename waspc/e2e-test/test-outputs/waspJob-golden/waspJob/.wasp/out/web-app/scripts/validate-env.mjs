import { throwIfNotValidAbsoluteURL } from './universal/validators.mjs';

console.info("🔍 Validating environment variables...");
throwIfNotValidAbsoluteURL(process.env.REACT_APP_API_URL, 'Environemnt variable REACT_APP_API_URL');
