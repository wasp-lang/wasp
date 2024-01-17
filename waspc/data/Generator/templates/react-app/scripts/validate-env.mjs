import { throwIfNotValidAbsoluteURL } from 'wasp/universal/validators';

console.info("🔍 Validating environment variables...");
throwIfNotValidAbsoluteURL(process.env.REACT_APP_API_URL, 'Environemnt variable REACT_APP_API_URL');
