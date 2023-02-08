// Template is located at from shared/validators.js
import { isValidAbsoluteURL } from './validators.mjs';

if (process.env.REACT_APP_API_URL && !isValidAbsoluteURL(process.env.REACT_APP_API_URL)) {
    throw 'Environemnt variable REACT_APP_API_URL is not a valid absolute URL';
}

