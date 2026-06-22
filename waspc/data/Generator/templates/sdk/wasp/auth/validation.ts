import { HttpError } from '../server/index.js';
import {
  ensurePasswordIsPresent as ensureLibPasswordIsPresent,
  ensureTokenIsPresent as ensureLibTokenIsPresent,
  ensureValidEmail as ensureLibValidEmail,
  ensureValidPassword as ensureLibValidPassword,
  ensureValidUsername as ensureLibValidUsername,
  PASSWORD_FIELD,
  ValidationError,
} from '@wasp.sh/lib-auth';

export { PASSWORD_FIELD };

// PUBLIC API
export function ensureValidEmail(args: object): void {
  throwWaspValidationError(() => ensureLibValidEmail(args));
}

// PUBLIC API
export function ensureValidUsername(args: object): void {
  throwWaspValidationError(() => ensureLibValidUsername(args));
}

// PUBLIC API
export function ensurePasswordIsPresent(args: object): void {
  throwWaspValidationError(() => ensureLibPasswordIsPresent(args));
}

// PUBLIC API
export function ensureValidPassword(args: object): void {
  throwWaspValidationError(() => ensureLibValidPassword(args));
}

// PUBLIC API
export function ensureTokenIsPresent(args: object): void {
  throwWaspValidationError(() => ensureLibTokenIsPresent(args));
}

// PRIVATE API
export function throwValidationError(message: string): void {
  throw new HttpError(422, 'Validation failed', { message })
}

function throwWaspValidationError(validate: () => void): void {
  try {
    validate();
  } catch (e) {
    if (e instanceof ValidationError) {
      throwValidationError(e.message);
    }
    throw e;
  }
}
