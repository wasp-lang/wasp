import { AuthServiceError } from "../authService/errors";

const unableToLoginMessage = "Unable to login with the OAuth provider.";

export function throwMissingOAuthCode(): never {
  throw new AuthServiceError(
    "missing-token",
    `${unableToLoginMessage} The code is missing.`,
    { responseMessage: `${unableToLoginMessage} The code is missing.` },
  );
}

export function throwUsedOAuthCode(): never {
  throw new AuthServiceError(
    "used-token",
    `${unableToLoginMessage} The code has already been used.`,
    {
      responseMessage: `${unableToLoginMessage} The code has already been used.`,
    },
  );
}

export function throwInvalidOAuthCode(): never {
  throw new AuthServiceError(
    "invalid-token",
    `${unableToLoginMessage} The code is invalid.`,
    { responseMessage: `${unableToLoginMessage} The code is invalid.` },
  );
}
