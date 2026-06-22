import { AuthServiceError } from "../authService/errors";

export function throwInvalidCredentials(): never {
  throw new AuthServiceError("invalid-credentials", "Invalid credentials");
}

export function throwEmailResendTooSoon(timeLeft: number): never {
  throw new AuthServiceError(
    "email-resend-too-soon",
    `Please wait ${timeLeft} secs before trying again.`,
    { timeLeft },
  );
}

export function throwInvalidToken(message: string): never {
  throw new AuthServiceError("invalid-token", message, {
    responseMessage: message,
  });
}

export function throwEmailDeliveryFailed(
  cause: unknown,
  {
    logMessage = "Failed to send email verification email:",
    responseMessage = "Failed to send email verification email.",
  }: {
    logMessage?: string;
    responseMessage?: string;
  } = {},
): never {
  throw new AuthServiceError("email-delivery-failed", responseMessage, {
    cause,
    logMessage,
    responseMessage,
  });
}
