export type AuthServiceErrorCode =
  | "invalid-credentials"
  | "invalid-token"
  | "used-token"
  | "missing-token"
  | "email-resend-too-soon"
  | "email-not-verified"
  | "email-delivery-failed"
  | "identity-already-exists"
  | "validation-failed"
  | "oauth-flow-failed"
  | "save-failed";

export class AuthServiceError extends Error {
  constructor(
    public readonly code: AuthServiceErrorCode,
    message: string,
    public readonly metadata: Record<string, unknown> = {},
  ) {
    super(message);
    this.name = "AuthServiceError";
  }
}
