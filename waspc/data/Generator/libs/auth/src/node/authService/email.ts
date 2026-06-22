export type EmailVerificationService = {
  createVerificationLink(email: string): Promise<string>;
  sendVerificationEmail(args: {
    email: string;
    verificationLink: string;
  }): Promise<void>;
};

export type PasswordResetService = {
  createPasswordResetLink(email: string): Promise<string>;
  sendPasswordResetEmail(args: {
    email: string;
    passwordResetLink: string;
  }): Promise<void>;
};

export type EmailTokenService = {
  verifyEmailToken(token: string): Promise<{ email: string }>;
};
