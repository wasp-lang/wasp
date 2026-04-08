import {
  type GetPasswordResetEmailContentFn,
  type GetVerificationEmailContentFn,
  defineUserSignupFields,
} from "wasp/server/auth";

export const getPasswordResetEmailContent: GetPasswordResetEmailContentFn = ({
  passwordResetLink,
}) => ({
  subject: "Password reset",
  text: `Click the link below to reset your password: ${passwordResetLink}`,
  html: `
        <p>Click the link below to reset your password</p>
        <a href="${passwordResetLink}">Reset password</a>
    `,
});

export const getVerificationEmailContent: GetVerificationEmailContentFn = ({
  verificationLink,
}) => ({
  subject: "Verify your email",
  text: `Click the link below to verify your email: ${verificationLink}`,
  html: `
        <p>Click the link below to verify your email</p>
        <a href="${verificationLink}">Verify email</a>
    `,
});

export const userSignupFields = defineUserSignupFields({
  address: (data) => {
    if (typeof data.address !== "string") {
      throw new Error("Address is required.");
    }
    if (data.address.length < 10) {
      throw new Error("Address must be at least 10 characters long.");
    }
    return data.address;
  },
});
