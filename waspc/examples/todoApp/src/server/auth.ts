import {
  GetVerificationEmailContentFn,
  GetPasswordResetEmailContentFn,
} from '@wasp/types'

export const getVerificationEmailContent: GetVerificationEmailContentFn = ({
  verificationLink,
}) => {
  const subject = 'Please verify your email'
  const html = `<a href="${verificationLink}">Verify your email address</a>`
  const text = `Copy and paste the following link in your browser to verify your email address: ${verificationLink}`
  return { subject, html, text }
}

export const getPasswordResetEmailContent: GetPasswordResetEmailContentFn = ({
  passwordResetLink,
}) => {
  const subject = 'Reset your password'
  const html = `<a href="${passwordResetLink}">Reset your password</a>`
  const text = `Copy and paste the following link in your browser to reset your password: ${passwordResetLink}`
  return { subject, html, text }
}
