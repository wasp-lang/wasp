import {
  createPasswordResetLink,
  createEmailVerificationLink,
  sendEmailVerificationEmail,
  sendPasswordResetEmail,
} from 'wasp/server/auth/email/utils'

export async function send() {
  const link = await createPasswordResetLink(
    'mihovil@ilakovac.com',
    '/password-reset'
  )
  const secondLink = await createEmailVerificationLink(
    'mihovil@ilakovac.com',
    '/email-verify'
  )

  // Send email verification email.
  await sendEmailVerificationEmail('mihovil@ilakovac.com', {
    from: {
      name: 'Wasp',
      email: 'mihovil@ilakovac.com',
    },
    to: 'mihovil@ilakovac.com',
    subject: 'Email verification',
    text: 'Click on the link to verify your email. ' + secondLink,
    html: `<a href="${secondLink}">Click here to verify your email.</a>`,
  })

  // Send password reset email.
  await sendPasswordResetEmail('mihovil@ilakovac.com', {
    from: {
      name: 'Wasp',
      email: 'mihovil@ilakovac.com',
    },
    to: 'mihovil@ilakovac.com',
    subject: 'Password reset',
    text: 'Click on the link to reset your password.' + link,
    html: `<a href="${link}">Click here to reset your password.</a>`,
  })
}
