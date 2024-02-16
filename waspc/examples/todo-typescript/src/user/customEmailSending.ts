// import {
//   createPasswordResetLink,
//   createEmailVerificationLink,
//   sendEmailVerificationEmail,
//   sendPasswordResetEmail,
// } from 'wasp/server/auth'

// export async function send() {
//   const userEmail = 'mihovil@ilakovac.com'

//   const link = await createPasswordResetLink(
//     userEmail,
//     '/password-reset'
//   )
//   const secondLink = await createEmailVerificationLink(
//     userEmail,
//     '/email-verify'
//   )

//   // Send email verification email.
//   await sendEmailVerificationEmail(userEmail, {
//     from: {
//       name: 'Wasp',
//       email: userEmail,
//     },
//     to: userEmail,
//     subject: 'Email verification',
//     text: 'Click on the link to verify your email. ' + secondLink,
//     html: `<a href="${secondLink}">Click here to verify your email.</a>`,
//   })

//   // Send password reset email.
//   await sendPasswordResetEmail(userEmail, {
//     from: {
//       name: 'Wasp',
//       email: userEmail,
//     },
//     to: userEmail,
//     subject: 'Password reset',
//     text: 'Click on the link to reset your password.' + link,
//     html: `<a href="${link}">Click here to reset your password.</a>`,
//   })
// }
