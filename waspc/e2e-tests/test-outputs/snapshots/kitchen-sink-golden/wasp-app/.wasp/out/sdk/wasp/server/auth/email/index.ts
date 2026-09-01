// PUBLIC API
/**
 * The email helpers of Wasp's own auth, bound to this app's auth runtime when
 * the `@wasp.sh/auth` lib was instantiated (see `wasp/server/auth/provider`).
 */
export {
    createEmailVerificationLink,
    sendEmailVerificationEmail,
    createPasswordResetLink,
    sendPasswordResetEmail,
    isEmailResendAllowed,
    type GetVerificationEmailContentFn,
    type GetPasswordResetEmailContentFn,
} from '@wasp.sh/auth/server'
export { ensureValidEmail } from '../../../auth/validation.js'
