import { findOrCreateUserEntity } from '../../core/auth.js' // TODO: properly expose this.

async function onSignInFn(_method, _context, args) {
    // TODO: Generate a secure, random password via a helper.
    const user = await findOrCreateUserEntity(args.profile.email, "password123!")
    return user
}

export default onSignInFn
