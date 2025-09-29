import type {
  OnAfterSignupHook,
  OnBeforeOAuthRedirectHook,
  OnBeforeSignupHook,
} from 'wasp/server/auth'

export const onBeforeSignup: OnBeforeSignupHook = async (args) => {
  const count = await args.prisma.user.count()
  console.log('before', count)
  console.log(args.providerId)
}

export const onAfterSignup: OnAfterSignupHook = async (args) => {
  const count = await args.prisma.user.count()
  console.log('after', count)
  console.log('user', args.user)
}

export const onBeforeOAuthRedirect: OnBeforeOAuthRedirectHook = async (
  args,
) => {
  console.log('redirect to', args.url.toString())
  return { url: args.url }
}

