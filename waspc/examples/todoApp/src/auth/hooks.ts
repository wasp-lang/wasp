import type {
  OnAfterSignupHookFn,
  OnBeforeOAuthRedirectHookFn,
  OnBeforeSignupHookFn,
} from 'wasp/server/auth'

export const onBeforeSignup: OnBeforeSignupHookFn = async (args) => {
  const count = await args.prisma.user.count()
  console.log('before', count)
  console.log(args.providerId)
}

const oAuthUserState = new Map<string, string>()

export const onAfterSignup: OnAfterSignupHookFn = async (args) => {
  const count = await args.prisma.user.count()
  console.log('after', count)
  console.log('user', args.user)
  console.log('providerId', args.providerId)
  console.log('accessToken', args.accessToken)

  // Using OAuth state to keep track of user state
  const state = args.oAuthState.state
  if (state) {
    const userState = oAuthUserState.get(state)
    if (userState) {
      console.log('user state', JSON.parse(userState))
    }
    oAuthUserState.delete(state)
  }
}

export const onBeforeOAuthRedirect: OnBeforeOAuthRedirectHookFn = async (
  args
) => {
  console.log('req.query before redirect', args.req.query)
  args.url.searchParams.set('someState', '123')
  console.log('redirect to', args.url.toString())

  // Using OAuth state to keep track of user state
  const state = args.oAuthState.state
  if (state) {
    oAuthUserState.set(state, JSON.stringify(args.req.query))
  }

  return { url: args.url }
}
