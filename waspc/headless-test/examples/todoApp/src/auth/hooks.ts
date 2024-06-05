import { HttpError } from 'wasp/server'
import type {
  OnAfterSignupHookFn,
  OnBeforeSignupHookFn,
} from 'wasp/server/auth'

export const onBeforeSignup: OnBeforeSignupHookFn = async (args) => {
  if (args.providerId.providerUserId === 'notallowed@email.com') {
    throw new HttpError(403, 'On Before Signup Hook disallows this email.')
  }
}

export const onAfterSignup: OnAfterSignupHookFn = async (args) => {
  await args.prisma.user.update({
    where: { id: args.user.id },
    data: {
      isOnAfterSignupHookCalled: true,
    },
  })
}
