{{={= =}=}}

import config from 'wasp/core/config'
import { SocialButton } from '../forms/internal/social/SocialButton'
import * as SocialIcons from '../forms/internal/social/SocialIcons'

export const signInUrl = `${config.apiUrl}{= signInPath =}`

export function SignInButton() {
  return (
    <SocialButton href={signInUrl}>
      <SocialIcons.{= displayName =} />
    </SocialButton>
  )
}
