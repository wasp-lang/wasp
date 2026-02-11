{{={= =}=}}

import { SocialButton } from '../../../core/auth/forms/internal/social/SocialButton'
import * as SocialIcons from '../../../core/auth/forms/internal/social/SocialIcons'
import { config } from '../../../core/client/config'

// PUBLIC API
export const signInUrl: string = `${config.apiUrl}{= signInPath =}`

// PUBLIC API
export function SignInButton(): React.JSX.Element {
  return (
    <SocialButton href={signInUrl}>
      <SocialIcons.{= displayName =} />
    </SocialButton>
  )
}
