{{={= =}=}}

import { SocialButton } from '../forms/internal/social/SocialButton'
import * as SocialIcons from '../forms/internal/social/SocialIcons'
import { config } from '../../client/config'

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
