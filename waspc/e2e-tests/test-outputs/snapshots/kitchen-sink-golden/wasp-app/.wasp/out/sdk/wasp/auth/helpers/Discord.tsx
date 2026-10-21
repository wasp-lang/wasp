
import { config } from '../../client/index.js'
import { SocialButton } from '@wasp.sh/lib-auth/browser'
import * as SocialIcons from '@wasp.sh/lib-auth/browser'

// PUBLIC API
export const signInUrl: string = `${config.apiUrl}/auth/discord/login`

// PUBLIC API
export function SignInButton(): React.JSX.Element {
  return (
    <SocialButton href={signInUrl}>
      <SocialIcons.Discord />
    </SocialButton>
  )
}
