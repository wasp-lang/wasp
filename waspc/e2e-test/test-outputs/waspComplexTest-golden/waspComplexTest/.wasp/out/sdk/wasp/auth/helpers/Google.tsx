
import { config } from 'wasp/client'
import { SocialButton } from '../forms/internal/social/SocialButton'
import * as SocialIcons from '../forms/internal/social/SocialIcons'

// PUBLIC API
export const signInUrl: string = `${config.apiUrl}/auth/google/login`

// PUBLIC API
export function SignInButton(): React.JSX.Element {
  return (
    <SocialButton href={signInUrl}>
      <SocialIcons.Google />
    </SocialButton>
  )
}
