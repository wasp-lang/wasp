
import { browserAppDelivery } from '../../client/index.js'
import { SocialButton } from '../forms/internal/social/SocialButton'
import * as SocialIcons from '../forms/internal/social/SocialIcons'

// PUBLIC API
export const signInUrl: string = browserAppDelivery.waspApiUrl('/auth/microsoft/login')

// PUBLIC API
export function SignInButton(): React.JSX.Element {
  return (
    <SocialButton href={signInUrl}>
      <SocialIcons.Microsoft />
    </SocialButton>
  )
}
