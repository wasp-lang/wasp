
import config from '../../config.js'
import { SocialButton } from '../forms/internal/social/SocialButton'
import * as SocialIcons from '../forms/internal/social/SocialIcons'

export const signInUrl = `${config.apiUrl}/auth/google/login`

export function SignInButton() {
  return (
    <SocialButton href={signInUrl}>
      <SocialIcons.Google />
    </SocialButton>
  )
}
