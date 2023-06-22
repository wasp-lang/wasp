
import config from '../../config.js'
import { SocialButton } from '../forms/SocialButton'
import * as SocialIcons from '../forms/SocialIcons'

export const signInUrl = `${config.apiUrl}/auth/google/login`

export function SignInButton() {
  return (
    <SocialButton href={signInUrl}>
      <SocialIcons.Google />
    </SocialButton>
  )
}
