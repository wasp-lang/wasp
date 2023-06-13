import { useContext, type FormEvent } from 'react'
import { styled } from '../../../../stitches.config'
import config from '../../../../config.js'

import { AuthContext } from '../../Auth'
import { Form, FormInput, FormItemGroup, FormLabel, SubmitButton } from '../Form'
import * as SocialIcons from '../social/SocialIcons'
import { SocialButton } from '../social/SocialButton'

const SocialAuth = styled('div', {
    marginTop: '1.5rem'
})

const SocialAuthLabel = styled('div', {
    fontWeight: '500',
    fontSize: '$sm'
})

const SocialAuthButtons = styled('div', {
    marginTop: '0.5rem',
    display: 'flex',

    variants: {
        direction: {
            horizontal: {
                display: 'grid',
                gridTemplateColumns: 'repeat(auto-fit, minmax(48px, 1fr))',
            },
            vertical: {
                flexDirection: 'column',
                margin: '8px 0',
            }
        },
        gap: {
            small: {
                gap: '4px',
            },
            medium: {
                gap: '8px',
            },
            large: {
                gap: '16px',
            }
        }
    }
})
const googleSignInUrl = `${config.apiUrl}/auth/google/login`

export const LoginSignupForm = ({
    state,
    socialButtonsDirection = 'horizontal',
}: {
    state: 'login' | 'signup',
    socialButtonsDirection?: 'horizontal' | 'vertical';
}) => {
  const {
    isLoading,
    setErrorMessage,
    setSuccessMessage,
    setIsLoading,
  } = useContext(AuthContext)
  const cta = state === 'login' ? 'Log in' : 'Sign up';

  return (<>
        <SocialAuth>
          <SocialAuthLabel>{cta} with</SocialAuthLabel>
          <SocialAuthButtons gap='large' direction={socialButtonsDirection}>
              <SocialButton href={googleSignInUrl}><SocialIcons.Google/></SocialButton>

          </SocialAuthButtons>
        </SocialAuth>
  </>)
}
