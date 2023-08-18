import { useContext } from 'react'
import { useForm } from 'react-hook-form'

import { AuthContext } from '../../Auth'
import { Form, FormInput, FormItemGroup, FormLabel, FormError, SubmitButton } from '../Form'
import { AdditionalSignupFields } from '../../types'
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

export type LoginSignupFormFields = {
  [key: string]: string;
}

export const LoginSignupForm = ({
    state,
    socialButtonsDirection = 'horizontal',
    additionalSignupFields,
}: {
    state: 'login' | 'signup'
    socialButtonsDirection?: 'horizontal' | 'vertical'
    additionalSignupFields?: AdditionalSignupFields
}) => {
  const {
    isLoading,
    setErrorMessage,
    setSuccessMessage,
    setIsLoading,
  } = useContext(AuthContext)
  const isLogin = state === 'login'
  const cta = isLogin ? 'Log in' : 'Sign up';
  const hookForm = useForm<LoginSignupFormFields>()
  const { register, formState: { errors }, handleSubmit: hookFormHandleSubmit } = hookForm

  return (<>
        <SocialAuth>
          <SocialAuthLabel>{cta} with</SocialAuthLabel>
          <SocialAuthButtons gap='large' direction={socialButtonsDirection}>
              <SocialButton href={googleSignInUrl}><SocialIcons.Google/></SocialButton>

          </SocialAuthButtons>
        </SocialAuth>
  </>)
}
