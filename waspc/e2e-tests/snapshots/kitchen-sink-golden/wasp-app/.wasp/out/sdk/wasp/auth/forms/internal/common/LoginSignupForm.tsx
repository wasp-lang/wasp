import { useContext } from 'react'
import { useForm, UseFormReturn } from 'react-hook-form'
import styles from './LoginSignupForm.module.css'
import '../auth-styles.css'
import { config } from 'wasp/client'
import { clsx } from '../util'

import { AuthContext } from '../../Auth'
import {
  Form,
  FormInput,
  FormItemGroup,
  FormLabel,
  FormError,
  FormTextarea,
  SubmitButton,
} from '../Form'
import type {
  AdditionalSignupFields,
  AdditionalSignupField,
  AdditionalSignupFieldRenderFn,
  FormState,
} from '../../types'
import * as SocialIcons from '../social/SocialIcons'
import { SocialButton } from '../social/SocialButton'
import { useNavigate } from 'react-router-dom'
import { useEmail } from '../email/useEmail'

const slackSignInUrl = `${config.apiUrl}/auth/slack/login`
const discordSignInUrl = `${config.apiUrl}/auth/discord/login`
const googleSignInUrl = `${config.apiUrl}/auth/google/login`
const gitHubSignInUrl = `${config.apiUrl}/auth/github/login`

// PRIVATE API
export type LoginSignupFormFields = {
  [key: string]: string;
}

// PRIVATE API
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
  const navigate = useNavigate();
  const onErrorHandler = (error) => {
    setErrorMessage({ title: error.message, description: error.data?.data?.message })
  };
  const hookForm = useForm<LoginSignupFormFields>()
  const { register, formState: { errors }, handleSubmit: hookFormHandleSubmit } = hookForm
  const { handleSubmit } = useEmail({
    isLogin,
    onError: onErrorHandler,
    showEmailVerificationPending() {
      hookForm.reset()
      setSuccessMessage(`You've signed up successfully! Check your email for the confirmation link.`)
    },
    onLoginSuccess() {
      navigate('/')
    },
  });
  async function onSubmit (data) {
    setIsLoading(true);
    setErrorMessage(null);
    setSuccessMessage(null);
    try {
      await handleSubmit(data);
    } finally {
      setIsLoading(false);
    }
  }

  return (<>
        <div className={styles.socialAuth}>
          <div className={styles.socialAuthLabel}>{cta} with</div>
            <div className={clsx(styles.socialAuthButtons, styles[socialButtonsDirection])}>
              <SocialButton href={slackSignInUrl}><SocialIcons.Slack/></SocialButton>

              <SocialButton href={discordSignInUrl}><SocialIcons.Discord/></SocialButton>

              <SocialButton href={googleSignInUrl}><SocialIcons.Google/></SocialButton>


              <SocialButton href={gitHubSignInUrl}><SocialIcons.GitHub/></SocialButton>
          </div>
        </div>
        <div className={styles.orContinueWith}>
          <div className={styles.orContinueWithLineContainer}>
            <div className={styles.orContinueWithLine}/>
          </div>
          <div className={styles.orContinueWithTextContainer}>
            <span className={styles.orContinueWithText}>Or continue with</span>
          </div>
        </div>
        <Form onSubmit={hookFormHandleSubmit(onSubmit)}>
          <FormItemGroup>
            <FormLabel>E-mail</FormLabel>
            <FormInput
              {...register('email', {
                required: 'Email is required',
              })}
              type="email"
              disabled={isLoading}
            />
            {errors.email && <FormError>{errors.email.message}</FormError>}
          </FormItemGroup>
          <FormItemGroup>
            <FormLabel>Password</FormLabel>
            <FormInput
              {...register('password', {
                required: 'Password is required',
              })}
              type="password"
              disabled={isLoading}
            />
            {errors.password && <FormError>{errors.password.message}</FormError>}
          </FormItemGroup>
          <AdditionalFormFields
            hookForm={hookForm}
            formState={{ isLoading }}
            additionalSignupFields={additionalSignupFields}
          />
          <FormItemGroup>
            <SubmitButton type="submit" disabled={isLoading}>{cta}</SubmitButton>
          </FormItemGroup>
        </Form>
  </>)
}

function AdditionalFormFields({
  hookForm,
  formState: { isLoading },
  additionalSignupFields,
}: {
  hookForm: UseFormReturn<LoginSignupFormFields>;
  formState: FormState;
  additionalSignupFields?: AdditionalSignupFields;
}) {
  const {
    register,
    formState: { errors },
  } = hookForm;

  function renderField<ComponentType extends React.JSXElementConstructor<any>>(
    field: AdditionalSignupField,
    // Ideally we would use ComponentType here, but it doesn't work with react-hook-form
    Component: any,
    props?: React.ComponentProps<ComponentType>
  ) {
    const errorMessage = errors[field.name]?.message;
    return (
      <FormItemGroup key={field.name}>
        <FormLabel>{field.label}</FormLabel>
        <Component
          {...register(field.name, field.validations)}
          {...props}
          disabled={isLoading}
        />
        {errorMessage && (
          <FormError>{errorMessage}</FormError>
        )}
      </FormItemGroup>
    );
  }

  if (areAdditionalFieldsRenderFn(additionalSignupFields)) {
    return additionalSignupFields(hookForm, { isLoading })
  }

  return (
    additionalSignupFields &&
    additionalSignupFields.map((field) => {
      if (isFieldRenderFn(field)) {
        return field(hookForm, { isLoading })
      }
      switch (field.type) {
        case 'input':
          return renderField<typeof FormInput>(field, FormInput, {
            type: 'text',
          })
        case 'textarea':
          return renderField<typeof FormTextarea>(field, FormTextarea)
        default:
          throw new Error(
            `Unsupported additional signup field type: ${field.type}`
          )
      }
    })
  )
}

function isFieldRenderFn(
  additionalSignupField: AdditionalSignupField | AdditionalSignupFieldRenderFn
): additionalSignupField is AdditionalSignupFieldRenderFn {
  return typeof additionalSignupField === 'function'
}

function areAdditionalFieldsRenderFn(
  additionalSignupFields?: AdditionalSignupFields
): additionalSignupFields is AdditionalSignupFieldRenderFn {
  return typeof additionalSignupFields === 'function'
}
