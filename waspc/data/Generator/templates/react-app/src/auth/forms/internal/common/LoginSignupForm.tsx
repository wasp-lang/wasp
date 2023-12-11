{{={= =}=}}
import { useContext } from 'react'
import { useForm, UseFormReturn } from 'react-hook-form'
import { styled } from '../../../../stitches.config'
import config from '../../../../config'

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
{=# isSocialAuthEnabled =}
import * as SocialIcons from '../social/SocialIcons'
import { SocialButton } from '../social/SocialButton'
{=/ isSocialAuthEnabled =}
{=# isAnyPasswordBasedAuthEnabled =}
import { useHistory } from 'react-router-dom'
{=/ isAnyPasswordBasedAuthEnabled =}
{=# isUsernameAndPasswordAuthEnabled =}
import { useUsernameAndPassword } from '../usernameAndPassword/useUsernameAndPassword'
{=/ isUsernameAndPasswordAuthEnabled =}
{=# isEmailAuthEnabled =}
import { useEmail } from '../email/useEmail'
{=/ isEmailAuthEnabled =}

{=# areBothSocialAndPasswordBasedAuthEnabled =}
const OrContinueWith = styled('div', {
    position: 'relative',
    marginTop: '1.5rem'
})
  
const OrContinueWithLineContainer = styled('div', {
    position: 'absolute',
    inset: '0px',
    display: 'flex',
    alignItems: 'center'
})

const OrContinueWithLine = styled('div', {
    width: '100%',
    borderTopWidth: '1px',
    borderColor: '$gray500'
})

const OrContinueWithTextContainer = styled('div', {
    position: 'relative',
    display: 'flex',
    justifyContent: 'center',
    fontSize: '$sm'
})

const OrContinueWithText = styled('span', {
    backgroundColor: 'white',
    paddingLeft: '0.5rem',
    paddingRight: '0.5rem'
})
{=/ areBothSocialAndPasswordBasedAuthEnabled =}
{=# isSocialAuthEnabled =}
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
{=/ isSocialAuthEnabled =}
{=# isGoogleAuthEnabled =}
const googleSignInUrl = `${config.apiUrl}{= googleSignInPath =}`
{=/ isGoogleAuthEnabled =}
{=# isGitHubAuthEnabled =}
const gitHubSignInUrl = `${config.apiUrl}{= gitHubSignInPath =}`
{=/ isGitHubAuthEnabled =}

{=!
// Since we allow users to add additional fields to the signup form, we don't
// know the exact shape of the form values. We are assuming that the form values
// will be a flat object with string values.
=}
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
  {=# isAnyPasswordBasedAuthEnabled =}
  const history = useHistory();
  const onErrorHandler = (error) => {
    setErrorMessage({ title: error.message, description: error.data?.data?.message })
  };
  {=/ isAnyPasswordBasedAuthEnabled =}
  const hookForm = useForm<LoginSignupFormFields>()
  const { register, formState: { errors }, handleSubmit: hookFormHandleSubmit } = hookForm
  {=# isUsernameAndPasswordAuthEnabled =}
  const { handleSubmit } = useUsernameAndPassword({
    isLogin,
    onError: onErrorHandler,
    onSuccess() {
      history.push('{= onAuthSucceededRedirectTo =}')
    },
  });
  {=/ isUsernameAndPasswordAuthEnabled =}
  {=# isEmailAuthEnabled =}
  const { handleSubmit } = useEmail({
    isLogin,
    onError: onErrorHandler,
    showEmailVerificationPending() {
      hookForm.reset()
      setSuccessMessage(`You've signed up successfully! Check your email for the confirmation link.`)
    },
    onLoginSuccess() {
      history.push('{= onAuthSucceededRedirectTo =}')
    },
    {=# isEmailVerificationRequired =}
    isEmailVerificationRequired: true,
    {=/ isEmailVerificationRequired =}
    {=^ isEmailVerificationRequired =}
    isEmailVerificationRequired: false,
    {=/ isEmailVerificationRequired =}
  });
  {=/ isEmailAuthEnabled =}
  {=# isAnyPasswordBasedAuthEnabled =}
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
  {=/ isAnyPasswordBasedAuthEnabled =}

  return (<>
      {=# isSocialAuthEnabled =}
        <SocialAuth>
          <SocialAuthLabel>{cta} with</SocialAuthLabel>
          <SocialAuthButtons gap='large' direction={socialButtonsDirection}>
            {=# isGoogleAuthEnabled =}
              <SocialButton href={googleSignInUrl}><SocialIcons.Google/></SocialButton>
            {=/ isGoogleAuthEnabled =}

            {=# isGitHubAuthEnabled =}
              <SocialButton href={gitHubSignInUrl}><SocialIcons.GitHub/></SocialButton>
            {=/ isGitHubAuthEnabled =}
          </SocialAuthButtons>
        </SocialAuth>
      {=/ isSocialAuthEnabled =}
      {=# areBothSocialAndPasswordBasedAuthEnabled =}
        <OrContinueWith>
          <OrContinueWithLineContainer>
            <OrContinueWithLine/>
          </OrContinueWithLineContainer>
          <OrContinueWithTextContainer>
            <OrContinueWithText>Or continue with</OrContinueWithText>
          </OrContinueWithTextContainer>
        </OrContinueWith>
      {=/ areBothSocialAndPasswordBasedAuthEnabled =}
      {=# isAnyPasswordBasedAuthEnabled =}
        <Form onSubmit={hookFormHandleSubmit(onSubmit)}>
          {=# isUsernameAndPasswordAuthEnabled =}
          <FormItemGroup>
            <FormLabel>Username</FormLabel>
            <FormInput
              {...register('username', {
                required: 'Username is required',
              })}
              type="text"
              disabled={isLoading}
            />
            {errors.username && <FormError>{errors.username.message}</FormError>}
          </FormItemGroup>
          {=/ isUsernameAndPasswordAuthEnabled =}
          {=# isEmailAuthEnabled =}
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
          {=/ isEmailAuthEnabled =}
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
      {=/ isAnyPasswordBasedAuthEnabled =}
  </>)
}

function AdditionalFormFields({
  hookForm,
  formState: { isLoading },
  additionalSignupFields,
}: {
  hookForm: UseFormReturn<LoginSignupFormFields>;
  formState: FormState;
  additionalSignupFields: AdditionalSignupFields;
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
    return (
      <FormItemGroup key={field.name}>
        <FormLabel>{field.label}</FormLabel>
        <Component
          {...register(field.name, field.validations)}
          {...props}
          disabled={isLoading}
        />
        {errors[field.name] && (
          <FormError>{errors[field.name].message}</FormError>
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
  additionalSignupFields: AdditionalSignupFields
): additionalSignupFields is AdditionalSignupFieldRenderFn {
  return typeof additionalSignupFields === 'function'
}
