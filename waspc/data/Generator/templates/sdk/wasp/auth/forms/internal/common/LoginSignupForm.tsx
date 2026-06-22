{{={= =}=}}
import { useForm, UseFormReturn } from 'react-hook-form'
import { useLoginForm, useSignupForm } from '@wasp.sh/lib-auth/browser'
import styles from './LoginSignupForm.module.css'
import '../auth-styles.css'
import { config } from '../../../../client/index.js'
import { clsx } from '../util'

import {
  Form,
  FormInput,
  FormItemGroup,
  FormLabel,
  FormError,
  FormTextarea,
  SubmitButton,
} from '../Form'
import { MessageError, MessageSuccess } from '../Message'
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
import { useNavigate } from 'react-router'
{=/ isAnyPasswordBasedAuthEnabled =}
{=# enabledProviders.isUsernameAndPasswordAuthEnabled =}
import { login as usernameLogin, signup as usernameSignup } from '../../../username'
{=/ enabledProviders.isUsernameAndPasswordAuthEnabled =}
{=# enabledProviders.isEmailAuthEnabled =}
import { login as emailLogin } from '../../../email/actions/login'
import { signup as emailSignup } from '../../../email/actions/signup'
{=/ enabledProviders.isEmailAuthEnabled =}

{=# enabledProviders.isSlackAuthEnabled =}
const slackSignInUrl = `${config.apiUrl}{= slackSignInPath =}`
{=/ enabledProviders.isSlackAuthEnabled =}
{=# enabledProviders.isDiscordAuthEnabled =}
const discordSignInUrl = `${config.apiUrl}{= discordSignInPath =}`
{=/ enabledProviders.isDiscordAuthEnabled =}
{=# enabledProviders.isGoogleAuthEnabled =}
const googleSignInUrl = `${config.apiUrl}{= googleSignInPath =}`
{=/ enabledProviders.isGoogleAuthEnabled =}
{=# enabledProviders.isKeycloakAuthEnabled =}
const keycloakSignInUrl = `${config.apiUrl}{= keycloakSignInPath =}`
{=/ enabledProviders.isKeycloakAuthEnabled =}
{=# enabledProviders.isGitHubAuthEnabled =}
const gitHubSignInUrl = `${config.apiUrl}{= gitHubSignInPath =}`
{=/ enabledProviders.isGitHubAuthEnabled =}
{=# enabledProviders.isMicrosoftAuthEnabled =}
const microsoftSignInUrl = `${config.apiUrl}{= microsoftSignInPath =}`
{=/ enabledProviders.isMicrosoftAuthEnabled =}

{=!
// Since we allow users to add additional fields to the signup form, we don't
// know the exact shape of the form values. We are assuming that the form values
// will be a flat object with string values.
=}
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
  const isLogin = state === 'login'
  const cta = isLogin ? 'Log in' : 'Sign up'
  {=# isAnyPasswordBasedAuthEnabled =}
  const navigate = useNavigate()
  const hookForm = useForm<LoginSignupFormFields>()
  const { register, formState: { errors }, handleSubmit: hookFormHandleSubmit } = hookForm
  {=# enabledProviders.isUsernameAndPasswordAuthEnabled =}
  const identityField = 'username' as const
  {=/ enabledProviders.isUsernameAndPasswordAuthEnabled =}
  {=# enabledProviders.isEmailAuthEnabled =}
  const identityField = 'email' as const
  {=/ enabledProviders.isEmailAuthEnabled =}
  const loginForm = useLoginForm<LoginSignupFormFields>({
    identityField,
    submit: submitLogin,
  })
  const signupForm = useSignupForm<LoginSignupFormFields>({
    identityField,
    submit: submitSignup,
  })
  const authForm = isLogin ? loginForm : signupForm
  const isLoading = authForm.isSubmitting

  async function submitLogin(data: LoginSignupFormFields) {
    {=# enabledProviders.isUsernameAndPasswordAuthEnabled =}
    await usernameLogin(data as Parameters<typeof usernameLogin>[0])
    {=/ enabledProviders.isUsernameAndPasswordAuthEnabled =}
    {=# enabledProviders.isEmailAuthEnabled =}
    await emailLogin(data as Parameters<typeof emailLogin>[0])
    {=/ enabledProviders.isEmailAuthEnabled =}
    navigate('{= onAuthSucceededRedirectTo =}')
  }

  async function submitSignup(data: LoginSignupFormFields) {
    {=# enabledProviders.isUsernameAndPasswordAuthEnabled =}
    await usernameSignup(data as Parameters<typeof usernameSignup>[0])
    await usernameLogin(data as Parameters<typeof usernameLogin>[0])
    navigate('{= onAuthSucceededRedirectTo =}')
    {=/ enabledProviders.isUsernameAndPasswordAuthEnabled =}
    {=# enabledProviders.isEmailAuthEnabled =}
    await emailSignup(data as Parameters<typeof emailSignup>[0])
    hookForm.reset()
    return {
      successMessage: `You've signed up successfully! Check your email for the confirmation link.`,
    }
    {=/ enabledProviders.isEmailAuthEnabled =}
  }

  async function onSubmit(data: LoginSignupFormFields) {
    await authForm.submitFields(data)
  }
  {=/ isAnyPasswordBasedAuthEnabled =}

  return (<>
      {=# isAnyPasswordBasedAuthEnabled =}
        {authForm.errorMessage && (
          <MessageError>
            {authForm.errorMessage.title}{authForm.errorMessage.description && ': '}{authForm.errorMessage.description}
          </MessageError>
        )}
        {authForm.successMessage && <MessageSuccess>{authForm.successMessage}</MessageSuccess>}
      {=/ isAnyPasswordBasedAuthEnabled =}
      {=# isSocialAuthEnabled =}
        <div className={styles.socialAuth}>
          <div className={styles.socialAuthLabel}>{cta} with</div>
            <div className={clsx(styles.socialAuthButtons, styles[socialButtonsDirection])}>
            {=# enabledProviders.isSlackAuthEnabled =}
              <SocialButton href={slackSignInUrl}><SocialIcons.Slack/></SocialButton>
            {=/ enabledProviders.isSlackAuthEnabled =}

            {=# enabledProviders.isDiscordAuthEnabled =}
              <SocialButton href={discordSignInUrl}><SocialIcons.Discord/></SocialButton>
            {=/ enabledProviders.isDiscordAuthEnabled =}

            {=# enabledProviders.isGoogleAuthEnabled =}
              <SocialButton href={googleSignInUrl}><SocialIcons.Google/></SocialButton>
            {=/ enabledProviders.isGoogleAuthEnabled =}

            {=# enabledProviders.isKeycloakAuthEnabled =}
              <SocialButton href={keycloakSignInUrl}><SocialIcons.Keycloak/></SocialButton>
            {=/ enabledProviders.isKeycloakAuthEnabled =}

            {=# enabledProviders.isGitHubAuthEnabled =}
              <SocialButton href={gitHubSignInUrl}><SocialIcons.GitHub/></SocialButton>
            {=/ enabledProviders.isGitHubAuthEnabled =}

            {=# enabledProviders.isMicrosoftAuthEnabled =}
              <SocialButton href={microsoftSignInUrl}><SocialIcons.Microsoft/></SocialButton>
            {=/ enabledProviders.isMicrosoftAuthEnabled =}
          </div>
        </div>
      {=/ isSocialAuthEnabled =}
      {=# areBothSocialAndPasswordBasedAuthEnabled =}
        <div className={styles.orContinueWith}>
          <div className={styles.orContinueWithLineContainer}>
            <div className={styles.orContinueWithLine}/>
          </div>
          <div className={styles.orContinueWithTextContainer}>
            <span className={styles.orContinueWithText}>Or continue with</span>
          </div>
        </div>
      {=/ areBothSocialAndPasswordBasedAuthEnabled =}
      {=# isAnyPasswordBasedAuthEnabled =}
        <Form onSubmit={hookFormHandleSubmit(onSubmit)}>
          {=# enabledProviders.isUsernameAndPasswordAuthEnabled =}
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
          {=/ enabledProviders.isUsernameAndPasswordAuthEnabled =}
          {=# enabledProviders.isEmailAuthEnabled =}
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
          {=/ enabledProviders.isEmailAuthEnabled =}
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
