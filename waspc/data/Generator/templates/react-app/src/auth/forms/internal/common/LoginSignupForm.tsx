{{={= =}=}}
import { useContext, type FormEvent } from 'react'
import { styled } from '../../../../stitches.config'
import config from '../../../../config.js'

import { AuthContext } from '../../Auth'
import { Form, FormInput, FormItemGroup, FormLabel, SubmitButton } from '../Form'
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
  {=# isAnyPasswordBasedAuthEnabled =}
  const history = useHistory();
  const onErrorHandler = (error) => {
    setErrorMessage({ title: error.message, description: error.data?.data?.message })
  };
  {=/ isAnyPasswordBasedAuthEnabled =}
  {=# isUsernameAndPasswordAuthEnabled =}
  const { handleSubmit, usernameFieldVal, passwordFieldVal, setUsernameFieldVal, setPasswordFieldVal } = useUsernameAndPassword({
    isLogin: state === 'login',
    onError: onErrorHandler,
    onSuccess() {
      history.push('{= onAuthSucceededRedirectTo =}')
    },
  });
  {=/ isUsernameAndPasswordAuthEnabled =}
  {=# isEmailAuthEnabled =}
  const { handleSubmit, emailFieldVal, passwordFieldVal, setEmailFieldVal, setPasswordFieldVal } = useEmail({
    isLogin: state === 'login',
    onError: onErrorHandler,
    showEmailVerificationPending() {
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
  async function onSubmit (event: FormEvent<HTMLFormElement>) {
    event.preventDefault();
    setIsLoading(true);
    setErrorMessage(null);
    setSuccessMessage(null);
    try {
      await handleSubmit();
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
        <Form onSubmit={onSubmit}>
          {=# isUsernameAndPasswordAuthEnabled =}
          <FormItemGroup>
            <FormLabel>Username</FormLabel>
            <FormInput
              type="text"
              required
              value={usernameFieldVal}
              onChange={e => setUsernameFieldVal(e.target.value)}
              disabled={isLoading}
            />
          </FormItemGroup>
          {=/ isUsernameAndPasswordAuthEnabled =}
          {=# isEmailAuthEnabled =}
          <FormItemGroup>
            <FormLabel>E-mail</FormLabel>
            <FormInput
              type="email"
              required
              value={emailFieldVal}
              onChange={e => setEmailFieldVal(e.target.value)}
              disabled={isLoading}
            />
          </FormItemGroup>
          {=/ isEmailAuthEnabled =}
          <FormItemGroup>
            <FormLabel>Password</FormLabel>
            <FormInput
              type="password"
              required
              value={passwordFieldVal}
              onChange={e => setPasswordFieldVal(e.target.value)}
              disabled={isLoading}
            />
          </FormItemGroup>
          <FormItemGroup>
            <SubmitButton type="submit" disabled={isLoading}>{cta}</SubmitButton>
          </FormItemGroup>
        </Form>
      {=/ isAnyPasswordBasedAuthEnabled =}
  </>)
}
