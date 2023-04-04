{{={= =}=}}
import React, { useState, useEffect } from 'react'
import { useHistory } from 'react-router-dom'
import { createStitches, createTheme } from '@stitches/react'

import { errorMessage } from '../../utils.js'
{=# isUsernameAndPasswordAuthEnabled =}
import signup from '../signup.js'
import login from '../login.js'
{=/ isUsernameAndPasswordAuthEnabled =}
{=# isExternalAuthEnabled =}
import * as SocialIcons from './SocialIcons'
{=/ isExternalAuthEnabled =}

import config from '../../config.js'
import { styled, css } from '../../stitches.config'

const socialButtonsContainerStyle = {
  maxWidth: '20rem'
}

const logoStyle = {
  height: '3rem'
}

const Container = styled('div', {
  display: 'flex',
  flexDirection: 'column',
})

const HeaderText = styled('h2', {
  fontSize: '1.875rem',
  fontWeight: '700',
  marginTop: '1.5rem'
})

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

const SocialButton = styled('a', {
  display: 'flex',
  justifyContent: 'center',

  cursor: 'pointer',
  // NOTE(matija): icon is otherwise blue, since that
  // is link's default font color.
  color: 'inherit',
  backgroundColor: '#f0f0f0',
  borderRadius: '0.375rem',
  borderWidth: '1px',
  borderColor: '$gray600',
  fontSize: '13px',
  padding: '10px 15px',
  boxShadow: '0 1px 2px 0 rgba(0, 0, 0, 0.05)',
  '&:visited': {
    color: 'inherit',
  },
  '&:hover': {
    backgroundColor: '$gray500',
  },
  transitionTimingFunction: 'cubic-bezier(0.4, 0, 0.2, 1)',
  transitionDuration: '100ms'
})

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

// Email/username form

const UserPassForm = styled('form', {
  marginTop: '1.5rem'
})

const FormItemGroup = styled('div', {
  '& + div': {
    marginTop: '1.5rem'
  }
})

const FormLabel = styled('label', {
  display: 'block',
  fontSize: '$sm',
  fontWeight: '500'
})

const FormInput = styled('input', {
  display: 'block',
  lineHeight: '1.5rem',
  fontSize: '$sm',
  borderWidth: '1px',
  borderColor: '$gray600',
  backgroundColor: '#f8f4ff',
  boxShadow: '0 1px 2px 0 rgba(0, 0, 0, 0.05)',
  '&:focus': {
    borderWidth: '1px',
    borderColor: '$gray700',
    boxShadow: '0 1px 2px 0 rgba(0, 0, 0, 0.05)',
  },

  borderRadius: '0.375rem',
  width: '100%',

  paddingTop: '0.375rem',
  paddingBottom: '0.375rem',

  marginTop: '0.5rem'
})

const SubmitButton = styled('button', {
  display: 'flex',
  justifyContent: 'center',

  width: '100%',
  borderRadius: '0.375rem',
  borderWidth: '1px',
  borderColor: '$brand',
  backgroundColor: '$brand',
  color: '$submitButtonText',

  padding: '0.5rem 0.75rem',
  boxShadow: '0 1px 2px 0 rgba(0, 0, 0, 0.05)',

  fontWeight: '600',
  fontSize: '$sm',
  lineHeight: '1.25rem',
  borderRadius: '0.375rem',

  // TODO(matija): extract this into separate BaseButton component and then inherit it.
  '&:hover': {
    backgroundColor: '$brandAccent',
    borderColor: '$brandAccent',
  },
  transitionTimingFunction: 'cubic-bezier(0.4, 0, 0.2, 1)',
  transitionDuration: '100ms'
})

const googleSignInUrl = `${config.apiUrl}{= googleSignInPath =}`
const gitHubSignInUrl = `${config.apiUrl}{= gitHubSignInPath =}`

// TODO(matija): introduce type for appearance
const Auth = ({ isLogin, appearance, logo, socialLayout } :
              { isLogin: boolean; logo: string; socialLayout: "horizontal" | "vertical" }) => {
  const history = useHistory()

  const [usernameFieldVal, setUsernameFieldVal] = useState('')
  const [passwordFieldVal, setPasswordFieldVal] = useState('')

  const handleSubmit = async (event) => {
    event.preventDefault()
    try {
      if (!isLogin) {
        await signup({ username: usernameFieldVal, password: passwordFieldVal })
      }
      await login (usernameFieldVal, passwordFieldVal)

      setUsernameFieldVal('')
      setPasswordFieldVal('')

      // Redirect to configured page, defaults to /.
      history.push('{= onAuthSucceededRedirectTo =}')
    } catch (err) {
      console.log(err)
      window.alert(errorMessage(err))
    }
  }

  // TODO(matija): this is called on every render, is it a problem?
  // If we do it in useEffect(), then there is a glitch between the default color and the
  // user provided one.
  const customTheme = createTheme(appearance)

  const cta = isLogin ? 'Log in' : 'Sign up'
  const title = isLogin ? 'Log in to your account' : 'Create a new account'

  const socialButtonsDirection = socialLayout === 'vertical' ? 'vertical' : 'horizontal'

  return (
    <Container className={customTheme}>
      <div>
        {logo && (
          <img style={logoStyle} src={logo} alt='Your Company' />
        )}
        <HeaderText>{title}</HeaderText>
      </div>

      {=# isExternalAuthEnabled =}
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
      {=/ isExternalAuthEnabled =}
      
      {=# areBothExternalAndUsernameAndPasswordAuthEnabled =}
        <OrContinueWith>
          <OrContinueWithLineContainer>
            <OrContinueWithLine/>
          </OrContinueWithLineContainer>
          <OrContinueWithTextContainer>
            <OrContinueWithText>Or continue with</OrContinueWithText>
          </OrContinueWithTextContainer>
        </OrContinueWith>
      {=/ areBothExternalAndUsernameAndPasswordAuthEnabled =}
      
      {=# isUsernameAndPasswordAuthEnabled =}
        <UserPassForm onSubmit={handleSubmit}>
          <FormItemGroup>
            <FormLabel>Username</FormLabel>
            <FormInput
              type="text"
              value={usernameFieldVal}
              onChange={e => setUsernameFieldVal(e.target.value)}
            />
          </FormItemGroup>

          <FormItemGroup>
            <FormLabel>Password</FormLabel>
            <FormInput
              type="password"
              value={passwordFieldVal}
              onChange={e => setPasswordFieldVal(e.target.value)}
            />
          </FormItemGroup>

          <FormItemGroup>
            <SubmitButton type="submit">{cta}</SubmitButton>
          </FormItemGroup>
        </UserPassForm>
      {=/ isUsernameAndPasswordAuthEnabled =}
      
    </Container>
  )
}

export default Auth
