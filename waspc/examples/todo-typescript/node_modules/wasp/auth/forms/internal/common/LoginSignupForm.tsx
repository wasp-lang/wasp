import { useContext, type FormEvent } from 'react'
import { styled } from 'wasp/core/stitches.config'
import config from 'wasp/core/config'

import { AuthContext } from '../../Auth'
import {
  Form,
  FormInput,
  FormItemGroup,
  FormLabel,
  SubmitButton,
} from '../Form'
import { useHistory } from 'react-router-dom'
import { useUsernameAndPassword } from '../usernameAndPassword/useUsernameAndPassword'

export const LoginSignupForm = ({
  state,
  socialButtonsDirection = 'horizontal',
}: {
  state: 'login' | 'signup'
  socialButtonsDirection?: 'horizontal' | 'vertical'
}) => {
  const { isLoading, setErrorMessage, setSuccessMessage, setIsLoading } =
    useContext(AuthContext)
  const cta = state === 'login' ? 'Log in' : 'Sign up'
  const history = useHistory()
  const onErrorHandler = (error) => {
    setErrorMessage({
      title: error.message,
      description: error.data?.data?.message,
    })
  }
  const {
    handleSubmit,
    usernameFieldVal,
    passwordFieldVal,
    setUsernameFieldVal,
    setPasswordFieldVal,
  } = useUsernameAndPassword({
    isLogin: state === 'login',
    onError: onErrorHandler,
    onSuccess() {
      history.push('/')
    },
  })
  async function onSubmit(event: FormEvent<HTMLFormElement>) {
    event.preventDefault()
    setIsLoading(true)
    setErrorMessage(null)
    setSuccessMessage(null)
    try {
      await handleSubmit()
    } finally {
      setIsLoading(false)
    }
  }

  return (
    <>
      <Form onSubmit={onSubmit}>
        <FormItemGroup>
          <FormLabel>Username</FormLabel>
          <FormInput
            type="text"
            required
            value={usernameFieldVal}
            onChange={(e) => setUsernameFieldVal(e.target.value)}
            disabled={isLoading}
          />
        </FormItemGroup>
        <FormItemGroup>
          <FormLabel>Password</FormLabel>
          <FormInput
            type="password"
            required
            value={passwordFieldVal}
            onChange={(e) => setPasswordFieldVal(e.target.value)}
            disabled={isLoading}
          />
        </FormItemGroup>
        <FormItemGroup>
          <SubmitButton type="submit" disabled={isLoading}>
            {cta}
          </SubmitButton>
        </FormItemGroup>
      </Form>
    </>
  )
}
