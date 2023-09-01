import { SignupForm } from '@wasp/auth/forms/Signup'
import {
  FormError,
  FormInput,
  FormItemGroup,
  FormLabel,
} from '@wasp/auth/forms/internal/Form'

export const SignupPage = () => {
  return (
    <SignupForm
      additionalFields={(form, state) => {
        const username = form.watch('username')
        return (
          username && (
            <FormItemGroup>
              Hello there <strong>{username}</strong> 👋
            </FormItemGroup>
          )
        )
      }}
    />
  )
}
