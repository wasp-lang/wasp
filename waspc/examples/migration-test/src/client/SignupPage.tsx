import { SignupForm } from '@wasp/auth/forms/Signup'

export const SignupPage = () => {
  return (
    <SignupForm
      additionalFields={[
        {
          name: 'address',
          label: 'Address',
          type: 'input',
          validations: {
            required: 'Address is required'
          }
        }
      ]}
    />
  )
}