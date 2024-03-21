import { SignupForm } from "wasp/client/auth";

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
