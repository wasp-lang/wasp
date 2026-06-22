import { useForgotPasswordForm } from '@wasp.sh/lib-auth/browser'

import { requestPasswordReset } from '../../../email/actions/passwordReset.js'
import { Form, FormItemGroup, FormLabel, FormInput, SubmitButton, FormError } from '../Form'
import { MessageError, MessageSuccess } from '../Message'

// PRIVATE API
export const ForgotPasswordForm = () => {
  const form = useForgotPasswordForm({
    submit: requestPasswordReset,
  })

  return (
    <>
      {form.errorMessage && (
        <MessageError>
          {form.errorMessage.title}{form.errorMessage.description && ': '}{form.errorMessage.description}
        </MessageError>
      )}
      {form.successMessage && <MessageSuccess>{form.successMessage}</MessageSuccess>}
      <Form onSubmit={(event) => void form.submit(event)}>
        <FormItemGroup>
          <FormLabel>E-mail</FormLabel>
          <FormInput
            {...form.getFieldProps('email')}
            type="email"
          />
          {form.fieldErrors.email && <FormError>{form.fieldErrors.email}</FormError>}
        </FormItemGroup>
        <FormItemGroup>
          <SubmitButton type="submit" disabled={form.isSubmitting}>
            Send password reset email
          </SubmitButton>
        </FormItemGroup>
      </Form>
    </>
  )
}
