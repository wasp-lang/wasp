import { useLocation } from 'react-router'
import { useVerifyEmail } from '@wasp.sh/lib-auth/browser'
import { verifyEmail } from '../../../email/actions/verifyEmail.js'
import { Message, MessageError, MessageSuccess } from '../Message'

// PRIVATE API
export const VerifyEmailForm = () => {
  const location = useLocation()
  const token = new URLSearchParams(location.search).get('token')
  const form = useVerifyEmail({
    token,
    verify: verifyEmail,
  })

  return (
    <>
      {form.isSubmitting && <Message>Verifying email...</Message>}
      {form.errorMessage && (
        <MessageError>
          {form.errorMessage.title}{form.errorMessage.description && ': '}{form.errorMessage.description}
        </MessageError>
      )}
      {form.successMessage && <MessageSuccess>{form.successMessage}</MessageSuccess>}
    </>
  )
}
