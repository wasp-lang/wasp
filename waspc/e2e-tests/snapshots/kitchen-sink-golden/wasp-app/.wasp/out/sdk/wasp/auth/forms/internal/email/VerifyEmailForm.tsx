import { useEffect } from 'react'
import { useLocation } from 'react-router-dom'
import { useAuthContext } from '@wasp.sh/lib-auth/browser'
import { verifyEmail } from '../../../email/actions/verifyEmail.js'
import { Message } from '../Message'

// PRIVATE API
export const VerifyEmailForm = () => {
  const { isLoading, setErrorMessage, setSuccessMessage, setIsLoading } = useAuthContext()
  const location = useLocation()
  const token = new URLSearchParams(location.search).get('token')

  async function submitForm() {
    if (!token) {
      setErrorMessage({
        title:
          'The token is missing from the URL. Please check the link you received in your email.',
      })
      return
    }
    setIsLoading(true)
    setErrorMessage(null)
    setSuccessMessage(null)
    try {
      await verifyEmail({ token })
      setSuccessMessage('Your email has been verified. You can now log in.')
    } catch (error) {
      setErrorMessage({
        title: error.message,
        description: error.data?.data?.message,
      })
    } finally {
      setIsLoading(false)
    }
  }

  useEffect(() => {
    submitForm()
  }, [location])

  return <>{isLoading && <Message>Verifying email...</Message>}</>
}
