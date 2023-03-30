import { useEffect } from 'react'
import { useMutation } from '@tanstack/react-query'
import { Link, useHistory, useLocation } from 'react-router-dom'
import { verifyEmail } from '@wasp/auth/email'

export function EmailVerification() {
  const location = useLocation()
  const history = useHistory()
  const verifyEmailInfo = useMutation<
    { success: boolean },
    { data: { success: boolean; reason?: string } },
    { token: string },
    unknown
  >({
    mutationFn: verifyEmail,
  })
  useEffect(() => {
    const token = new URLSearchParams(location.search).get('token')
    if (!token) {
      history.push('/login')
      return
    }
    verifyEmailInfo.mutateAsync({ token })
  }, [location])
  return (
    <div>
      <h1>Email verification</h1>
      {verifyEmailInfo.isLoading && <p>Verifying email...</p>}
      {verifyEmailInfo.isError && (
        <p>
          Failed to verify email. Reason: {verifyEmailInfo.error.data?.reason}{' '}
          token
        </p>
      )}
      {verifyEmailInfo.isSuccess && verifyEmailInfo.data.success && (
        <>
          <p>Email verified successfully. ✅</p>
          <Link to="/login">Login</Link>
        </>
      )}
    </div>
  )
}
