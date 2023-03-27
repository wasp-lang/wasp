import { useHistory, useLocation } from 'react-router-dom'

import { resetPassword } from '@wasp/auth/email'

export function PasswordReset() {
  const location = useLocation()
  const history = useHistory()
  const token = new URLSearchParams(location.search).get('token')
  const handleSubmit = async (e: any) => {
    e.preventDefault()
    if (!token) {
      alert('Invalid token!')
      return
    }
    const password = e.target[0].value as string
    const passwordConfirmation = e.target[1].value as string
    if (!password || password !== passwordConfirmation) {
      alert("Passwords don't match!")
      return
    }
    try {
      await resetPassword({ newPassword: password, token })
      history.push('/login')
    } catch (e: any) {
      alert(e.message)
    }
  }
  return (
    <form className="flex flex-col gap-3 max-w-xs" onSubmit={handleSubmit}>
      <h1>Reset password</h1>
      <input type="password" placeholder="Enter new password" />
      <input type="password" placeholder="Confirm new password" />
      <button className="btn btn-primary">Reset password</button>
    </form>
  )
}
