import { Link } from 'react-router-dom'

export function EmailVerificationSuccess() {
  return (
    <div>
      <h1>Email verification success ✅</h1>
      <p style={{ margin: '0.5rem 0 1.5rem' }}>
        Your email has been successfully verified.
      </p>
      <Link to="/login">Login</Link>
    </div>
  )
}
