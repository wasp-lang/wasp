import { requestPasswordReset } from '@wasp/auth/email'
import { errorMessage } from '@wasp/utils'

export function RequestPasswordReset() {
  const handleSubmit = async (e: any) => {
    e.preventDefault()
    const email = e.target[0].value as string
    try {
      await requestPasswordReset({ email })
      window.alert('Check your e-mail!')
    } catch (e) {
      window.alert(errorMessage(e))
    }
  }
  return (
    <div>
      <form className="flex flex-col gap-3 max-w-xs" onSubmit={handleSubmit}>
        <h1>Request password reset</h1>
        <input type="email" placeholder="Enter your e-mail" />
        <button className="btn btn-primary">Request password reset</button>
      </form>
    </div>
  )
}
