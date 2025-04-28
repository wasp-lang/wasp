import { verifyAllUserEmails } from 'wasp/client/operations'
export function HeadlessVerifyEmailsPage() {
  return (
    <div>
      <button onClick={() => verifyAllUserEmails()} className="btn btn-primary">
        Verify all user emails
      </button>
    </div>
  )
}
