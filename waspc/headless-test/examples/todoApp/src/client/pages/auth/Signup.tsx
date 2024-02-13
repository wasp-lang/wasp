import { Link } from "wasp/client/router";
import { SignupForm } from "wasp/client/auth";
import { useQuery, getNumTasks } from "wasp/client/operations";
import { getTotalTaskCountMessage } from './helpers'

import appearance from './appearance'
import todoLogo from '../../todoLogo.png'

const Signup = () => {
  const { data: numTasks } = useQuery(getNumTasks)

  return (
    <div className="w-full h-full bg-white">
      <div className="min-w-full min-h-[75vh] flex items-center justify-center">
        <div className="w-full h-full max-w-sm p-5 bg-white">
          <div>
            <SignupForm
              appearance={appearance}
              logo={todoLogo}
              socialLayout="horizontal"
            />
            <br />
            <span className="text-sm font-medium text-gray-900">
              I already have an account (<Link to="/login">go to login</Link>).
            </span>
            <br />
          </div>
          <br />
          <br />
          <span>{getTotalTaskCountMessage(numTasks)}</span>
        </div>
      </div>
    </div>
  )
}

export default Signup
