import { signInUrl as googleSignInUrl } from '@wasp/auth/helpers/Google'

import googleLogo from '../google-logo.svg'

const GoogleAuthButton = () => (
  <div className='w-full mt-3 text-center'>
    <a
      href={googleSignInUrl}
      className={`
        flex items-center
        block w-full border border-neutral-200
        h-10
        text-sm font-bold
        rounded-sm shadow-md
        transition ease-out duration-200
        hover:bg-neutral-100
      `}
    >
      <img src={googleLogo}
        className={`h-5 pl-2`}
      />
      <div className='w-full flex justify-center items-center pr-2'>
        Continue with Google
      </div>
    </a>
  </div>
)

export default GoogleAuthButton
