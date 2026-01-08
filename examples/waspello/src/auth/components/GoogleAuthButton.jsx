import { googleSignInUrl } from "wasp/client/auth";

import googleLogo from "./google-logo.svg";

const GoogleAuthButton = () => (
  <div className="mt-3 w-full text-center">
    <a
      href={googleSignInUrl}
      className={`block flex h-10 w-full items-center rounded-xs border border-neutral-200 text-sm font-bold shadow-md transition duration-200 ease-out hover:bg-neutral-100`}
    >
      <img src={googleLogo} className={`h-5 pl-2`} />
      <div className="flex w-full items-center justify-center pr-2">
        Continue with Google
      </div>
    </a>
  </div>
);

export default GoogleAuthButton;
