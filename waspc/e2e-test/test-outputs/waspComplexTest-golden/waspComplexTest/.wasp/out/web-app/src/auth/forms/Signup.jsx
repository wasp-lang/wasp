import React, { useState } from 'react'

import Auth from './Auth'

const SignupForm = ({ appearance, logo, socialLayout }) => {
  return (
    <Auth
      appearance={appearance}
      logo={logo}
      socialLayout={socialLayout}
      isLogin={false}
    />
  )


}

export default SignupForm
