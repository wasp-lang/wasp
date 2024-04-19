{{={= =}=}}
import React from 'react'

import { Redirect } from 'react-router-dom'
import { useAuth } from 'wasp/client/auth'
import { styled, keyframes } from 'wasp/core/stitches.config'

// TODO: I should probably extract Loader into separate file as a component?

const spinKeyframes = keyframes({
  '0%': { transform: 'rotate(0deg)' },
  '100%': { transform: 'rotate(360deg)' }
});

// TODO: Do I need to add !important on all of this stuff because of Tailwind css?
const Loader = styled('div', {
  display: 'flex',
  justifyContent: 'center',
  alignItems: 'center',
  height: '100vh',
  width: '100vw',
  color: '$gray900',

  // Spinner that is two quarters of a circle across each other.
  '&::after': {
    content: '',
    width: '40px',
    height: '40px',
    border: '5px solid currentColor',
    borderColor: 'currentColor transparent currentColor transparent',
    borderRadius: '50%',
    animation: `${spinKeyframes} 1s linear infinite`
  }
});

const createAuthRequiredPage = (Page) => {
  return (props) => {
    const { data: user, status, error } = useAuth();

    switch (status) {
      case 'success':
        if (user) {
          return (
            <Page {...props} user={user} />
          );
        } else {
          return <Redirect to="{= onAuthFailedRedirectTo =}" />;
        }
      case 'loading':
        return <Loader/>;
    case 'error':
      // TODO: Add global error boundary instead and remove this.
      //   But then I need to throw an error I think.
      return <span>ERROR {error?.message}</span>
    }
  }
}

export default createAuthRequiredPage
