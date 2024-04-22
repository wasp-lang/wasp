import { styled, keyframes } from 'wasp/core/stitches.config'

const fullRotationKeyframes = keyframes({
  '0%': { transform: 'rotate(0deg)' },
  '100%': { transform: 'rotate(360deg)' }
});

// TODO: Do I need to add !important on all of this stuff because of Tailwind css?
// DefaultLoader is a React component that spans accross the whole screen and displays
// a spinner in the very middle.
export const DefaultLoader = styled('div', {
  display: 'flex',

  // Accross the whole screen.
  height: '100vh',
  width: '100vw',

  // Spinner in the middle.
  justifyContent: 'center',
  alignItems: 'center',

  // Color of the spinner.
  color: '$gray900',

  // This is a spinner: circle that has 1/4 of border visible, then 1/4 transparent, then again 1/4
  // visible and then again 1/4 transparent.
  '&::after': {
    content: '',
    width: '40px',
    height: '40px',
    border: '5px solid currentColor',
    borderColor: 'currentColor transparent currentColor transparent',
    borderRadius: '50%',
    animation: `${fullRotationKeyframes} 1s linear infinite`
  }
});

