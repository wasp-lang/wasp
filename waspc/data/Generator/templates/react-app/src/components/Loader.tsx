import { styled, keyframes } from 'wasp/core/stitches.config'

const fullRotationKeyframes = keyframes({
  '0%': { transform: 'rotate(0deg)' },
  '100%': { transform: 'rotate(360deg)' },
})

// DefaultLoader is a React component that spans accross the whole screen and displays
// a spinner in the very middle.
export function Loader() {
  return (
    <SpinnerWrapper className="loader">
      <Spinner />
      <SpinnerAccessbilityText>Loading...</SpinnerAccessbilityText>
    </SpinnerWrapper>
  )
}

const SpinnerWrapper = styled('div', {
  color: '$gray900',
})

// Taken from Chakra UI Spinner component
const Spinner = styled('div', {
  display: 'inline-block',
  borderTop: '2px solid currentcolor',
  borderRight: '2px solid currentcolor',
  borderBottomStyle: 'solid',
  borderLeftStyle: 'solid',
  borderRadius: '99999px',
  borderBottomWidth: '2px',
  borderLeftWidth: '2px',
  borderBottomColor: 'transparent',
  borderLeftColor: 'transparent',
  animation: `0.45s linear 0s infinite normal none running ${fullRotationKeyframes}`,
  width: 'var(--spinner-size)',
  height: 'var(--spinner-size)',
  '--spinner-size': '2rem',
})

const SpinnerAccessbilityText = styled('span', {
  border: '0px',
  clip: 'rect(0px, 0px, 0px, 0px)',
  width: '1px',
  height: '1px',
  margin: '-1px',
  padding: '0px',
  overflow: 'hidden',
  whiteSpace: 'nowrap',
  position: 'absolute',
})
