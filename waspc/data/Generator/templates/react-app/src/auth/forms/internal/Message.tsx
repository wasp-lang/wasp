import { styled } from '../../../stitches.config'

export const Message = styled('div', {
  padding: '0.5rem 0.75rem',
  borderRadius: '0.375rem',
  marginTop: '1rem',
  background: '$gray400',
})

export const ErrorMessage = styled(Message, {
  background: '$errorBackground',
  color: '$errorText',
})

export const SuccessMessage = styled(Message, {
  background: '$successBackground',
  color: '$successText',
})
