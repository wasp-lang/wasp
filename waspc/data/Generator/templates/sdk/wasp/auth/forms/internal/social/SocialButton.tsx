import { styled } from 'wasp/core/stitches.config'

// Note about using !important with some of the components:
// This is a workaround for CSS generated by Stitches not being specific enough
// and thus being overridden by Tailwind CSS. https://github.com/wasp-lang/wasp/issues/1764
// Long term we want to move away from Stitches and this is an acceptable workaround for now.

// PRIVATE API
export const SocialButton = styled('a', {
  display: 'flex !important',
  justifyContent: 'center !important',

  cursor: 'pointer !important',
  // NOTE(matija): icon is otherwise blue, since that
  // is link's default font color.
  color: 'inherit !important',
  backgroundColor: '#f0f0f0 !important',
  borderRadius: '0.375rem !important',
  borderWidth: '1px !important',
  borderColor: '$gray600 !important',
  fontSize: '13px !important',
  padding: '0.5rem 0.75rem !important',
  boxShadow: '0 1px 2px 0 rgba(0, 0, 0, 0.05) !important',
  '&:visited': {
    color: 'inherit !important',
  },
  '&:hover': {
    backgroundColor: '$gray500 !important',
    color: 'inherit !important',
  },
  transitionTimingFunction: 'cubic-bezier(0.4, 0, 0.2, 1) !important',
  transitionDuration: '100ms !important',
})
