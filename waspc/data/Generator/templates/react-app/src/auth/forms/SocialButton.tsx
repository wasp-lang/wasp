import { styled } from '../../stitches.config'

export const SocialButton = styled('a', {
    display: 'flex',
    justifyContent: 'center',

    cursor: 'pointer',
    // NOTE(matija): icon is otherwise blue, since that
    // is link's default font color.
    color: 'inherit',
    backgroundColor: '#f0f0f0',
    borderRadius: '0.375rem',
    borderWidth: '1px',
    borderColor: '$gray600',
    fontSize: '13px',
    padding: '0.5rem 0.75rem',
    boxShadow: '0 1px 2px 0 rgba(0, 0, 0, 0.05)',
    '&:visited': {
        color: 'inherit',
    },
    '&:hover': {
        backgroundColor: '$gray500',
        color: 'inherit',
    },
    transitionTimingFunction: 'cubic-bezier(0.4, 0, 0.2, 1)',
    transitionDuration: '100ms'
})