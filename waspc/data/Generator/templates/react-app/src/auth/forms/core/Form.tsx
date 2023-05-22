import { styled } from '../../../stitches.config'

export const FormItemGroup = styled('div', {
    '& + div': {
        marginTop: '1.5rem'
    }
})

export const FormLabel = styled('label', {
    display: 'block',
    fontSize: '$sm',
    fontWeight: '500'
})

export const FormInput = styled('input', {
    display: 'block',
    lineHeight: '1.5rem',
    fontSize: '$sm',
    borderWidth: '1px',
    borderColor: '$gray600',
    backgroundColor: '#f8f4ff',
    boxShadow: '0 1px 2px 0 rgba(0, 0, 0, 0.05)',
    '&:focus': {
        borderWidth: '1px',
        borderColor: '$gray700',
        boxShadow: '0 1px 2px 0 rgba(0, 0, 0, 0.05)',
    },
    '&:disabled': {
        opacity: 0.5,
        cursor: 'not-allowed',
        backgroundColor: '$gray400',
        borderColor: '$gray400',
        color: '$gray500',
    },

    borderRadius: '0.375rem',
    width: '100%',

    paddingTop: '0.375rem',
    paddingBottom: '0.375rem',
    paddingLeft: '0.75rem',
    paddingRight: '0.75rem',

    marginTop: '0.5rem'
})

export const SubmitButton = styled('button', {
    display: 'flex',
    justifyContent: 'center',

    width: '100%',
    borderWidth: '1px',
    borderColor: '$brand',
    backgroundColor: '$brand',
    color: '$submitButtonText',

    padding: '0.5rem 0.75rem',
    boxShadow: '0 1px 2px 0 rgba(0, 0, 0, 0.05)',

    fontWeight: '600',
    fontSize: '$sm',
    lineHeight: '1.25rem',
    borderRadius: '0.375rem',

    // TODO(matija): extract this into separate BaseButton component and then inherit it.
    '&:hover': {
        backgroundColor: '$brandAccent',
        borderColor: '$brandAccent',
    },
    '&:disabled': {
        opacity: 0.5,
        cursor: 'not-allowed',
        backgroundColor: '$gray400',
        borderColor: '$gray400',
        color: '$gray500',
    },
    transitionTimingFunction: 'cubic-bezier(0.4, 0, 0.2, 1)',
    transitionDuration: '100ms'
})