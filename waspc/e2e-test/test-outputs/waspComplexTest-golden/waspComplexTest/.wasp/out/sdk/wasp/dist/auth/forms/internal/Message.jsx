import { styled } from 'wasp/core/stitches.config';
// PRIVATE API
export const Message = styled('div', {
    padding: '0.5rem 0.75rem',
    borderRadius: '0.375rem',
    marginTop: '1rem',
    background: '$gray400',
});
// PRIVATE API
export const MessageError = styled(Message, {
    background: '$errorBackground',
    color: '$errorText',
});
// PRIVATE API
export const MessageSuccess = styled(Message, {
    background: '$successBackground',
    color: '$successText',
});
//# sourceMappingURL=Message.jsx.map