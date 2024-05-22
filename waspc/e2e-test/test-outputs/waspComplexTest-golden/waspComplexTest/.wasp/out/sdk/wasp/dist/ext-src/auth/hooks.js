export const onBeforeSignup = async (args) => {
    const count = await args.prisma.user.count();
    console.log('before', count);
    console.log(args.providerId);
};
export const onAfterSignup = async (args) => {
    const count = await args.prisma.user.count();
    console.log('after', count);
    console.log('user', args.user);
};
export const onBeforeOAuthRedirect = async (args) => {
    console.log('redirect to', args.url.toString());
    return { url: args.url };
};
//# sourceMappingURL=hooks.js.map