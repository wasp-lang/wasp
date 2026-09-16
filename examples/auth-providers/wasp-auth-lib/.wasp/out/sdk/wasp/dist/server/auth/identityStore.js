import { prisma } from '../index.js';
import { normalizeProviderUserId, } from '../../auth/providerData.js';
export function getIdentityStore(providerName) {
    // Unknown provider names pass through normalization unchanged (its default
    // branch), so the cast only widens the accepted names.
    const normalize = (providerUserId) => normalizeProviderUserId(providerName, providerUserId);
    const whereIdentity = (providerUserId) => ({
        providerName_providerUserId: {
            providerName,
            providerUserId: normalize(providerUserId),
        },
    });
    return {
        async find(providerUserId) {
            const identity = await prisma.authIdentity.findUnique({
                where: whereIdentity(providerUserId),
            });
            if (identity === null) {
                return null;
            }
            return {
                providerName: identity.providerName,
                providerUserId: identity.providerUserId,
                authId: identity.authId,
                data: JSON.parse(identity.providerData),
                claims: JSON.parse(identity.providerClaims),
            };
        },
        async createIdentity(providerUserId, identity, userFields) {
            return prisma.user.create({
                data: {
                    // Using any here to prevent type errors when userFields are not
                    // defined. We want Prisma to throw an error in that case.
                    ...(userFields ?? {}),
                    auth: {
                        create: {
                            identities: {
                                create: {
                                    providerName,
                                    providerUserId: normalize(providerUserId),
                                    providerClaims: JSON.stringify(identity?.claims ?? {}),
                                    providerData: JSON.stringify(identity?.data ?? {}),
                                    providerSecrets: JSON.stringify(identity?.secrets ?? {}),
                                },
                            },
                        }
                    },
                },
                // We need to include the Auth entity here because we need `authId`
                // to be able to create a session.
                include: {
                    auth: true,
                },
            });
        },
        async provision(providerUserId, identity, userFields) {
            const existing = await this.find(providerUserId);
            if (existing !== null) {
                return { authId: existing.authId };
            }
            try {
                const created = await this.createIdentity(providerUserId, identity, userFields);
                return { authId: created.auth.id };
            }
            catch (e) {
                // Another request provisioned the same subject between our read and
                // our write. Its row is the winner; re-read rather than failing.
                if (isUniqueConstraintViolation(e)) {
                    const raced = await this.find(providerUserId);
                    return raced === null ? null : { authId: raced.authId };
                }
                throw e;
            }
        },
        async getSecrets(providerUserId) {
            const identity = await prisma.authIdentity.findUnique({
                where: whereIdentity(providerUserId),
                omit: { providerSecrets: false },
            });
            return identity === null ? null : JSON.parse(identity.providerSecrets);
        },
        async setSecrets(providerUserId, secrets) {
            await prisma.authIdentity.update({
                where: whereIdentity(providerUserId),
                data: { providerSecrets: JSON.stringify(secrets) },
            });
        },
        async updateData(providerUserId, updates) {
            const identity = await prisma.authIdentity.findUnique({
                where: whereIdentity(providerUserId),
                select: { providerData: true },
            });
            if (identity === null) {
                throw new Error('Auth identity not found.');
            }
            const newData = { ...JSON.parse(identity.providerData), ...updates };
            await prisma.authIdentity.update({
                where: whereIdentity(providerUserId),
                data: { providerData: JSON.stringify(newData) },
            });
        },
        async deleteUser(providerUserId) {
            const { count } = await prisma.user.deleteMany({
                where: {
                    auth: {
                        identities: {
                            some: {
                                providerName,
                                providerUserId: normalize(providerUserId),
                            },
                        },
                    },
                },
            });
            return count > 0;
        },
    };
}
function isUniqueConstraintViolation(e) {
    return (typeof e === 'object' && e !== null && 'code' in e && e.code === 'P2002');
}
//# sourceMappingURL=identityStore.js.map