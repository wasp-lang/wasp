var __rest = (this && this.__rest) || function (s, e) {
    var t = {};
    for (var p in s) if (Object.prototype.hasOwnProperty.call(s, p) && e.indexOf(p) < 0)
        t[p] = s[p];
    if (s != null && typeof Object.getOwnPropertySymbols === "function")
        for (var i = 0, p = Object.getOwnPropertySymbols(s); i < p.length; i++) {
            if (e.indexOf(p[i]) < 0 && Object.prototype.propertyIsEnumerable.call(s, p[i]))
                t[p[i]] = s[p[i]];
        }
    return t;
};
import { hashPassword } from './password.js';
import { prisma, HttpError } from 'wasp/server';
import { sleep } from 'wasp/server/utils';
import { Prisma } from '@prisma/client';
import { throwValidationError } from './validation.js';
// PRIVATE API
export const contextWithUserEntity = {
    entities: {
        User: prisma.user
    }
};
// PRIVATE API
export const authConfig = {
    failureRedirectPath: "/login",
    successRedirectPath: "/",
};
// PUBLIC API
export function createProviderId(providerName, providerUserId) {
    return {
        providerName,
        providerUserId: providerUserId.toLowerCase(),
    };
}
// PUBLIC API
export async function findAuthIdentity(providerId) {
    return prisma.authIdentity.findUnique({
        where: {
            providerName_providerUserId: providerId,
        }
    });
}
// PUBLIC API
/**
 * Updates the provider data for the given auth identity.
 *
 * This function performs data sanitization and serialization.
 * Sanitization is done by hashing the password, so this function
 * expects the password received in the `providerDataUpdates`
 * **not to be hashed**.
 */
export async function updateAuthIdentityProviderData(providerId, existingProviderData, providerDataUpdates) {
    // We are doing the sanitization here only on updates to avoid
    // hashing the password multiple times.
    const sanitizedProviderDataUpdates = await ensurePasswordIsHashed(providerDataUpdates);
    const newProviderData = Object.assign(Object.assign({}, existingProviderData), sanitizedProviderDataUpdates);
    const serializedProviderData = await serializeProviderData(newProviderData);
    return prisma.authIdentity.update({
        where: {
            providerName_providerUserId: providerId,
        },
        data: { providerData: serializedProviderData },
    });
}
// PRIVATE API
export async function findAuthWithUserBy(where) {
    const result = await prisma.auth.findFirst({ where, include: { user: true } });
    if (result === null) {
        return null;
    }
    if (result.user === null) {
        return null;
    }
    return Object.assign(Object.assign({}, result), { user: result.user });
}
// PUBLIC API
export async function createUser(providerId, serializedProviderData, userFields) {
    return prisma.user.create({
        data: Object.assign(Object.assign({}, (userFields !== null && userFields !== void 0 ? userFields : {})), { auth: {
                create: {
                    identities: {
                        create: {
                            providerName: providerId.providerName,
                            providerUserId: providerId.providerUserId,
                            providerData: serializedProviderData,
                        },
                    },
                }
            } }),
        // We need to include the Auth entity here because we need `authId`
        // to be able to create a session.
        include: {
            auth: true,
        },
    });
}
// PRIVATE API
export async function deleteUserByAuthId(authId) {
    return prisma.user.deleteMany({ where: { auth: {
                id: authId,
            } } });
}
// PRIVATE API
// If an user exists, we don't want to leak information
// about it. Pretending that we're doing some work
// will make it harder for an attacker to determine
// if a user exists or not.
// NOTE: Attacker measuring time to response can still determine
// if a user exists or not. We'll be able to avoid it when 
// we implement e-mail sending via jobs.
export async function doFakeWork() {
    const timeToWork = Math.floor(Math.random() * 1000) + 1000;
    return sleep(timeToWork);
}
// PRIVATE API
export function rethrowPossibleAuthError(e) {
    // Prisma code P2002 is for unique constraint violations.
    if (e instanceof Prisma.PrismaClientKnownRequestError && e.code === 'P2002') {
        throw new HttpError(422, 'Save failed', {
            message: `user with the same identity already exists`,
        });
    }
    if (e instanceof Prisma.PrismaClientValidationError) {
        // NOTE: Logging the error since this usually means that there are
        // required fields missing in the request, we want the developer
        // to know about it.
        console.error(e);
        throw new HttpError(422, 'Save failed', {
            message: 'there was a database error'
        });
    }
    // Prisma code P2021 is for missing table errors.
    if (e instanceof Prisma.PrismaClientKnownRequestError && e.code === 'P2021') {
        // NOTE: Logging the error since this usually means that the database
        // migrations weren't run, we want the developer to know about it.
        console.error(e);
        console.info('🐝 This error can happen if you did\'t run the database migrations.');
        throw new HttpError(500, 'Save failed', {
            message: `there was a database error`,
        });
    }
    // Prisma code P2003 is for foreign key constraint failure
    if (e instanceof Prisma.PrismaClientKnownRequestError && e.code === 'P2003') {
        console.error(e);
        console.info(`🐝 This error can happen if you have some relation on your User entity
   but you didn't specify the "onDelete" behaviour to either "Cascade" or "SetNull".
   Read more at: https://www.prisma.io/docs/orm/prisma-schema/data-model/relations/referential-actions`);
        throw new HttpError(500, 'Save failed', {
            message: `there was a database error`,
        });
    }
    throw e;
}
// PRIVATE API
export async function validateAndGetUserFields(data, userSignupFields) {
    const { password: _password } = data, sanitizedData = __rest(data, ["password"]);
    const result = {};
    if (!userSignupFields) {
        return result;
    }
    for (const [field, getFieldValue] of Object.entries(userSignupFields)) {
        try {
            const value = await getFieldValue(sanitizedData);
            result[field] = value;
        }
        catch (e) {
            throwValidationError(e.message);
        }
    }
    return result;
}
// PUBLIC API
export function getProviderData(providerData) {
    return sanitizeProviderData(getProviderDataWithPassword(providerData));
}
// PUBLIC API
export function getProviderDataWithPassword(providerData) {
    // NOTE: We are letting JSON.parse throw an error if the providerData is not valid JSON.
    return JSON.parse(providerData);
}
function sanitizeProviderData(providerData) {
    if (providerDataHasPasswordField(providerData)) {
        const { hashedPassword } = providerData, rest = __rest(providerData, ["hashedPassword"]);
        return rest;
    }
    else {
        return providerData;
    }
}
// PUBLIC API
export async function sanitizeAndSerializeProviderData(providerData) {
    return serializeProviderData(await ensurePasswordIsHashed(providerData));
}
function serializeProviderData(providerData) {
    return JSON.stringify(providerData);
}
async function ensurePasswordIsHashed(providerData) {
    const data = Object.assign({}, providerData);
    if (providerDataHasPasswordField(data)) {
        data.hashedPassword = await hashPassword(data.hashedPassword);
    }
    return data;
}
function providerDataHasPasswordField(providerData) {
    return 'hashedPassword' in providerData;
}
// PRIVATE API
export function createInvalidCredentialsError(message) {
    return new HttpError(401, 'Invalid credentials', { message });
}
//# sourceMappingURL=utils.js.map