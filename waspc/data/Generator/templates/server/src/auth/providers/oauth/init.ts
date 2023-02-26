import passport from "passport";

import waspServerConfig from '../../../config.js';

import { InitData, ProviderConfig, OAuthConfig } from "../types.js";

export function makeOAuthInit({ passportImportPath, npmPackage, oAuthConfig }: OAuthImports) {
    return async function init(provider: ProviderConfig): Promise<InitData> {
        const { config: userDefinedConfig, getUserFieldsFn } = await import(passportImportPath);
        const ProviderStrategy = await import(npmPackage);

        const passportStrategyName = `wasp${provider.slug}LoginStrategy`;
        const requiredConfig = {
            clientID: oAuthConfig.clientID,
            clientSecret: oAuthConfig.clientSecret,
            // TODO: enable user to extend the scope
            scope: oAuthConfig.scope,
            callbackURL: `${waspServerConfig.frontendUrl}/auth/login/${provider.slug}`,
            passReqToCallback: true
        };

        const config = { ...requiredConfig, ...userDefinedConfig };
        ensureValidConfig(provider, config);

        const passportStrategy = new ProviderStrategy.default(
            config,
            addProviderProfileToRequest
        );
        passport.use(passportStrategyName, passportStrategy);

        return {
            passportStrategyName,
            getUserFieldsFn,
        };
    }
}

// This function is invoked after we successfully exchange the one-time-use OAuth code for a real provider API token.
// This token was used to get the provider profile information supplied as a parameter.
// We add the provider profile to the request for downstream use.
async function addProviderProfileToRequest(req, _accessToken, _refreshToken, providerProfile, done) {
    req.wasp = { ...req.wasp, providerProfile };
    done(null, {});
}

function ensureValidConfig(provider: ProviderConfig, config: OAuthConfig): void {
    if (!config.clientID) {
        throw new Error(`The ${provider.slug} configFn must return an object with a clientID property.`)
    }

    if (!config.clientSecret) {
        throw new Error(`The ${provider.slug} configFn must return an object with a clientSecret property.`)
    }

    if (!config.scope || !Array.isArray(config.scope)) {
        throw new Error(`The ${provider.slug} configFn must return an object with a scope property.`)
    }
}

type OAuthImports = {
    npmPackage: string;
    passportImportPath: string;
    oAuthConfig: OAuthConfig;
};
