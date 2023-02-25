import passport from "passport";

import waspServerConfig from '../../../config.js'

import { InitData, ProviderConfig } from "../types.js";

export function makeOAuthInit({ passportImportPath, npmPackage }: OAuthImports) {
    return async function init(provider: ProviderConfig): Promise<InitData> {
        const { config, getUserFieldsFn } = await import(passportImportPath);
        const ProviderStrategy = await import(npmPackage);

        const passportStrategyName = `wasp${provider.slug}LoginStrategy`;
        const requiredConfig = {
            callbackURL: `${waspServerConfig.frontendUrl}/auth/login/${provider.slug}`,
            passReqToCallback: true
        };
        const passportStrategy = new ProviderStrategy.default(
            { ...config, ...requiredConfig },
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

type OAuthImports = {
    npmPackage: string;
    passportImportPath: string;
};
