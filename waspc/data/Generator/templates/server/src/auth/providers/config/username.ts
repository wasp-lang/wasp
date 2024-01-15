{{={= =}=}}

import { Router } from "express";

import login from "../username/login.js";
import { getSignupRoute } from "../username/signup.js";
import { ProviderConfig } from "../types.js";

{=# userFieldsFn.isDefined =}
{=& userFieldsFn.importStatement =}
const _waspGetUserFieldsFn = {= userFieldsFn.importIdentifier =}
{=/ userFieldsFn.isDefined =}
{=^ userFieldsFn.isDefined =}
const _waspGetUserFieldsFn = undefined
{=/ userFieldsFn.isDefined =}

const config: ProviderConfig = {
    id: "{= providerId =}",
    displayName: "{= displayName =}",
    createRouter() {
        const router = Router();

        router.post('/login', login);
        const signupRoute = getSignupRoute({
            getUserFieldsFn: _waspGetUserFieldsFn,
        });
        router.post('/signup', signupRoute);

        return router;
    },
}

export default config;
