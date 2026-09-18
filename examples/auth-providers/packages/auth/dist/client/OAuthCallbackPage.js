import { jsx as _jsx, jsxs as _jsxs } from "react/jsx-runtime";
import { useState } from "react";
import { Navigate, useLocation } from "react-router";
import { confirmMerge, exchangeOAuthCodeForSession } from "./actions.js";
import { Message, MessageError } from "./forms/internal/Message.js";
import { useEffectOnce } from "./hooks.js";
import { getClientConfig, getClientRuntime } from "./runtime.js";
/**
 * The client route the OAuth handback lands on: redeems the one-time code,
 * then redirects. After an account-linking flow (`startOAuthLink`) there is
 * no code to redeem: the user is refreshed and sent to `linkedRedirectTo`.
 */
export function OAuthCallbackPage({ linkedRedirectTo, } = {}) {
    const [error, setError] = useState(null);
    const [isDone, setIsDone] = useState(false);
    const [isLinked, setIsLinked] = useState(false);
    const [mergeTicket, setMergeTicket] = useState(null);
    const location = useLocation();
    useEffectOnce(() => {
        (async () => {
            const query = new URLSearchParams(location.search);
            const errorFromRedirect = query.get("error");
            if (errorFromRedirect !== null) {
                setError(errorFromRedirect);
                return;
            }
            // The provider account belongs to another account of this user: the
            // merge needs their explicit yes, so nothing happens until they click.
            const ticket = query.get("mergeTicket");
            if (ticket !== null) {
                setMergeTicket(ticket);
                return;
            }
            if (query.get("linked") !== null) {
                await getClientRuntime().refreshUser();
                setIsLinked(true);
                return;
            }
            try {
                await exchangeOAuthCodeForSession(location.hash.slice(1));
                setIsDone(true);
            }
            catch (e) {
                console.error(e);
                setError("Unable to login with the OAuth provider.");
            }
        })();
    });
    if (mergeTicket !== null && !isLinked) {
        return (_jsxs(Message, { children: ["That account already belongs to another account here. Merge it into the one you are signed in to? Its data moves here and it is deleted.", " ", _jsx("button", { onClick: async () => {
                        try {
                            await confirmMerge(mergeTicket);
                            setIsLinked(true);
                        }
                        catch (e) {
                            console.error(e);
                            setMergeTicket(null);
                            setError("Merging the accounts failed.");
                        }
                    }, children: "Merge accounts" })] }));
    }
    if (isLinked) {
        return (_jsx(Navigate, { to: linkedRedirectTo ?? getClientConfig().onAuthSucceededRedirectTo, replace: true }));
    }
    if (isDone) {
        return (_jsx(Navigate, { to: getClientConfig().onAuthSucceededRedirectTo, replace: true }));
    }
    if (error) {
        return _jsx(MessageError, { children: error });
    }
    return _jsx(Message, { children: "Please wait a moment while we log you in." });
}
