---
title: 'Incident Report: Case insensitive OAuth IDs vulnerability in Wasp'
authors: [miho]
image: /img/security-incident/security-incident.png
tags: [security, oauth, keycloak, wasp]
---

## Introduction

On May 23rd, 2025, we learned about a security vulnerability in Wasp auth related to our OAuth support in Wasp `0.16.5` and earlier.
Users with same IDs with different casing (e.g. `abc` and `ABC`) were considered the same person which could lead to users gaining access to other users' accounts.

**Only users using Keycloak with non-default case-sensitive IDs are affected.**

All other configurations (Google, GitHub, Discord, or Keycloak with the default case-insensitive configuration) are **not affected**. Email and username auth providers are also **not affected**.

Users should upgrade to Wasp version `0.16.6` which contains the fix. Affected Keycloak users will need to [migrate their user IDs](#migrate) in the database.

## Description

Wasp has a concept of a `ProviderId` which contains the provider name and the provider-specific ID. 

For example:

- (`email`, `alice@wasp.sh`) for Email auth
- (`google`, `10769150350006150715113082367`) for Google auth

Wasp takes the provider-specific ID (the second part), converts it to a string and lowercases it to keep the IDs normalized.

This approach made sense for:

- **email** - emails are case insensitive (e.g. you can't signup for Gmail with `johnnY@gmail.com` if `johnny@gmail.com` already exists)
- **usernames** - we don't want users to impersonate each other with similar usernames

However, the [OpenID spec](https://openid.net/specs/openid-connect-core-1_0.html#IDToken) states the following for the `sub` property (which we use for OAuth as user ID):

> REQUIRED.
	    Subject Identifier. A locally unique and never
	    reassigned identifier within the Issuer for the End-User,
            …
	    The sub value is a case-sensitive string.
> 

Treating the ID we receive as case insensitive violates the OpenID spec, but in practice, for providers that use numerical IDs, lowercasing the ID had no impact.

It worked fine for:

- **Google** - uses numeric ID (`10769150350006150715113082367`)
- **GitHub** - uses numeric ID (`1`)
- **Discord** - uses numeric ID (`80351110224678912`)

It was problematic for:

- **Keycloak** - uses lowercase UUID string (`25a37fd0-d10e-40ca-af6c-821f20e01be8`) by default, but you can configure Keycloak so that the IDs are case sensitive, which current Wasp auth ignores

  For example, two different users with Keycloak IDs `abc` and `ABC` would be considered the same person:
    - Example 1: `abc` person adds their credit card → `ABC` gets access to their credit card
    - Example 2: `admin` person exists → `ADMIN` gets admin rights as well

The [source](https://github.com/wasp-lang/wasp/blob/014f661a27f829bddf2290f7cdf1cd7c38f3387c/waspc/data/Generator/templates/sdk/wasp/auth/utils.ts#L85) of the problem is the `createProviderId` function that normalized the received ID:

```tsx
export function createProviderId(
  providerName: ProviderName,
  providerUserId: string,
): ProviderId {
  return {
    providerName,
    providerUserId: providerUserId.toLowerCase(), // <--- incorrect behavior
  }
}
```

## Applied fix

Wasp has released version `0.16.6` which includes a fix for the vulnerability - Wasp no longer lowercases user IDs received from OAuth providers, only the `email` and `username` user IDs.

Users who use Keycloak auth with a configuration that makes the user IDs case sensitive should upgrade immediately to Wasp `0.16.6` to keep their Wasp apps secure.

Upgrade to the latest Wasp version by running:

```bash
curl -sSL https://get.wasp.sh/installer.sh | sh -s
```

<small>

Commit addressing the issue: [wasp-lang/wasp#433b9b7](https://github.com/wasp-lang/wasp/commit/433b9b7f491c172db656fb94cc85e5bd7d614b74)
</small>


### Migration for Keycloak users {#migrate}

If you did have case sensitive IDs set up, your users will need to register again with Keycloak since their old Keycloak account ID won’t be correct.

If you wish to avoid this, you’ll need to manually update the database table called `AuthIdentity` to contain the correct user ID casing.

You will need to update the `providerUserId` column for all affected users to match the IDs in Keycloak. Read more about the `AuthIdentity` table in the [Wasp docs](/docs/auth/entities#authidentity-entity-).



## Timeline

- May 23rd: Received report from [@Scorpil](https://github.com/Scorpil)
- May 26th: Confirmed the vulnerability
- June 4th: Prepared the fix
- June 4th: Drafted a GitHub Security Advisory and requested a CVE
- June 4th: Notified most likely affected users on Discord privately
- June 9th: Released Wasp version `0.16.6` with the fix
- June 9th: Published the GitHub Security Advisory ([CVE-2025-49006](https://github.com/wasp-lang/wasp/security/advisories/GHSA-qvjc-6xv7-6v5f))
- June 9th: Notified users on Discord publicly
- June 20th: Published this incident report
