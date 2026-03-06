# Crossposting Blog Articles to Dev.to and Medium

This skill allows you to crosspost Wasp blog articles (MDX) to DEV.to and Medium.
If your article has local .mp4 videos, the skill can optionally upload them to YouTube for use on these platforms.

## API Keys and Secrets

The Dev.to API key, as well as the Youtube Client ID and Secret, are saved in the Wasp shared 1password account.

For Medium, allow Claude to start a Chrome browser session, then use email login (not google oauth) to log in with `info@wasp-lang.dev` using the OTP sent to your email.

## How To Crosspost

- Finish your MDX blogpost for wasp.sh
- Invoke the skill: `/crossposting`

If you need more info, just ask Claude how it works.
