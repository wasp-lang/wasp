# Changelog

## v0.5.2.1 (2022/07/14)

### Bug fixes
- Made wasp CLI more robust regarding encoding used on the machine.
- Worked around the bug in latest npm, so that Wasp now again supports latest LTS npm version.

---

## v0.5.2.0 (2022/06/23)

### Upgraded Prisma to latest version (13.15.2)

Among various other things, this brins support for OpenSSL3. So if you couldn't run Wasp on your operating system due to Prisma not supporting OpenSSL3, those days are over!

---

## v0.5.1.0 (2022/06/17)

### [NEW FEATURES]
- There is now `app.client.setup` function in .wasp that you can use to define custom setup you want to do on client before on its initialization.
- You can now configure the React Query's QueryClient by calling special function exposed by Wasp in your JS (in `app.client.setup`).

### Various improvements and bug fixes
- Limited Wasp node version to <=16.15.0 for now, since there is a problem with later versions and how Wasp uses `npx`.
- Reduced some of the redundant warning messages in Wasp CLI.
- Fixed unresponsive UI on server reload.

---

## v0.5.0.0 (2022/05/18)

### [NEW FEATURE] Wasp now has support for running Jobs!

If you have server tasks that you do not want to handle as part of the normal request-response cycle, now Wasp allows you to make that function a Job and it will gain some "superpowers"!

Jobs will persist between server restarts, can be retried if they fail, and they can even be delayed until the future (or have a recurring schedule)!

Some examples where you may want to use a Job on the server include sending an email, making an HTTP request to some external API, or doing some nightly calculations.

To run Jobs, you don't need any additional infrastructure at the moment, just a Postgre database that you anyway need to deploy Wasp to production.

### BREAKING CHANGES

- Wasp now requires latest LTS version of NodeJS
  - We had a bit of issues with being too relaxed on the version of NodeJS that can be used with Wasp so we thightened it up a bit.
    We also added a more thorough check in Wasp for it, that will warn you very explicitely if you are using the wrong version of Node.
- Updated react-query to v3
  - This brings some new features from react query while also laying the foundation for the further features we are building on top of it in Wasp (coming soon!).
- Updated python to python3 in Dockerfile generated upon `wasp build`.

### Various improvements

- Finally fixed a bug with orphaned processes in development.
- Various other bug fixes, doc improvements, and refactorings.

---

## v0.4.0.0 (2022/02/23)

### [BREAKING CHANGE] Upgrading Prisma to version 3.9.1

We are happy to announce Wasp is now using a much newer version of Prisma! This change does not impact the Wasp DSL support for Prisma, but it does come with some caveats from Prisma based on your usage. Please see this note for any breaking changes: https://www.prisma.io/docs/guides/upgrade-guides/upgrading-versions/upgrading-to-prisma-3

*Note: When you first migrate after upgrading, you will likely see a new migration created for 3.x specific features related to updating foreign keys and indexes.*

### Various improvements

- Automatically regenerating your Prisma client, as needed, based on your Prisma schema changes.
- Tracking your NPM project dependency changes and automatically invoking `npm install`, as needed, so you are always up to date.
- and more!

---

## v0.3.0.0 (2022/02/04)

### [BREAKING CHANGE] New Wasp-lang syntax!

Mostly it is very similar to what it was before, with some following bigger changes:
  - `auth`, `dependencies`, and couple of other "singleton" delcarations now became part of `app` declaration.
  - All declarations now need to have name, including `route`.
  - `route` has different syntax.
  - `dependencies` have different syntax.

For exact details about new syntax, check https://wasp-lang.dev/docs/language/syntax .

### Various improvements

  - Better compiler error messages.
  - Nicer CLI output.
  - Added delay on recompilation to avoid redundant recompiling.
  - Added `onAuthSucceededRedirectTo` field in `app`.
  - and more!

## Unreleased changes
