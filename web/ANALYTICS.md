# Website analytics events

The website sends a few custom events to Plausible and, where it runs, to PostHog. This file gives a short overview. Read it before you move or rewrite a component that has a `data-placement` or `data-track` attribute, or a `track()` call.

## How it works

- `src/lib/analytics.ts` has `track(name, props)`. It sends each event to Plausible (every visitor, no cookie) and to PostHog with the same name. PostHog runs only on `wasp.sh`, after a visitor accepts cookies.
- `src/clientModules/track.ts` listens to clicks and copies on the whole page: the code block copy button, text copied by hand, and links to Discord, GitHub and Open SaaS.
- Components with their own copy button call `track()` themselves: the homepage install bar, `InstallBlock` and the newsletter form. `InstallBlock` also sets `data-copy-event` and `data-copy-kind`, so a copy by hand sends the same event as its button.
- `src/lib/analytics-classifier.ts` decides which event a copied code block is.

## Events

| Event | When | Properties |
| --- | --- | --- |
| `Install Command: Copy` | A copy of `npm i -g @wasp.sh/wasp-cli`, from any copy button or by hand | `placement`, `method` |
| `CLI Command: Copy` | A copy from another shell code block | `command`, `placement`, `method` |
| `Agent Plugin: Copy` | A copy of an agent plugin command or prompt | `kind`, `placement`, `method` |
| `Newsletter: Signup` | Loops accepts a newsletter signup | `placement` |
| `Newsletter: Error` | Loops rejects a signup, or the request fails | `status`, `placement` |
| `Discord: Click` | A click on a Discord invite link, or on a `/discord/<source>` link | `placement` |
| `Get Started: Click` | A click on an element with `data-track="get_started"` | `placement` |
| `GitHub: Click` | A click on a link to `github.com/wasp-lang/...` | `target`, `placement` |
| `Open SaaS: Click` | A click on a link to `opensaas.sh` or `docs.opensaas.sh` | `placement` |

`placement` says where on the page the event happened. It comes from the nearest `data-placement` attribute, or from the Docusaurus theme class of that part of the page. Plausible adds the page, the source and the country by itself.

## Property values

| Property | Values |
| --- | --- |
| `placement` | `announcement_bar`, `header`, `mobile_menu`, `hero`, `body`, `post_card`, `post_list`, `sidebar`, `toc`, `doc_footer`, `footer` |
| `method` | `button`, `selection` |
| `command` | `wasp_new`, `wasp_start`, `wasp_db`, `wasp_deploy`, `wasp_build`, `wasp_other`, `other_shell` |
| `kind` | `claude_plugin`, `skills_cli`, `plugin_prompt`, `example_prompt` |
| `target` | `repo`, `examples`, `issues`, `edit_page`, `other_repo` |
| `status` | `400`, `429`, `500`, `network` |

## Rules

- Event names and property values are fixed once they are live. A renamed event is a new event in Plausible and PostHog.
- Plausible counts an event only if a goal with the same name exists. Create the goal before the event goes live.
- Never send copied text, an email address, an ID or other personal data as a property.
- `TextLink`, `CtaLink` and `FaqLink` drop unknown props. Put `data-*` attributes on a container.
- `data-track="get_started"` marks the Get Started buttons. `data-track="announcement"` marks the announcement bar for a PostHog action.

## How to test

- `npm start`: each event prints `[analytics] <event> <props>` in the browser console. Nothing is sent.
- In a production build, run `localStorage.setItem("wasp-analytics-debug", "true")` in the browser console to see the same log.
