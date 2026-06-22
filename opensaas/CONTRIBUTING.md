Thanks so much for considering contributing to Open SaaS 🙏

## Considerations before Contributing

Check if there is a GitHub issue already for the thing you would like to work on. If there is no issue yet, create a new one.

Let us know, in the issue, that you would like to work on it and how you plan to approach it.
This helps, especially with the more complex issues, as it allows us to discuss the solution upfront and make sure it is well planned and fits with the rest of the project.

## Repo organization

Repo is divided into two main parts: [template](/template) dir and [opensaas-sh](/opensaas-sh) dir.

`template` contains the actual open saas template that will be used by Wasp to create your new open-saas-based app when you run `wasp new -t saas`.

`opensaas-sh` is the app deployed to https://opensaas.sh , and is actually made with open saas! It contains a demo app and open saas docs. We keep it updated as we work on the template.

## How to Contribute

> [!IMPORTANT]
> The in-development version of the template uses the in-development version of Wasp. We've set up the `./tools/wasp` script.
> To use it, whenever you would normally run `wasp <command>`, run `<path-to-repo>/tools/wasp <command>` instead.

1. Make sure you understand the basics of how open-saas works (check out [docs](https://docs.opensaas.sh)).
2. Check out this repo (`main` branch) and make sure you are able to get the app in [template/app/](/template/app) running (to set it up, follow the same steps as for running a new open-saas app, as explained in the open-saas docs).
3. Create a new git branch for your work (aka feature branch) and do your changes on it.
4. Update e2e tests in [template/e2e-tests](/template/e2e-tests/) if needed and make sure they are passing.
5. Update docs in [opensaas-sh/blog/src/content/docs](/opensaas-sh/blog/src/content/docs/) if needed. Check [opensaas-sh/README.md](/opensaas-sh/README.md) for more details.
6. Update demo app in [opensaas-sh/app_diff](/opensaas-sh/app_diff) if needed. Check [opensaas-sh/README.md](/opensaas-sh/README.md) for more details.
7. Create a pull request (towards `main` as a base branch).
8. Make a "Da Boi" meme while you wait for us to review your PR(s).
9. If you don't know who "Da Boi" is, head back to the [Wasp Discord](https://discord.gg/aCamt5wCpS) and find out :)

## Additional Info

### Template Versioning

Whenever a user starts a new Wasp project with `wasp new -t <template_name>`, Wasp looks for a specific tag on the template repo, and pulls the project at the commit associated with that tag.

In the case of Open SaaS, which is a Wasp template, the tag is `wasp-v{{version}}-template`, where `{{version}}` is the current version of Wasp, e.g. `wasp-v0.13-template`.

We manually (re)assign the appropriate tag when we are ready to release a new version of Open Saas.

### Releasing

1. Assign appropriate tag to the commit we want to release (usually current `main`) so `wasp new -t saas` can pick up code from the [/template](/template).
2. Deploy the demo app + landing page and blog + docs: [/opensaas-sh/README.md#Deployment](/opensaas-sh/README.md#Deployment).
