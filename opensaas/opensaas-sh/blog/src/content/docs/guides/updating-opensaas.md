---
title: How (Not) to Update Your Open SaaS App
banner:
  content: |
    Have an Open SaaS app in production? <a href="https://e44cy1h4s0q.typeform.com/to/EPJCwsMi">We'll send you some swag! 👕</a>
---

:::danger[We advise against merging the latest template changes into your app]
If you've already started building your app, we generally advise against merging the latest template changes into your app.

Below we outline our reasoning why, and provide a basic guide to help you update your app if you decide to do so anyway.
:::


## Why you probably shouldn't include the latest template changes in your app

We generally **advise against updating your Open SaaS-based applications** after initial setup. 

Why? 

Because your codebase will naturally diverge from the template as you build your unique application, and any updates we may make to the template may not be compatible with your modified codebase, or your version of Wasp.

Even if you *really* want to include a new feature from the template in your app, proceed with caution and thoroughly consider the following:

- Changes to the template may be tightly coupled. Implementing one change without related ones could cause unexpected issues.
- Updates might not be compatible with your version of Wasp.
- The more your codebase has diverged, the more challenging the update will be.

## If you still decide to update your app

If you read above, considered the risks, and still need specific improvements, we recommend that you manually merge the changes.

To do this, you should can either 1) merge new Open SaaS template changes into your current project, or 2) merge project changes into a fresh Open SaaS template.

1) Merge new Open SaaS template changes into your current project by:
- reviewing the latest commits, 
- understanding what happened, 
- being mindful of the Wasp version you're using,
- and then fitting those changes into your own codebase.

2) Merge your project changes into a fresh Open SaaS template by:
- starting a new, fresh project with the latest Open SaaS template,
- and then copying over the logic from your existing project that you want to keep.

The method you choose is up to you and will largely depend on the complexity of the changes you need to make.