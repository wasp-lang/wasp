# Wasp docs writing guide

Our docs are the first touch point for new Wasp users.
It's the top of our funnel.
If we lose them on the docs, they will never get a chance to use Wasp (even if they might like it).

This guide combines the excellent [Vue Docs Writing Guide](https://github.com/vuejs/v2.vuejs.org/blob/master/writing-guide.md) and our own learnings from the process of writing documentation.
We've copied many of the points (mostly) verbatim, removed some that don't apply to Wasp, and added some that do.
We've also changed examples to be more relevant for Wasp.

## A word of caution

Writing the docs will take longer than you expect (even if you expect it will take a long time).
To make the process as painless as possible, read this guide thoroughly(with particular emphasis on the [Processes section](#processes)).

## Principles

- **A feature doesn't exist until it's well-documented.**
- **Respect users' cognitive capacity (i.e., brain power).** When a user starts reading, they begin with a certain amount of limited brain power. When they run out of brain power, they stop learning.
  - Cognitive capacity is **depleted faster** by complex sentences, having to learn more than one concept at a time, and abstract examples that don't directly relate to a user's work.
  - Cognitive capacity is **depleted more slowly** when we help them feel consistently smart, powerful, and curious. Breaking things down into digestible pieces and minding the flow of the document can help keep them in this state.
- **Always try to see things from the user's perspective.** When we understand something thoroughly, it becomes obvious to us. This is called _the curse of knowledge_. In order to write good documentation, try to remember what you first needed to know when learning this concept. What jargon did you need to learn? What did you misunderstand? What took a long time to really grasp? Good documentation meets users where they are. It can be helpful to practice explaining the concept to people in person before and during the writing process.
- **Describe the _problem_ first, then the solution.** Before showing how a feature works, it's important to explain why it exists. Otherwise, users won't have the context to know if this information is important to them (is it a problem they experience?) or what prior knowledge/experience to connect it to.

## Organization

### Organization of pages

- **Getting Started**:
  - **Introduction** - Provides a less than 10-minute overview of the problems Wasp solves and explains why it exists.
  - **Quick Start** - Provides short (less than 5 minutes) instructions on installing Wasp and starting the demo app. At the end, it links to the tutorial and the rest of our resources (Discord, editor setup, newsletter)
  - **Editor Setup** - Provides short instructions (less than 5 minutes) on how to get the most out of Wasp in your editor, and which editors we currently support.
- **Tutorial**: Takes users through the journey of building a simple application in Wasp from scratch.
  The goal is to make users feel smart, powerful, and curious.
  Tutorial pages are meant to be read sequentially. Their order depends on how the implemented features depend on each other (e.g., to query the database, the user must first define database models).
- **Feature pages**: Explores all of Wasp's features.
  The features are divided into several sections and don't need to be read sequentially.
  See [Organization inside a page](#organization-inside-a-page) to see how to organize each page.
  - **Data model**: Goes deeper into the Wasp's central feature - its data model (Entities, Actions, and Queries).
  - **Authentication**: Covers everything there is to know about authentication in Wasp.
  - **Project setup**: Explains how to customize and configure Wasp projects, how to run tests, how to set environment variables, etc. It goes into everything you could run into while building a project besides programming.
- **Advanced Features**: Describes all the remaining features. This section also follows the rules from [Organization inside a page](#organization-inside-a-page).
  These are either features that most small apps won't need but are bound to come up in production-ready projects (e.g., deployment, recurring jobs, sending emails) or features useful for apps of all sizes that require more skill/familiarity with Wasp or TypeScript (e.g., Type-safe links).
- **General**: Includes an overview of the Wasp language and the CLI Reference.
- **Miscellaneous**: Talks about our vision, ways to contribute, the data we collect, and how to contact us.

### Organization inside a page

Each feature page is divided into two sections: **The guide**, and **The API reference**.

#### Guide

The guide tells a story about a feature and takes the reader through a step-by-step process to get the feature working.
It goes through the feature's most important parts and does not have to be exhaustive. The goal is to provide 20% of the knowledge that helps users handle 80% of use cases.

The guide is almost a tutorial, the only difference being that it can assume some context for the rest of the application.
The text can link to parts of the API reference, but you should avoid such links in most cases.
When you provide them, you also need to provide a context so users know whether they should follow this link on their first reading.
Otherwise, many users end up exhausting their cognitive capacity link-hopping, trying to fully learn every aspect of a feature before moving on, and as a result, never finish that first read-through of the guide.
Remember that a smooth read is more important than being thorough.
We want to give people the information they need to avoid a frustrating experience, but they can always come back and read further, or Google a less common problem when they encounter it.

#### API reference

The API reference is an exhaustive list of the feature's API and must describe everything:

1. The Wasp API (e.g., a declaration, mandatory and optional fields)
2. The JavaScript API (e.g., imports, available functions, arguments, etc.)
3. The CLI (i.e. CLI commands, their arguments, and usage examples)

#### General

Both the guide and the API Reference should be self-sufficient and contain examples showcasing the feature.
Always assume that the reader is only reading one or the other.
The guide doesn't need to explain everything about the feature, only the most important bits, but the API reference must be exhaustive.

Make sure to feature each example in all languages Wasp supports (currently TypeScript and JavaScript) using tabs.
You should almost always use tabs, even when there's no difference between the example in TypeScript and the example in JavaScript.
This might seem redundant, but it makes our examples future-proof and helps reassure the reader we haven't forgotten about their language.

## Grammar and writing style

### Style

- **Headings should describe problems**, not solutions. For example, a less effective heading might be "The `useQuery` hook" because it describes a solution.
  A better heading might be "Making Query data reactive" because it provides the context of the problem the `useQuery` hook solves.
  Users won't start paying attention to the explanation of a feature until they have some idea of why/when they'd use it.
- **When you assume knowledge, declare it** at the beginning and link to resources for the knowledge you expect.
- **Introduce only one new concept at a time whenever possible** (including both text and code examples). Some (maybe even many) people will be able to understand multiple concepts at once. Still, many will also become lost - and even those who don't become lost will have depleted more of their cognitive capacity.
- **Avoid special content blocks for tips and caveats when possible.** It's generally preferable to blend these more naturally into the main content (e.g., by building on examples to demonstrate an edge case).
- **Don't include more than two interwoven tips and caveats per page.** If you find that more than two tips are needed on a page, consider adding a caveats section to address these issues. The guide is meant to be read straight through, and tips and caveats can overwhelm or distract someone trying to understand the base concepts.
- **Avoid appeals to authority** (e.g., "You should do X because that's a best practice" or "X is best because it gives you full separation of concerns"). Instead, demonstrate with examples the specific human problems caused and/or solved by a pattern.
- **When deciding what to teach first, think of what knowledge provides the best power/effort ratio.** That means teaching whatever will help users solve the greatest pains or greatest number of problems, with the relatively least effort to learn. This helps learners feel smart, powerful, and curious, so their cognitive capacity will drain more slowly.
- **Show, don't tell.** For example, "You can make the data a Query returns reactive using the `useQuery` hook" (then show import and usage of the `useQuery` hook), instead of "To make the data Query returns reactive, you can pass it into the `useQuery` hook and destructure the `data` field from the returned object."
- **Almost always avoid humor**, especially sarcasm and pop culture references, as it doesn't translate well across cultures.
- **Never assume a more advanced context than you have to.**
- **In most cases, prefer links between sections of the docs over repeating the same content in multiple sections.** Some repetition in content is unavoidable and even essential for learning. However, too much repetition also makes the docs more difficult to maintain because a change in the API will require changes in many places, making it easy to miss something. This is a difficult balance to strike.
- **Specific is better than generic.** For example, a `<BlogPost>` component example is better than `<ComponentA>`.
- **Relatable is better than obscure.** For example, a `<BlogPost>` component example is better than `<CurrencyExchangeSettings>`.
- **Be emotionally relevant.** Explanations and examples that relate to something people have experience with and care about will always be more effective.
- **Always prefer simpler language over complex or jargony language.** For example:
  - "You can begin defining an Action by declaring it in Wasp." instead of "In order to define an Action, it must first be declared via a Wasp declaration."
  - "function that returns a function" instead of "higher order function" (while technically correct and more concise, higher order function requires knowledge and context the reader doesn't necessarily have)
- **Avoid language that invalidates struggle**, such as "easy", "just", "obviously", etc. For reference, see [Words To Avoid in Educational Writing](https://css-tricks.com/words-avoid-educational-writing/).

### Grammar

- **Don't use emojis (except in discussions).** Emojis are cute and friendly, but they can be a distraction in documentation. Some emojis even convey different meanings in different cultures. They also make the documentation seem unprofessional and of lower quality.
- **Don't use memes and funny pictures.** Everything said about emojis applies to memes as well. It's hard to focus on the text and take it seriously when it contains jokes and memes.
- **Avoid passive voice.** Instead of "The Wasp app can be deployed...", write "You can deploy the Wasp app..."
- **Avoid abbreviations** in writing and code examples (e.g., `attribute` is better than `attr`, `message` is better than `msg`), unless you want to specifically reference an abbreviation in the API (e.g., the `auth` declaration). Abbreviation symbols included on standard keyboards (e.g., `@`, `#`, `&`) are OK.
- **Avoid too many exclamation points.** False excitement can alienate readers and also make the docs seem less professional.
- **Address the reader directly.** Instead of "We can implement a Query..." or "A Query can be implemented..." or "The user can implement a Query...", say "You can implement a Query..."
  The docs should speak to the reader and address them directly to avoid possible confusion (e.g., Who are we? The Wasp team? The reader together with the Wasp team? etc.).
  Use the word _user_ only to refer to the user of the software that your reader is developing.
  You can sometimes use first-person plural pronouns to refer to the organization (Wasp). For example, "We support both TypeScript and JavaScript" is OK, but "Wasp supports both TypeScript and JavaScript" is usually better.
- **Avoid using too many pronouns.** Whenever possible, try to call a thing by its name instead of relying on a previous context. This will sometimes sound strange (it's OK to use pronouns in those cases), but it should work most of the time.
- **When referencing a directly following example, use a colon (`:`) to end a sentence**, rather than a period (`.`).
- **When referencing the name of a project, prioritize the broader conventions of English over internal branding conventions of that project.** For example, "webpack" and "npm" both disregard conventions such as "always start a word at the beginning of a sentence with a capital letter," "project names always use Title Case," and "acronyms are always capitalized." Instead, always write "Webpack and NPM" to provide a more consistent experience in the docs and avoid sentences like "If you don't want to use Vue CLI, you can use webpack or Rollup directly by installing them via npm or Yarn."
- **Do not use Title Case for headings** - There's research suggesting that sentence case (only the first word of the heading starts with a capital) is superior for legibility and also reduces the cognitive overhead for documentation writers since they don't have to try to remember whether to capitalize words like "and", "with", and "about."
  Many of our titles are currently in title-case, we should start phasing those out.
- **Use the Oxford comma** (e.g., "a, b, and c" instead of "a, b and c"). ![Why the Oxford comma is important](https://raw.githubusercontent.com/vuejs/v2.vuejs.org/master/src/images/oxford-comma.jpg)

## Content and Communication

- **Excellence comes from iteration.** First drafts are always bad, but writing them is a vital part of the process. It's extremely difficult to avoid the slow progression of Bad -> OK -> Good -> Great -> Inspiring -> Transcendent.
- **Only wait until something is "Good" before publishing.** Vue's guide originally says: "The community will help you push it further down the chain." We don't yet have that luxury, as our community isn't large enough. Still, we can't afford to invest _too much_ time into the docs, so "Good" will have to do for now.

## Processes

- **Ideally, you should write the docs before you implement the feature.** This will help you see the feature from the user's perspective and better spot the API's deficiencies and improvement potential. If something is difficult to explain, it's most likely difficult to understand. If it is difficult to understand, there might be a better way of designing it.
- **Try not to get defensive when receiving feedback.** Our writing can be very personal to us, but if we get upset with the people who help us improve it, they will either stop giving feedback or start limiting the kind of feedback they give.
- **Proofread your work before showing it to others (and use Grammarly).** If you show someone work with many spelling/grammar mistakes, you'll get feedback about spelling grammar/mistakes instead of more valuable notes about whether the writing is achieving your goals.
- **When you ask people for feedback, tell reviewers:**
  - **What you're trying to do.**
  - **What your fears are.**
  - **Which balances you're trying to strike.**
- **Do your best to come up with a good and straightforward way to say something.** Again, this will help the reviewer focus on high-level issues instead of rephrasing your sentences.
- **Read and correct your text several times before submitting it (preferably with some time between the readings).** This is similar to proofreading but has more to do with content and communication style than grammar.
  A time offset is beneficial because it removes the text from your short-term memory, helping you view it more objectively.
- **It's OK to ask AI to improve your text.** Just make sure to check it and correct it. You should always sign off on the last version.
- **When someone reports a problem, there is almost always a problem**, even if the solution they proposed isn't quite right. Keep asking follow-up questions to learn more.
- People need to feel safe asking questions when contributing/reviewing content. Here's how you can do that:
  - **Thank people for their contributions/reviews, even if you're feeling grumpy.** For example:
    - "Great question!"
    - "Thanks for taking the time to explain. 🙂"
    - "This is actually intentional, but thanks for taking the time to contribute. 😊"
  - **Listen to what people are saying and mirror if you're not sure you're understanding correctly.** This can help validate people's feelings and experiences while also understanding if _you're_ understanding _them_ correctly.
  - **Use a lot of positive and empathetic emojis.** It's always better to seem a little strange than mean or impatient.
    This primarily applies to Wasp team members speaking to outside contributors. Since most of the core team knows each other pretty well, there's no need to go overboard with the emojis and pleasantries.
  - **Kindly communicate rules/boundaries.** If someone behaves in a way that's abusive/inappropriate, respond only with kindness and maturity, but also make it clear that this behavior is not acceptable and what will happen (according to the code of conduct) if they continue behaving poorly.
- **All docs must go through the review cycle, preferably with more than a single reviewer.** Different people focus on different things. Some of us are great at coming up with examples, others easily come up with analogies and explain complex topics, some have a clear and concise writing style, etc. Therefore, try to get at least two or three people to review your document.

## Linking to pages in the docs

Always use relative links (e.g. `../../overview.md`) to link to other pages, unless you are writing a reusable snippet.

Never use absolute links starting with `/docs` because they break our versioned docs, instead use links "absolute to the file root".

Writing a link "absolute to the file root":
1. Write an absolute link, start from the file root (e.g. `/` represents the `docs` folder)
2. Include the extension (e.g. `.md`)

For example, `/docs/introduction` should be written as `/introduction/introduction.md` because this file is located at `./docs/introduction/introduction.md`.

Or another example `/docs/auth/entities#accessing-the-auth-fields` becomes `/auth/entities/entities.md#accessing-the-auth-fields`. This file is located at `./docs/auth/entities/entities.md`.

## Possible improvements

- Some parts of our docs don't follow all the guidelines outlined in this document. There's no need to start fixing all the issues right away. We can slowly improve the docs as we edit them.
- We've discussed having a git repo with all the example code in the docs. This should make copying, pasting, testing, and maintaining code snippets easier.
