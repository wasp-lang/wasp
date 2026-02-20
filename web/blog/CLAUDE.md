# Wasp.sh Blog
This is the blog for Wasp.sh (batteries-included full-stack framework for React, NodeJS, and Prisma apps), built with Docusaurus.

## Blogpost Workflow

### 1. Blogpost Draft Planning
1. When drafting blogposts, first use the AskUserQuestion tool to clarify the following with the user:
  - the topic of the article
  - the [article type](#article-length-guidelines)
  - what research should be done, if necessary
  - if the article is to be optimized for GEO and SEO, and if so, what keywords to target
  - if there are any references articles to use as examples (either local files or by URL)
  - if there are any specific sections or considerations to be included in the article

2. Write the plan along with an outline of the article to a file called `blogpost-draft-plan.md`.
3. Ask the user to review the plan and make any necessary changes.

### 2. Blogpost Drafting
1. Suggest that the user spawn sub-agents to take care of pre-writing tasks in parallel, e.g. researching then reviewing the accuracy of the research.
2. When ready, create a draft of the article (`<YYYY>-<MM>-<DD>-<title>-<slug>.mdx`) while considering:
  - the `blogpost-draft-plan.md` file
  - the [Wasp Blogpost Style Guide](#blogpost-style-guide)

### 3. Blogpost Draft Reviewing
**General draft review:**
1. Spawn a sub-agent to review the draft for readability, coherence, and adherence to the [Wasp Blogpost Style Guide](#blogpost-style-guide) and the original `blogpost-draft-plan.md`.
2. If necessary, have the user amend the proposed revisions before making further changes to the draft.

**GEO-friendly review:**
1. Build the draft into a full HTML page in the `build/blog` directory by running `npm run build` in the `../web` directory.
2. Instruct the user to run the `/geo-fundamentals` skill on the draft to check if it is GEO-friendly.
4. If necessary, have the user amend the proposed revisions before making further changes to the draft.

### 4. Blog Crossposting
- Invoke the `/crossposting` skill to crosspost the Wasp blog article to popular blogging platforms.

### 5. Creating Social Media Content
- Inform the user to create social media posts (tweets, linkedin posts, etc.) for the article by invoking the `/social-content` skill.


## Blogpost Style Guide

This style guide is derived from patterns observed across 80+ blog posts spanning 2019-2026, written by 12+ authors.

### Tone, Voice, and Structure

- **Casual, developer-peer-to-peer.** Friendly and approachable, not corporate.
- **Paragraphs:** keep them short — 2-4 sentences. Break longer explanatory content into bullet points or numbered lists before it exceeds 5-6 lines.
- **Contractions are standard:** "it's", "you'll", "we're", "don't" — always use them.
- **Second person ("you")** for tutorials: "Let's jump right in.", "Now you'll create..."
- **First person plural ("we")** for Wasp team posts: announcements, roadmaps, year reviews.
- **First person singular ("I")** for guest posts and personal opinion pieces.
- **TL;DR section** for tutorials, technical long-form: `## TL;DR` as the first heading, summarizing key points. Great for GEO.
- Casual asides and parentheticals are welcome: "(we'll explain this in more detail later)"
- Light humor, memes, and GIF references are encouraged in announcements and community posts. Keep technical posts (security, migrations) more measured.

### Wasp Promotion

Honest promotion is encouraged when Wasp is relevant to the article. While our purpose is to market Wasp, we want it to be honest and fair in its assessment. We want to be transparent about our bias and not deceive the reader. The level of promotion should also be appropriate for the post type.

**Opinion/technical posts:**
- Brief disclosure if Wasp is mentioned: "(disclaimer: this is us!)"
- Light promotional mention in closing, if relevant
- No hard sell

**Case studies:**
- Weave Wasp mentions naturally through the customer's story
- End with CTA

### Frontmatter

Every post **must** include:

```yaml
---
title: "Post Title Here"
authors: [authorHandle] # array of handles from authors.yml
image: /img/<topic-slug>/banner.webp
tags: [wasp, web dev, relevant, tags] # 3-6 lowercase tags
keywords: ["long-tail keyword 1", "keyword 2", ...] # 10+ for SEO/GEO
description: "SEO/GEO-optimized summary of the article."
---
```

Optional fields:
```yaml
last_update:
  date: "YYYY-MM-DD"  # use when significantly updating an existing post
```

### Truncate Marker (Required)

Every post **must** include a truncate marker

```mdx
Opening paragraph that hooks the reader and stands alone as an excerpt.

{/* truncate */}
```

### CTA Resources

- Install the Wasp CLI globally: `npm i -g @wasp.sh/wasp-cli`
- GitHub: https://github.com/wasp-lang/wasp
- Discord: https://discord.gg/rzdnErX

### Article Length Guidelines

| Post Type | Target Length |
|---|---|
| Team introductions | 500-800 words |
| Announcements / launch weeks | 500-800 words |
| Opinion / think pieces | 600-1,000 words |
| Case studies | 700-1,000 words |
| Short tutorials | 800-1,500 words |
| Comparison articles | 2,000-3,000 words |
| Technical explainers | 2,000-3,000 words |
| Deep-dive coding tutorials | 3,000-6,000+ words |
