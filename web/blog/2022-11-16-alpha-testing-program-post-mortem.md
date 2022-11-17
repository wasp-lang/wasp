---
title: 'Alpha Testing Program: post-mortem'
authors: [matijasos]
image: /img/farnance/farnance-hero-shot.png
tags: [webdev, wasp, startups, github]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

We are working on a new web framework that integrates with React & Node.js, and also happens to be a language. As you can probably imagine, it’s not easy to get people to use a new piece of technology, especially while still in Alpha. On the other hand, without users and their feedback, it’s impossible to know what to build.

That is why we ran Alpha Testing Program for Wasp - here is what we learned and what went both well and wrong along the way.

<ImgWithCaption
    alt="twitter DM - shared atp in swag groups"
    source="img/atp/swag-groups-twitter.png"
/>

<!--truncate-->

## “Of course I know about Wasp! I just haven’t come around to trying it out yet.”

Although we hit the front page of HN [several](https://news.ycombinator.com/item?id=26091956) [times](https://news.ycombinator.com/item?id=32098144) and are about to reach 2,000 stars on GitHub, there is still a big difference between a person starring a repo and actually sitting down and building something with it.

Talking to people, we realised a lot of them had heard of Wasp, thought it was a neat idea, but hadn’t tried it out. These were the main reasons:

- having to find 30 mins to go through our **Build a Todo App** tutorial - *“I'm busy now, but I’ll do it next week.”*
- building a bare-bones todo app is not that exciting
- not having an idea what else to build
- *“the product is still in alpha, so I will bookmark it for later”*

These are all obvious and understandable reasons. I must admit, I’m much the same — maybe even worse — when it comes to trying out something new/unproven. It just isn’t a priority, and without a push that will help me overcome all these objections, I usually don’t have an incentive to go through with it.

Having realised all that, we understood we needed to give people a reason to try Wasp out **now**, because that’s when we needed the feedback, not next week.

## Welcome to Wasp Alpha Testing Program!

<p align="center">
  <figure>
    <img alt="The team"
        src={useBaseUrl('img/atp/welcome-to-atp-notion.png')}
    />
    <figcaption style={{color: '#808080'}}>I was having a bit too much fun <Link to={useBaseUrl('https://wasp-lang.notion.site/CLOSED-Welcome-to-Wasp-Alpha-Testing-program-f3a8a350802341abac87fb7831bb1e60')}>here</Link>, but Portal fans will understand.</figcaption>
  </figure>
</p>

We quickly put together an admissions page for alpha testers in Notion (you can see it [here](https://wasp-lang.notion.site/Wasp-Alpha-Testing-Program-Admissions-dca25649d63849cb8dfc55881e4f6f82)) and started sharing it around. To counter the hurdles we mentioned above, we time-boxed the program (*”this is happening now and you have 48 hours to finish once you start*”) and promised a t-shirt to everyone that goes through the tutorial and fills out the feedback form.

<ImgWithCaption
    alt="Apply to ATP - CTA"
    source="img/atp/atp-apply-here.png"
    caption="CTA from the admissions page"
/>

Soon, the first applications started trickling in! For each new applicant, we’d follow up with the [instructions](https://www.notion.so/CLOSED-Welcome-to-Wasp-Alpha-Testing-program-f3a8a350802341abac87fb7831bb1e60) on how to successfully go through the Alpha Testing Program:

- fill out intro form (years of experience, preferred stack, etc)
- go through our “build a Todo app” tutorial
- fill out the feedback form - what was good, what was bad etc.

<ImgWithCaption
    alt="Timeboxing"
    source="img/atp/timeboxing.png"
    caption="People were really respectful of this deadline and would politely ask to extend it in case they couldn’t make it."
/>

But, soon after I got the following message on Twitter:

<ImgWithCaption
    alt="twitter DM - shared atp in swag groups"
    source="img/atp/swag-groups-twitter.png"
/>

We got really scared that we would get a ton of folks putting in minimal effort while trying Wasp out just to get the free swag, leaving us empty-handed and having learned nothing! On the other hand, we didn’t have much choice since we didn’t define the “minimum required quality” of feedback in advance.

Luckily, it wasn’t the problem in the end, even the opposite -- we did get a surge of applications, but only a portion of them finished the program and the ones that did left really high-quality feedback!

## How it went - test profile & feedback

### Tester profile

We received 210 applications and 53 out of those completed the program — 25% completion rate.

We also surveyed applicants about their preferred stack, years of programming experience, etc:

<ImgWithCaption
    alt="Intro survey - tester profile"
    source="img/atp/atp-intro-survey-yoe.png"
    caption="Yep, we like puns."
/>

### The feedback

The feedback form evaluated testers’ overall experience with Wasp. We asked them what they found to be the best and worst parts of working with Wasp, as well as about the next features they’d like to see.

<ImgWithCaption
    alt="Feedback survey - experience"
    source="img/atp/atp-feedback-survey-exp.png"
/>

**The bad parts**

What our testers were missing the most was a full-blown IDE and TypeScript support. Both of these are coming in Beta but only JS was supported at the time. Plus, there were some installation problems with Windows (which is not fully supported yet — best to use it through WSL).

<ImgWithCaption
    alt="Feedback survey - the bad parts"
    source="img/atp/atp-bad-parts.png"
/>

We were already aware that TypeScript support is an important feature, but didn’t have an exact feeling of how much - the feedback was really helpful and helped us prioritise our Beta backlog.

**The good parts**

Testers’ favourite part was the batteries-included experience, particularly the [auth model](/docs/tutorials/todo-app/06-auth).

<ImgWithCaption
    alt="Feedback survey - the good parts"
    source="img/atp/atp-good-parts.png"
/>

## Post-mortem: what didn’t go well

### No threshold for feedback quality

<ImgWithCaption
    alt="Feedback quality"
    source="img/atp/atp-feedback-quality.png"
/>

We didn’t put any kind of restrictions on the feedback form, e.g. minimal length of the feedback. That resulted in ~15%-20% of answers being single words, such as depicted above. I’m not sure if there is an efficient way to avoid this or just a stat to live with.

### Using free text form for collecting addresses

It never crossed our minds before that validating addresses could be such an important part of shipping swag, but turns out it is. It seems that there are a lot of ways to specify an address, some of which are different from what is expected by our post office, resulting in a number of shipments getting returned.

An ideal solution would be to use a specialized “address” field in a survey that would auto-validate it, but turns out Typeform (which we used) doesn’t have that feature implemented yet, although [it’s been highly requested](https://community.typeform.com/suggestions-feedback-34/address-field-question-type-2950).

<ImgWithCaption
    alt="Shipment returned"
    source="img/atp/atp-shipment-returned.jpg"
/>

<ImgWithCaption
    alt="Shipment returned email"
    source="img/atp/atp-shipment-returned-email.png"
/>

## The non-obvious benefit of Alpha Testing Program

What went well is that we got a lot of high-quality feedback that steered and fortified our plan for the upcoming Beta release.

The other big benefit is that we finally solved the *“looks cool but i’ll try it out later maybe”* problem. Overall, our usage went well up during the program, but even after it ended, the baseline increased significantly. This was the second-order effect we didn’t foresee.

Our understanding is that once people finally gave it a try, a portion of them felt the value first-hand and decided to keep using it for other projects as well.

<ImgWithCaption
    alt="Alpha testing program - usage spike"
    source="img/atp/atp-usage-spike.png"
/>

## Summary & going forward: Beta

The overall conclusion from our Alpha Testing Program is it was a worthy effort which got us valuable feedback and positively affected the overall usage. Moving forward we’ll try to focus on ensuring more quality feedback and prioritising 1-to-1 communication to make sure we fully understand what bothers Wasp users and what we can improve. It also might be helpful to do testing in smaller batches so we are not overwhelmed with responses and can focus on the individual testers - that’s something we might try out in Beta.

As mentioned, the next stop is Beta! It comes out on the 27th of November - [sign up here](/#signup) to get notified.
