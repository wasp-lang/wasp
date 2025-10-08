---
title: 'Building and Selling a GPT Wrapper SaaS in 5 Months'
authors: [vinny]
image: /img/selling-saas.png
tags: [saas, boilerplate, acquire, react, gpt, openai, marketing]
---

Since the release of ChatGPT, we’ve been flooded with all possible versions of apps that use it in one way or another. Building on top of trendy technology is an excellent way to get initial attention, but still, 99% of these apps die very quickly and don’t last beyond a week or two following their “big” Twitter or Product Hunt launch.

{/* truncate */}

Why? **Because they aren’t solving a real problem**. It’s either a fun tech gadget or a gross overpromise (e.g., _“you will never need to code again,”_ which [I strongly disagree with](https://wasp.sh/blog/2022/06/24/ML-code-gen-vs-coding-by-hand-future)) that quickly falls short.

Building a successful product still follows the same rules as in the pre-GPT era: **find a problem people are willing to pay for and then figure out a way to reach these people**. Sounds simple? It is, but it for sure isn’t easy. The good news is that GPT opened so many new opportunities that actually doing it is faster and easier than ever.

## Meet the hero of our story - Max! 🦸

![our-hero-max](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/rowl3gqdrb2nhhdl1hyo.gif)

The hero of our story today is Max, a software engineer at Red Hat. He built [https://description-generator.online](https://description-generator.online/) (an AI description generator for Etsy products) and sold it on [acquire.com](http://acquire.com). A senior backend engineer by day and a serial hacker and tinkerer by night, Max always had a passion for building products, and GPT was the last piece of the puzzle he was waiting for.

Read on to learn how he went through the entire cycle of finding a problem, building a solution, getting customers, and ultimately selling his app in 5 months total.

## Lesson #1: Look for problems in “unusual” places 🕵️‍♂️

![Looking for problems](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/kz3nsmm5zheyjukvr6j8.gif)

_TL;DR: Talk to your friends who aren’t developers! Learn about their problems and offer help. The more unfamiliar and disconnected from tech their occupation is, the better - these are gold mines for GPT-powered solutions!_

It all started with Max’s friend who owns an Etsy marketplace - she needed help with some data/workflow automation, and Max agreed to lend a hand. Consequently, he also started hanging out in the Ukranian Etsy community on Slack.

Soon, he learned that one of the most common requests there is for help with writing product descriptions (”listings”) in English. Although most members used English daily and had no problem communicating, writing high-quality, compelling, and professional-sounding listings was still a challenge. Auto-translation services still weren’t sophisticated enough, and hiring native English speakers was too expensive for most.

This sounded like a real, glaring problem directly connected to the number of items Etsy sellers sell and, thus, the profit they make. As it turned out, it was the case.

## Lesson #2: Build a prototype, fast 🏎️

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/6ign7kz6464wnuac13y8.gif)

_TL;DR: Speed is the name of the game here. Don’t spend time flexing on your stack and optimizing to the last byte. Pick something that works and ship it!_

The problem of writing convincing product listings in English caught Max’s attention. He was aware of ChatGPT and how useful it could be for this. However, being a backend engineer with limited frontend experience, building a full-stack app around it and choosing and configuring all parts of the stack himself sounded daunting and laborious. It wasn’t until he came across [Open SaaS](https://opensaas.sh) that he felt ready to take action.

The prototype was ready after a couple of days, and Max immediately shared it with his Etsy community.  He kept it extremely simple - no landing page or any copy at all (just a form to enter your product details), even no custom domain yet, but myProduct.fly.io you get assigned upon deploying to Fly (which takes just a single CLI command with Wasp).

And that was enough - as his product scratched the itch Etsy sellers repeatedly mentioned, the reception was overwhelmingly positive! In just a few days, Max got 400 signups, and several hundred product listings were generated daily.

---

By the way, if you’re looking for an easy, low maintenance way to start your next side project, check out [Open SaaS](https://opensaas.sh/), a 100% free, open-source Saas Starter!

![https://dev-to-uploads.s3.amazonaws.com/uploads/articles/sf1fhsgwuurkre9a7drq.png](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/sf1fhsgwuurkre9a7drq.png)

Open SaaS is a feature-rich, React + NodeJS SaaS template, with Stripe, OpenAI / GPT app examples, AWS S3 file upload, Analytics, Admin Dashboard, and full Documentation!

---

## Lesson #3: Test willingness to pay early 💸

![money please](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/v6rcbozh1euokgyvgv1g.gif)

_TL;DR: People signing up for your product is amazing, but convincing them to pay is a completely separate game. If you want to ensure your solution brings real value and you’re not wasting time, find a way to test monetizing as early as possible._

Max saw the adoption picking up, which made him ask himself _“How do I turn this into a business? What would users be willing to pay for?”_ After all, he had his own expenses, like server costs and GPT API subscription.

Looking at how users use the product, he quickly realized he could make generating descriptions even easier - a seller could upload the image of a product, and that’s it; the full product description can be generated directly from it. That was a good candidate for a “premium” feature, since it was an upgrade on top of the basic functionality.

Max added the feature, and soon enough, the first customers started rolling in! 💰

## Lesson #4: Keep building or sell? How to decide 🤔

![homer selling](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/sm55hit6negsr6emmgrg.gif)

_TL;DR: Is the market’s domain something you’re personally excited about and see yourself in for the long term? Do you feel your competitive advantage will grow stronger with time? If yes, keep building. Otherwise, sell!_

[description-generator.online](http://Decision-generator.online) now had both users and first revenue, amazing! Still, soon, it became apparent that the Etsy community Max was part of had its limits. Although all non-English speaking markets shared the problem, which made for a big opportunity, reaching them and setting up and executing a sales process would still take time and effort.

On the other hand, competing products started appearing. Although super valuable for Etsy sellers, if Max built the product in a week, others could do it too. It started becoming clear that the value of the business would soon start moving from the technical solution to sales, support, and customer experience.

Being a hacker at heart and not so personally invested in arts & crafts marketplaces, Max decided to sell the product to somebody who is. He listed the description generator on https://acquire.com/, along with the usage metrics and relevant data, and soon started receiving offers.

## Lesson #5: Provide support during acquisition 🤝

![got my back](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/i6ay41rsjabkv9mkqcto.gif)

_TL;DR: Selling your product takes more than finding a buyer. Providing impeccable support during acquisition is just as important as building the product._

Finding a buyer and agreeing on a price took about a month. Since the buyer was taking over everything - the source code, domain, and customers, Max providing 3-month support with the transition was an essential part of the deal.

Also, since they couldn’t use an escrow service due to some technical and geographical limitations, they agreed on splitting the payment 50/50 - half in the beginning and another half when the migration was over. Max made sure his customers had a flawless experience with moving everything over, resulting in a great relationship mutually filled with trust. Besides selling your app, making friends is an underrated bonus! 😎

After a few months, the deal has been reached! [Description-generator.online](http://description-generator.online/) got a new owner, an expert in the industry willing to expand to new markets, and Max got his first exit and could move on to the next exciting project!

## Summary

![michael summary](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/l6zioe34s0u9ic80g26k.gif)

That’s it! Building a product others find helpful so much they’re willing to pay for it is a deeply gratifying experience. We saw how Max did it and what lessons he learned along the way:

1. Look for problems in “unusual” places
2. Build a prototype fast
3. Test willingness to pay early
4. Decide whether you want to keep building or sell
5. Provide support during the acquisition

Hopefully, this was helpful!
