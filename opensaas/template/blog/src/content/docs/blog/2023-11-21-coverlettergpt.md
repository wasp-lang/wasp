---
title: How I Built & Grew CoverLetterGPT to 5,000 Users and $200 MRR
date: 2023-11-21
tags: ["indiehacker", "saas", "sideproject"] 
subtitle: A guide to building a profitable, open-source side-project
hideBannerImage: false # Banner images stored in public/banner-images/ are automatically used as cover images and social media preview images (og:image) for each blog post.
authors:
  - name: vince
    title: Dev Rel @ Wasp
    url: https://wasp.sh
---
## Hey, I’m Vince…

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/az8xf61b2qxx1msfo4t5.png)

I’m a self-taught developer that changed careers during the Covid Pandemic. I did it because I wanted a better career, enjoyed programming, and at the same time, had a keen interest in IndieHacking.
<!--truncate-->
If you’re not aware, IndieHacking is the movement of developers who build potentially profitable side-projects in their spare time. And there are some very successful examples of IndieHackers and “solopreneurs” out there inspiring others, such as [levels.io](http://levels.io) and [Marc Lou](https://twitter.com/marc_louvion). 

This thought of being able to build my own side-project that could generate profit while I slept was always attractive to me.

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/e1r07ajn3gysdscjdkns.png)

So I’m happy to report that I’ve finally done it with my first software-as-a-service (SaaS) app, [CoverLetterGPT.xyz](http://CoverLetterGPT.xyz), which I launched in March 2023!

I’ll be the first to admit that the results aren’t spectacular, but they’re still something I’m very proud of: 

- over 5,000 registered users
- $203 monthly recurring revenue (MRR)

Below, I’m going to share with you how I built it (yes, it’s [open-source](https://github.com/vincanger/coverlettergpt)!), how I marketed and monetized it, along with a bunch of helpful resources to help you build your own profitable side-project.

## What the heck is CoverLetterGPT?

[CoverLetterGPT.xyz](http://CoverLetterGPT.xyz) was an idea I got after the OpenAI API was released. It’s an app that allows you to upload a PDF of your CV/resumé, along with the job description you’re applying to, and it will generate and edit unique cover letters for you based on this information.

{% embed https://youtu.be/ZhcFRD9cVrI %}

It also lets you save and manage your cover letters per each job, making it easy to make and apply to multiple jobs without having to keep copy and pasting all your important info into ChatGPT! 

## What’s the Tech Stack?

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/xpb97bgrx98bwxemrg0o.png)

CoverLetterGPT is entirely open-source, so you can [check out the code](https://github.com/vincanger/coverlettergpt), fork it, learn from it, make your own, submit a PR (I’d love you forever if you did 🙂)… whatever!

I built it using the [Wasp full-stack framework](https://wasp.sh) which allowed me to ship it about 10x faster. 

Why? 

Because [Wasp](https://wasp.sh) as a framework allows you to describe your app’s core features in a `main.wasp` config file. Then it continually compiles and “glues” these features into a React-ExpressJS-Prisma full-stack app for you. 

All you have to focus on is writing the client and server-side logic, and Wasp will do the boring stuff for you, like authentication & authorization, server config, email sending, and cron jobs. 

BTW, [Wasp](https://wasp.sh) is open-source and free and you can help the project out a ton by starring the repo on GitHub: [https://www.github.com/wasp-lang/wasp](https://www.github.com/wasp-lang/wasp) 🙏

![https://media1.giphy.com/media/ZfK4cXKJTTay1Ava29/giphy.gif?cid=7941fdc6pmqo30ll0e4rzdiisbtagx97sx5t0znx4lk0auju&ep=v1_gifs_search&rid=giphy.gif&ct=g](https://media1.giphy.com/media/ZfK4cXKJTTay1Ava29/giphy.gif?cid=7941fdc6pmqo30ll0e4rzdiisbtagx97sx5t0znx4lk0auju&ep=v1_gifs_search&rid=giphy.gif&ct=g)

{% cta [https://www.github.com/wasp-lang/wasp](https://www.github.com/wasp-lang/wasp) %} ⭐️ Thanks For Your Support 🙏  {% endcta %}

For the UI, I used [Chakra UI](https://chakra-ui.com/), as I always do. I like that it’s a component-based UI library. This helps me build UI’s a lot faster than I would with Tailwind or vanilla CSS.

For payments, I used [Stripe](https://www.notion.so/How-I-Built-and-Open-Sourced-CoverLetterGPT-5-000-users-200-MRR-0d32f13fa00a440fb8e08c8dbf2b8a27?pvs=21), (I’ll go into the details of monetization below). 

The Server and Postgres Database are hosted on [https://railway.app](https://railway.app/), with the client on [Netlify.com](http://Netlify.com)’s free tier.

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/09ca1yaqodkb7b2vnwr9.png)

By the way, If you’re interested in building your own SaaS with almost the same stack as above, I also built a [free SaaS template](https://github.com/wasp-lang/SaaS-Template-GPT) you can use that will save you days of work!  

## How I Marketed It

My biggest take-away from this whole project was that open-sourcing it was the best way to market it!

This seems counter-intuitive, right? Why would making the code available for anyone to see and copy be good for a business? You’re basically rolling out a red carpet for competitors, aren’t you?

Well, not quite.

First of all, the number of people who will realistically spend the time and energy launching a direct competitor is low. Also, most people interested in your open-source code want to learn some aspect of it and apply it to their own ideas, not just copy yours directly.

Secondly, and most importantly, the fact that it’s open-source makes people a lot more receptive to you talking about it. 

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/q79djej6doj2yq10l2og.png)

When you present something you’ve built and give people the opportunity to learn from it, they’re much more welcoming! As a result, they’re more likely to upvote it, share it, use it, and recommend it to others.

This is exactly what happened with CoverLetterGPT! As a result of me sharing the open-source code, it get featured on the [IndieHackers.com](https://www.indiehackers.com/post/whats-new-don-t-build-things-no-one-wants-833ee752ba?utm_source=indie-hackers-emails&utm_campaign=ih-newsletter&utm_medium=email) newsletter (>100k subscribers), shared on blogs, and talked about on social media platforms.

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/44rlv65u97qhufbhqt0k.png)

And even though it’s a small, simple product, I tried launching it on [Product Hunt](http://producthunt.com), where it also performed considerably well.

So, all together, these initial efforts combined gave my product a good initial marketing presence. To this day, I haven’t really done much else to market it, except some twitter posts (and this post, if you want to consider it marketing 🤑).

## How I Monetized It

When I first launched in March 2023, I didn’t really expect anyone to pay for the product, but I wanted to learn how to use Stripe as a payments processor, thinking that the skills might be useful in the future.

So I started simple, and just put a one-time payment link for tips. No paywall, no subscriptions. It was entirely free to use with any tip amount welcome.

To my surprise, tips started coming in, with some as high as $10 dollars!

This encouraged me to force users to login to use the product, and add a paywall after users used up 3 credits.

My initial payment options were:

- $4.95 for a 3 months access
- $2.95 for 10 cover letter generations

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/golo3tnh3o0sy5sujrer.png)

That went reasonably well until I implemented the ability for users to use GPT to make finer edits to their generated cover letters. That’s when I changed my pricing and that’s when better profits started to come in:

- $5.95 / month subscription with GPT-4
- $2.95 / month subscription with GPT-3.5-turbo

Currently, over 90% of my customers choose the more powerful, more [expensive plan with GPT-4](https://openai.com/pricing), even though the 3 trial credits use the GPT-3.5-turbo model.

(I also integrated Bitcoin Lightning payments — check out the [repo](https://github.com/vincanger/coverlettergpt) if you want to learn how — but haven’t received any yet.)

Now, with an MRR of ~$203, my monthly profit of course depends on my costs, which are:

- Domain Name: $10/year
- OpenAI bill: ~ $15/month
- Hosting bill: ~ $3/month

Which leaves me at about ~ $183/month in profits 😀

## Future Plans

One of the most surprising aspects about [CoverLetterGPT.xyz](http://CoverLetterGPT.xyz)’s success is that, on the surface, the product is very simple. Also, I’ve done very little in the way of SEO marketing, and haven’t continued to market it much at all. The current growth is mostly organic at this point thanks to my initial marketing efforts.

But I still have some plans to make it better:

- buy a better top-level domain (TLD), like [CoverLetterGPT.ai](http://CoverLetterGPT.ai)
- add more features, like the ability to generate interview questions based on the cover letters
- improve the UX and make it look more “professional”

If you have any other ideas how I could improve it, drop me a comment, message me on [twitter/x](https://twitter.com/hot_town), or submit a [PR to the repo](https://github.com/vincanger/coverlettergpt).

## Final Words + More Resources

My intention with this article was to help others who might be considering launching their own SaaS product. So I hope that’s been the case here. If you still have any questions, don’t hesitate to ask.

Here are also the most important links from this article along with some further resources that will help in building and marketing your own profitable side-project:

- 👨‍💻 [CoverLetterGPT GitHub Repo](https://github.com/vincanger/coverlettergpt)
- 💸 [Free Full-Stack SaaS Template w/ Google Auth, Stripe, GPT, & instructions in the README!](https://github.com/wasp-lang/SaaS-Template-GPT)
- ✍️ [Initial CoverLetterGPT Reddit Post](https://www.reddit.com/r/webdev/comments/11uh4qo/comment/jco5ggp/?utm_source=share&utm_medium=web2x&context=3)
- 🪓 [IndieHackers Feature](https://www.indiehackers.com/post/whats-new-don-t-build-things-no-one-wants-833ee752ba?utm_source=indie-hackers-emails&utm_campaign=ih-newsletter&utm_medium=email)
- 💸 [Great Video on how to use Stripe CLI & Webhooks](https://www.youtube.com/watch?v=Psq5N5C-FGo&t=1041s)

Oh, and if you found these resources useful, don't forget to support Wasp by [starring the repo on GitHub](https://github.com/wasp-lang/wasp)! 

![https://res.cloudinary.com/practicaldev/image/fetch/s--OCpry2p9--/c_limit%2Cf_auto%2Cfl_progressive%2Cq_66%2Cw_800/https://dev-to-uploads.s3.amazonaws.com/uploads/articles/bky8z46ii7ayejprrqw3.gif](https://res.cloudinary.com/practicaldev/image/fetch/s--OCpry2p9--/c_limit%2Cf_auto%2Cfl_progressive%2Cq_66%2Cw_800/https://dev-to-uploads.s3.amazonaws.com/uploads/articles/bky8z46ii7ayejprrqw3.gif)

{% cta [https://www.github.com/wasp-lang/wasp](https://www.github.com/wasp-lang/wasp) %} ⭐️ Thanks For Your Support 🙏  {% endcta %}