---
title: 'Wasp Beta December 2022'
authors: [matijasos]
image: /img/update-dec-23/banner.png
tags: [webdev, wasp, startups, github]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

<ImgWithCaption
    alt="Wasp Update Dec 22"
    source="img/update-dec-23/banner.png"
/>

*Want to stay in the loop? → [Join our newsletter!](/#signup)*

Hey Wasp tribe 🐝 ,

Happy New Year! I know you're probably already sick of hearing it, but hopefully we're the last ones to congratulate you 🔫 👈 (that's pistol fingers emoji in case you were wondering).

<!--truncate-->

<ImgWithCaption
    alt="Pistol fingers"
    source="img/update-dec-23/pistol-fingers.gif"
    caption="This is how I imagine myself telling the joke above."
/>

Now that the [Beta Launch](/blog/2022/11/29/wasp-beta) craze is over (thanks for your support, it was amazing - we saw more devs hacking with Wasp than ever!), we're back to our usual programming. Let's dive in and see what's new and what's in the plans for this year:

## 🎮 🐝 We hosted our first hackathon - it was a blast! 🎉 🎉

<ImgWithCaption
    alt="Tweet about Wasp"
    source="img/update-dec-23/tskaggs-tweet.png"
/>

We launched our first Wasp hackathon ever on the last day of Beta Launch (thus we named it Betathon) and got some [really cool submissions](/blog/2023/01/11/betathon-review)! Winners received hosting credits kindly offered by our partners at [Railway](https://railway.app/) and a special 1st place award was a wasp-themed mechanical keyboard (we're still assembling it but we'll post photos on our [twitter](https://twitter.com/WaspLang) :))!

> “***This was the best app dev experience I ever had!*** *…Walking through the docs, I immediately figured out how to use Wasp and was able to make a prototype in a couple of days.”* - Chris
> 

To check out the winning projects and see where devs found Wasp most helpful, take a look here: [Wasp Betathon review post](/blog/2023/01/11/betathon-review)

## 🔑 New auth method - GitHub! 🐙

Next to [username/password](/docs/language/features#username-and-password) and [Google](/docs/language/features#google), **Wasp now also supports [GitHub](/docs/language/features#github)** as an authentication method!

<ImgWithCaption
    alt="Support for GitHub auth in Wasp"
    source="img/update-dec-23/github-auth.png"
/>

Putting the code above in your `main.wasp` file and specifying your GitHub env variables is all you need to do! Wasp will provide you with a full-stack GitHub authentication along with UI helpers (GitHub sign-up button) you can immediately use in your React component.

For more details, check the docs [here](/docs/language/features#github).

## 💬 Let's discuss - on GitHub Discussions!

<ImgWithCaption
    alt="Wasp is now on GitHub Discussions"
    source="img/update-dec-23/gh-discussions.png"
/>

So far we've been capturing your feedback across [GitHub issues](https://github.com/wasp-lang/wasp/issues) and [Wasp Discord server](https://discord.gg/rzdnErX), but with the current volume it has become a bit unwieldy and hard to keep track of.

That's why we introduced [Wasp GitHub Discussions](https://github.com/wasp-lang/wasp/discussions)! It's a relatively new service by GitHub that allows distinguishing between specific, well-defined issues (bug reports, TODOs, ...) and discussion items (ideating about new features, figuring out best practices, etc) and allows for upvotes from the community.

If there is a feature you'd like to see in Wasp (e.g. support for Vue) you can create a new post for it or upvote it if it is already there!

## 🚀 Next launch is coming - a super early sneak peek 👀 

<ImgWithCaption
    alt="Next launch sneak peek"
    source="img/update-dec-23/next-launch.png"
/>

We know we just wrapped up Beta release, but we are busy wasps and our heads are already in the next one! We made a preliminary draft of the features that are going to be included - **the "theme" of this release is going to be about making Wasp super easy and friendly for you to use**.

We'll further polish our auth & deployment experience, along with ensuring TypeScript experience is fully typed and as helpful as possible. Stay tuned for the official roadmap and date of the next launch!

Want to make sure your fav feature makes it into the next release? [Let us know on Discussions](https://github.com/wasp-lang/wasp/discussions)!

## 🎥 Wasp is now on YouTube!

<ImgWithCaption
    alt="Wasp is on YouTube"
    source="img/update-dec-23/wasp-youtube.png"
/>

Thanks to Vince, who recently joined as Devrel (intro blog post coming soon!), Wasp now finally has [its YouTube channel](https://www.youtube.com/@wasplang/videos)!

We're just starting out but already made some splashes - our ["Build a full-stack app in 9 mins with Wasp and ChatGPT"](https://youtu.be/HjUpqfEonow) got over 2k views (not bad for a channel with 50 subscribers, right?).

We also made our first YT short, featuring [how to add auth to your app in 60 seconds](https://youtube.com/shorts/-daNTYiUC64?feature=share) with Wasp.

If you want to stay in the loop (and I guess you do since you're reading this :D), please [subscribe to our channel](https://www.youtube.com/@wasplang) and help us reach the first 100 subscribers on YouTube!

<ImgWithCaption
    alt="Subscribe to Wasp on YouTube"
    source="img/update-dec-23/yt-cta.png"
    caption="You know you want it!"
/>

## 🕹 Community highlights

* [Making Something Waspy: A Review Of Wasp](https://dev.to/emmanuelthecoder/making-something-waspy-a-review-of-wasp-571j) - an overview of Wasp by Emmanuel, one of our contributors and hackathon winners!

* [The first 2,000 GitHub stars of Wasp](https://podcast.bitreach.io/episodes/matija-sosic) (podcast 🔊) - I was a guest on the Scaling DevTools podcast by [Jack Bridger](https://twitter.com/jacksbridger) - we chatted about how Wasp started, what were the hardest parts and what are the plans for the future!

## Wasp Github Star Growth - over 2,000 ⭐️, woohoo!

Beta was great and it brought us to [2,234 stars](https://github.com/wasp-lang/wasp)! We never imagined Wasp could become so popular when we were just getting started. Huge thanks to all our [contributors](https://github.com/wasp-lang/wasp/graphs/contributors) and [stargazers](https://github.com/wasp-lang/wasp/stargazers) - you are amazing!

<ImgWithCaption
    alt="Wasp has over 2,000 GitHub stars"
    source="img/update-dec-23/gh-stars.png"
/>

And if you haven't yet, please [star us on Github](https://wasp-lang.dev/docs)! Yes, we are shameless star beggars, but if you believe in the project and want to support it that's one of the best ways to do it (next to actually building something with Wasp - [go do that too](https://wasp-lang.dev/docs)! :D)

And before you leave, here's a photo of a squishy wasp (ok, it's a bumblebee, but you get it) proudly rocking Wasp swag 🤘 🐝 (yep, we got a bunch of these for the office, you can also see Martin the background :D)!

<ImgWithCaption
    alt="Wasp's new mascot"
    source="img/update-dec-23/da-boi.jpg"
    caption="This lil' boy actually became pretty popular in our community - we're now looking for a name for him!"
/>

Thanks for reading and see you in a month!

Buzzity buzz, you got that pizzazz 🐝 🐝,  
Matija, Martin and the Wasp team


*Want to stay in the loop? → [Join our newsletter!](/#signup)*

