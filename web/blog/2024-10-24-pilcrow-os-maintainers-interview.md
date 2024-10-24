---
title: 'The Faces Behind Open Source Projects: Pilcrow'
authors: [milica]
image: /img/os-interviews/pilcrow_banner.png
tags: [webdev, auth, os-maintainers]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

We're back with our series of posts in which we interview open-source maintainers and showcase their amazing projects they keep pushing. This week we've talked with [Pilcrow](https://github.com/pilcrowonpaper), an inspiring young dev from Japan, famous for his auth library Lucia and love for auth and security. 

[Lucia](https://github.com/lucia-auth/lucia) is one of the open-source tools that we use under-the-hood at Wasp. It grew to 9.5K stars on GitHub since October 2023. Unfortunately, this project is going to be deprecated in April 2025. Wasp will make all the necessary changes on time, no worries 😃 You should look into other projects Pilcrow has kickstarted as well, some of those will serve as a replacement for Lucia.

We hope this interview inspires you to contribute to the open-source community yourself. Let's dive in!

- **Could you tell us a little about yourself and how you got into coding? What led you to focus on authentication and tooling for developers?**

I’m Pilcrow. I’m a university student in Japan working on various open source projects around auth and security. I started programming like 4 years ago so I'm still new to the field. I love designing APIs a lot and that's probably the biggest reason I enjoy working on libraries.

- **Lucia brought you recognition within the dev community. What inspired you to create it, and how did you decide to dedicate so much time and effort to it?**

I was asked to build several websites for school events and the part I struggled with the most was auth. Half of it was my inexperience, but I also found third-party offerings like Firebase to be unintuitive. Most of their features and APIs at the time were aimed at SPAs as well and didn't work well with server-rendered applications. The open source offerings weren't great either. To this day, I still don't really know how to actually use NextAuth (now Auth.js) or Passport.js.

Over summer break, I decided to build a more leaner NextAuth for SvelteKit so I could use it for my projects. It was my first open source library but it got some attention in the Svelte community. I made it framework agnostic a little while later and it grew naturally.

Working on it got me hooked on library development, and I found designing APIs and even documenting them to be very enjoyable.

:::tip[Would you star us on GitHub?]
If you'd like to keep reading more about other open-source maintainers and help Wasp grow, could you please [star us on GitHub as a sign of your support](https://github.com/wasp-lang/wasp)? 🙏
:::

- **Managing an open-source project like Lucia must come with challenges. What were some unexpected hurdles, and how did you handle them?**

I honestly don't find anything about maintaining open-source projects specifically to be challenging. It just requires a lot of your time with answering questions on Discord and writing documentation. I've definitely changed my approach to library development from when I started though. I now put most of my effort into designing APIs and writing documentation over adding new features.

![giphy applause](https://media.giphy.com/media/mGK1g88HZRa2FlKGbz/giphy.gif?cid=790b7611pf6oxpvnttw8a1ej43c0c8escbmkyvschhu7wr9f&ep=v1_gifs_search&rid=giphy.gif&ct=g)

- **You're also working on [The Copenhagen Book](https://github.com/pilcrowonpaper/copenhagen), which focuses on authentication guidelines. Why is authentication such a key area of interest for you?**

I find figuring why something is and isn't vulnerable a fun mystery to solve. I also love that there are so many rabbit holes you can find yourself in. You're searching about some protocol one moment and the next you're reading a 50-page RFC on some obscure encoding format. It's a field that pushes you to learn more.

(As a bonus, I don't have to deal with CSS).

- **[Oslo](https://github.com/pilcrowonpaper/oslo) provides auth-related utilities. Could you walk us through its main features and the problem space it addresses?**

Oslo is a collection of packages for various auth and cryptographic operations. Base64, WebAuthn, SHA-256, ECDSA, TOTP/HOTP, ASN.1, etc. It doesn't rely on third-party dependencies or runtime-specific APIs, so it's super lean and runs everywhere. The APIs are relatively low-level compared to similar libraries as well. It requires some knowledge on the underlying standards, but you don't need to learn any library-specific concepts, which is a very big win in my book.

Also, it's a very simple feature but just having some kind of documentation is a big plus.

- **How do you balance your time across multiple open-source projects while ensuring each gets the attention it deserves?**

I wish I had a better system, but I just work on whatever I feel like. I’m not very good at multi-tasking and prefer focusing on one thing at a time. I don't get a lot of bug reports anyway so it works for me.

- **How do you keep up with evolving security standards across projects, and what advice would you give to teams building their own auth solutions from scratch?**

To be honest, I don't think web application security evolves that fast. At least compared to the JavaScript ecosystem (but I guess nothing does). It's rare for a cryptographic algorithm to break overnight and the threat model and basic recommendation is still the same from 10 years ago. A lot of vulnerabilities are still broken access controls and insufficient input sanitation.

But just a lot of reading. Understand the framework and protocols you're dealing with. RFCs and documentations already cover a lot of possible pitfalls. I guess this applies to those who are planning to delegate auth to a third party as well, at least to some extent.

- **Open-source maintainers often struggle with burnout. How do you stay motivated, and what keeps you coming back to these projects?**

Maintaining various projects definitely helps with avoiding burnout. My projects aren’t that big in scale and actual bug reports are rare, so I don’t worry about switching my focus to a different project or taking a break for a week or two.

I also see open source as just a hobby. I'll still try to address bugs quickly, but I don't rush myself to add new features.

![relax gif](https://i.giphy.com/media/v1.Y2lkPTc5MGI3NjExcWF3ZGRwNjFmYmVzanM0d3ZydzNjd2w5bTlxMWlqd294aWwzNmZzMiZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/q10hztkXiFFmhBbH2l/giphy.gif)
- **When collaborating on open-source, how do you manage community contributions and keep other developers involved in a meaningful way?**

I don't think my projects are particularly good examples of open source collaborations. I usually ask people to open GitHub issues for new features and I'm not really open to pull requests except for bug and typo fixes. I like to take my time experimenting with various API designs and I personally find it easier if I do the initial design phase by myself. There's also a certain flavor of code I like and I really want to keep my code "beautiful" (yes, I know it sounds dumb).

To be honest, I don't like pull requests in general since I feel like I wasted the contributor's time if I reject it, even if the code is objectively awful.

I'm well aware that this closed approach doesn't work in bigger projects, but working on open source is ultimately just a hobby, so I think I should be able to have some fun with it.

Most of the community aspect comes from Discord, and I really appreciate people who are active daily and answer questions there. Honestly, it's probably some of the most valuable contributions I receive.

- **What would you do differently in the development and management of your projects, knowing what you know now about open-source and developer tools?**

Defining the scope and goals of a project early on. I don’t blame myself for not thinking much ahead when starting Lucia since it was literally my first open source library, but it would’ve made development on subsequent major releases much easier. There’s only so many features you can add to a project before the code becomes a mess. You can’t appease everyone, so pick a lane and be the best for that purpose.

I also think that libraries that are flexible in how and where they can be used, instead of flexible in their behavior, are more predictable and easier to work with. Clearly defining the line between library and user responsibility is going to make it much more enjoyable to use.

- **What is your favorite type of ramen?**

Probably tonkotsu, specifically from Kyushu (the third-largest island of Japan's four main islands). The broth is based on pork bones, which gives it a creamy and rich texture, and the noodle has a bite to it (like al dente pasta).

![Tonkatsu ramen](https://prd-static.gltjp.com/glt/data/article/21000/20290/20230407_103858_503a919a_w1920.webp)

Hope you enjoyed this interview. If you have any suggestions about whom to interview next, [join us on Discord](https://discord.gg/TUeXksNqaH) and let us know!
