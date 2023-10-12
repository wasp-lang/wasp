---
title: 'Watch us build a *truly* full-stack app in just 9 minutes w/ Wasp & ChatGPT 🚀 🤯'
authors: [vinny]
tags: [wasp, ai, chatgpt, fullstack, language]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

<br/>
<div style={{ textAlign: "center", width: "100%", display: "inline-block" }}>
<iframe width="100%" height="400" src="https://www.youtube.com/embed/HjUpqfEonow" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen="true"></iframe>
</div>

<!--truncate-->
---

There’s a lot of hype around ChatGPT at the moment, and for good reason. It’s amazing. But there’s also some very valid criticism: that it’s simply taking the grunt work out of programming by writing boilerplate for us, which we as developers have to maintain! 

<div style={{ marginBottom: "1rem" }}>
    <a href="https://twitter.com/paulg/status/1600447377248116736?ref_src=twsrc%5Etfw">
        <img src='https://dev-to-uploads.s3.amazonaws.com/uploads/articles/5rgubux630836d05mje5.png' alt="I expected technology to make programming less laborious, as it does to most things. But I have to admit I expected it to happen by programmers switching to more powerful languages, rather than continuing to write programs full of boilerplate, but having AIs generate most of it."/>
    </a>
</div>

PG is totally right in his remark above, but what he doesn’t realize is that there are languages out there that attempt to overcome this very problem, and [Wasp](https://wasp-lang.dev) is one of them. 

What makes Wasp unique is that it’s a framework that uses a super simple **language** to help you build your web app: front-end, server, and deployment. But it’s not a complicated language like Java or Python, it’s more similar to SQL or JSON, so the learning curve is really quick (technically, it’s a *Domain Specific Langauge* or *DSL*). 

Check it out for yourself:

```wasp title="main.wasp"
app todoApp {
  title: "ToDo App",/* visible in tab */

  auth: {/* full-stack auth out-of-the-box */
    userEntity: User,
    externalAuthEntity: SocialLogin,
    methods: {
      usernameAndPassword: {},
      google: {}
    }
  }
}

route RootRoute { path: "/", to: MainPage }
page MainPage {
	/* import your React code */
  component: import Main from "@client/Main.js"
}
```

With this simple file above, Wasp will continually compile a truly full-stack web app for you, with a React front-end, and an ExpressJS server. You’re free to then build out the important features yourself with React, NodeJS, Prisma, and react-query.

The great part is, you can probably understand the Wasp syntax without even referencing the docs. Which means AI can probably work with it easily as well. So rather than having AI create a ton of boilerplate for us, we thought “can ChatGPT write Wasp?” If it can, all we need is to have it create that one file, and then the power of Wasp will take care of the rest. No more endless boilerplate!

So that’s exactly what we set to find out in the video above. The results? Well let’s just say they speak for themselves.
