---
title: Feature Announcement - New auth method (Google)
authors: [shayneczyzewski]
image: /img/auth-hero.png
tags: [webdev, wasp, feature, auth]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';
import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

<p align="center">
  <img alt="No login for you!"
      src={useBaseUrl('img/auth-hero.png')}
      width="300px"
  />
</p>

<!--truncate-->

<WaspIntro />
<InBlogCta />

## Prologue

We've all been there. Your app needs to support user authentication with social login, and you must now decide what to do next. Should you eschew the collective experience and wisdom of the crowd and YOLO it by rolling your own, praying you don't get pwned in prod? "Nah, I just ate some week-old sushi and can't take another risk that big anytime soon.", you rightly think.

Ok, surely you can just use a library, right? Open source software, baby! "Hmm, seems Library X, Y, and Z are all somewhat used, each with their pros/cons, nuances, and integration pain points. Oh wait, there are tutorials for each... but each says how hard they are to correctly set up and use. I scoped this feature for one day, not a one-week hair-pulling adventure (Dang scrum! Who likes it anyways? Oh yeah, PMs do. Dang PMs!)." Ok, something else. You need to brainstorm. `You instead start to surf Twitter and see an ad for some unicorn auth startup.`

Eureka, you can go with a third-party SaaS offering! "We shouldn't have to pay for a while (I ~~think?~~ hope!), and it's just another dependency, no biggie... #microservices, right?" "But what about outages, data privacy, mapping users between systems, and all that implicit trust you are placing in them?" you think. "What happens when Elon buys them next?" You gasp as if you walked by a Patagonia vest covered in that hot new *Burnt Hair* cologne.

"All I want is username and password auth with Google login support, why is that so hard in 2022?!? I miss Basic HTTP auth headers. I think I'll move off the grid and become a woodworker."

## Easy auth setup in Wasp

Wasp helps that dev by taking care of the entire auth setup process out of the box. Adding support for username and password auth, plus Google login, is super quick and easy for Wasp apps. We think this makes adding auth fast and convenient, with no external dependencies or frustrating manual configuration. Here’s how it works:

### Step 1 - Add the appropriate models

We need to store user info and the external mapping association for social logins. Here is an example you can start from and add new fields to:

```sql title="./main.wasp"
entity User {=psl
    id                        Int           @id @default(autoincrement())
    username                  String        @unique
    password                  String
    externalAuthAssociations  SocialLogin[]
psl=}

entity SocialLogin {=psl
  id          Int       @id @default(autoincrement())
  provider    String
  providerId  String
  user        User      @relation(fields: [userId], references: [id], onDelete: Cascade)
  userId      Int
  createdAt   DateTime  @default(now())
  @@unique([provider, providerId, userId])
psl=}
```

### Step 2 - Update `app.auth` to use these items

```css title="./main.wasp"
app authExample {
  // ...
  auth: {
    userEntity: User,
    externalAuthEntity: SocialLogin,
    methods: {
      usernameAndPassword: {},
      google: {}
    },
    onAuthFailedRedirectTo: "/login"
  }
}
```

### Step 3 - Get Google credentials and add environment variables

Follow the Google setup guide [here](https://wasp-lang.dev/docs/integrations/google) and add the environment variables to your `.env.server` file.

### Step 4 - Make use of the Google login button in your `Login` page component

```jsx title="./src/client/auth/Login.js"
import React from 'react'
import { Link } from 'react-router-dom'

import { SignInButton as GoogleSignInButton } from '@wasp/auth/helpers/Google'
import LoginForm from '@wasp/auth/forms/Login'

const Login = () => {
  return (
    <div>
      <div>
        <LoginForm/>
      </div>
      <div>
        I don't have an account yet (<Link to="/signup">go to signup</Link>).
      </div>
      <div>
        <GoogleSignInButton/>
      </div>
    </div>
  )
}

export default Login
```

### Step 5 - Run the app!

## Epilogue

No need to move off the grid out of frustration when adding authentication and social login to your web app. [Here](https://github.com/shayneczyzewski/authExample) is a complete, minimal example if you want to jump right in, and [here](https://wasp-lang.dev/docs/language/features#authentication--authorization) are the full docs for more info. With just a few simple steps above, we've added authentication with best practices baked into our app so we can move on to solving problems that add value to our users!
