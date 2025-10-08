---
title: "Why We Don't Have a Laravel For JavaScript... Yet"
authors: [vinny]
image: /img/laravel-for-js.png
tags: [framework, javascript, full-stack, laravel, rails]
---

## JavaScript's Need for a Full-stack Framework

“_Why Don't We Have A Laravel For JavaScript?”._ This is the question [Theo poses in his most recent video](https://www.youtube.com/watch?v=yaodD79Q4iE).

And if you’re not familiar with tools like [Laravel](https://laravel.com/) and [Ruby-on-Rails](https://rubyonrails.org/), they are opinionated full-stack frameworks (for PHP and Ruby) with lots of built-in features that follow established conventions so that developers can write less boilerplate and more business logic, while getting the industry best practices baked into their app.

{/* truncate */}

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/dndfaz2hjet9nisbiimc.png)

He answers this question with the opinion that JavaScript _doesn’t need_ such frameworks because it’s better to select the tools you want and build the solution you need yourself.

This sounds great — and it also happens to be a nice flex if you’re a seasoned dev — but I feel that he doesn’t back up this claim very well, and **I’m here to tell you where I think he’s wrong**.

In my opinion, the better question to ask is why don’t we have a Laravel for JavaScript _yet_? The answer being that we’re still working on it.

In his summary of the full-stack frameworks of the JavaScript world that could be comparable to Laravel or Rails, he fails to consider a few important points:

1. **People really want a Laravel / Rails for JavaScript**. If they didn’t, there wouldn’t be so many attempts to create one, and he wouldn’t be making a video whose sole purpose is to respond to the pleading cry “_WHY DOESN’T JAVASCRIPT HAVE ITS OWN LARAVEL!?_”
2. **He fails to consider the timing and maturity of the underlying tools within the JS ecosystem**. Perhaps it’s not that a Laravel for JavaScript doesn’t _need_ to exist, it’s just that it doesn’t exist yet due to some major differences in the ecosystems themselves, like how old they are and where the innovation is mostly happening.
3. **He also fails to ask for whom these types of solutions are suitable for**. Surely, not all devs have the same objectives, so some might opt for the composable approach while others prefer to reach for a framework.

So let’s take a look at how we got to the point we’re at today, and how we might be able to bring a full-stack framework like Laravel or Rails to the world of JavaScript.

## Getting Shit Done

In his video, Theo brings up the point that "_there's a common saying in the React world now which is that ‘if you're not using a framework you're building one’”._ Even though this is meant to be used as a criticism, Theo feels that most JavaScript devs are missing the point and that building your “own framework” is actually an advantage.

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/q5ft5f1j2zemurcvvzzk.png)

He feels that the modular nature of the JavaScript ecosystem is a huge advantage, but that sounds like a lot of pressure on the average developer to make unnecessary judgement calls and manage lots of boilerplate code.

Sure, you have teams that need to innovate and meet the needs of special use cases. These are the ones that prioritize modularity. They tweak, improve, and squeeze as much out of developer experience (DX) and performance as possible to get their unique job done right.

But on the other hand, there are also numerous teams whose main objective is producing value and innovating on the side of the product they are building, instead of the tools they are using to build it. These devs will favor a framework that allows them to focus solely on the business logic. This gives them a stable way to build stuff with best practices so they can easily advance from one project to another. In this camp are also the lean, mean indiehackers looking for frameworks so they can move fast and get ideas to market!

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/21tanpp8rmrboal0jl57.png)

It’s a bit like the difference between Mac and Linux. Mac’s unified stack that just works out-of-the box means many professionals prefer it for its productivity, whereas Linux is great if you’re looking for flexibility and have the time and knowledge to tweak it to your desires. Both are valid solutions that can coexist to meet different needs.

This focus on productivity is what made Rails so powerful back in the day, and why Laravel is such a loved framework at the moment. And the many attempts at creating such a framework for JavaScript is proof enough that there is a large subset of JavaScript devs who also want such a solution.

But maybe the reason such a framework doesn’t exist yet doesn’t have to do with whether devs want one or not, but rather the important factors which are needed in order for such a framework to come together haven’t aligned up until this point. For such a framework to be widely adoptable, it first needs underlying technologies that are stable enough to build upon. After that, it needs time and many iteration cycles to reach maturity itself, so that devs can feel comfortable adopting it.

Have these factors aligned in the JavaScript world to give us the type of frameworks that PHP and Ruby already have? Maybe not quite yet, but they do seem to be slowly coming together.

## Comparing Ecosystems

One of Theo’s main points is that JavaScript as a language enables a level of modularity and composability that languages like Ruby and PHP don’t, which is why Ruby and PHP ecosystems are well served by full-stack frameworks, but JavaScript _doesn’t need one_ since you can just compose stuff on your own.

While JavaScript is a peculiar language, with its support for both functional and imperative paradigms and dynamic nature, it also comes with a lot of pitfalls (although it has improved quite a bit lately), so you don’t typically hear it get praised in the way Theo does here. In fact, you are probably more likely to hear praise for Ruby and its properties as a modular and flexible language.

So if it isn’t some unique properties of JavaScript as a language that make it the king of web dev, what is it then?

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/r051nlojgrvt9crc03jd.png)

Well, the answer is pretty simple: **JavaScript is the language of the browser**.

_Way back_ when most of the web development was happening on the server side, PHP, Java, Ruby and other languages where reigning supreme. During this era, devs would only write small pieces of functionality in JavaScript, because most of the work was being handled server-side.

But as web development evolved and we started building richer applications, with more dynamic, responsive, and real-time features, a lot of code moved away from the server and over towards JavaScript on the client, because it’s (basically) the only language that supports this. So instead of doing your development mostly in PHP or Ruby with a little bit of JavaScript sprinkled in there, you were now splitting your apps between substantial amounts of JavaScript on the client, plus Ruby or PHP on the server.

JavaScript’s final power move came with the arrival of NodeJS and the ability to also write it on the server, which secured its position as the king of web dev languages. Today, devs can (and do) write their entire apps in JavaScript. This means you need to know one language less, while you’re also able to share the code between front-end and back-end. This has opened up a way for better integration between front-end and back-end, which has snowballed into the ecosystem we know today.

So it’s not so much the unique properties of JavaScript as a language that have made it the dominant ecosystem for web development, but more its unique monopoly as the only language that can be used to write client code, plus it can also be used server-side.

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/ulc5eo5qu3a0nuv6bnv5.png)

As Theo says, “_we’ve got infinitely more people making awesome solutions_” in the JavaScript ecosystem. That’s right. It’s exactly those infinite number of developers working in the space creating the flexibility and modular solutions for JavaScript, rather than it being an innate quality of the programming language.

And because the JavaScript ecosystem is still the hottest one around, it has the most devs in total while continuing to attract new ones every day. This means that we get a large, diverse community doing two main things:

1. Innovating
2. Building

The innovators (and influencers) tend to be the loudest, and as a result opinion largely skews in their favor. But there is also a lot of building, or “normal” usage, happening! It’s just that the innovators tend to do the talking on behalf of the builders.

So with all that’s going on in the JavaScript ecosystem, is it pointless to try and build a lasting framework for JavaScript developers, as Theo suggests, or are we on the path towards achieving this goal regardless of what the innovators might claim?

## Show Me What You’re Working With

Theo also drops the names of a bunch of current JavaScript frameworks that have either failed to take off, or “_just can’t seem to get it right_” when it comes to being a comprehensive full-stack solution.

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/susa9icldz2hzysqo1qm.png)

And he does have a point here. So far, solutions like [Blitz](https://blitzjs.com/), [Redwood](https://redwoodjs.com/), [Adonis](https://adonisjs.com/), or [T3](https://create.t3.gg/) haven’t managed to secure the popularity in their ecosystem that Rails or Laravel have in theirs.

**But these things take time.**

Have a look at the graph above. Laravel and Rails have been around for 13-15 years! The JavaScript frameworks being used in comparison are just getting started, with some of them, like [Wasp](https://wasp.sh) and [Redwood](https://redwoodjs.com/), at similar stages in their development as Laravel and Rails were during their initial years.

As you can see, it takes time for good solutions to reach maturity. And even with some of these frameworks starting to stagnate their great initial growth is evidence that demand for these tools definitely exists!

The main overlying issue that tends to plague these tools is that Javascript as an ecosystem is moving quite fast, so for a solution like this to survive long term, it needs to not only be opinionated enough, but also modular enough to keep up with the shifts in the ecosystem.

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/biqe18hoeb6d06ughjhh.png)

One factor that prevents frameworks from reaching this state is being tied too tightly to the wrong technology. This was NextJS for BlitzJS, GraphQL for Redwood, and Blaze for MeteorJS. And another factor is _not going big enough_ with the framework, because it seems too daunting a task within the JavaScript ecosystem, where things move fast and everyone is “terrified of being opinionated” because they might get criticized by the loudest voices in the scene.

In other words, frameworks that avoid going big on their own, and going _truly_ full-stack, like Ruby-on-Rails and Laravel went, miss the opportunity to solve the most common pain-points that continue to plague JavaScript developers.

But, the JavaScript ecosystem is maturing and stabilizing, we are learning from previous attempts, and there _will_ be a full-stack framework bold enough to go all the way in, get enough things right, and persist for long enough to secure its place.

## Say Hi to Wasp

In his comparison of JavaScript frameworks on the market today, Theo also fails to mention the full-stack framework for React & NodeJS that we’re currently working on, [Wasp](https://wasp.sh).

We’ve been working hard on [Wasp](https://wasp.sh) to be the _truly_ full-stack framework that meets the demands of web developers and fills that void in the JavaScript ecosystem to become the framework they love to use.

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/z7j67hychqw770nrau3f.png)

**With Wasp, we decided to go big, opinionated, and truly full-stack**. In other words, we’re going _all in_ with this framework.

That means thinking from first principles and designing a novel approach that only Wasp uses, like building our own compiler for our configuration language, and truly going full-stack, while also keeping it modular enough to move together with the ecosystem as it progresses.

This means that we spent more time in the beginning trying different approaches and building the foundation, which finally brought us a significant jump in usage starting in late 2023. Wasp is now growing strong, and at a really fast pace!

It’s really cool for us to see Wasp being used today to ship tons of new apps and businesses, and even being used internally by some big names and organizations (more info on that will be officially released soon)!

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/1eo9feum42heb2qcua9o.png)

What Wasp does differently than other full-stack frameworks in the JavaScript world is that it separates it’s main layer of abstraction into its own configuration file, `main.wasp`. This config file gives Wasp the knowledge it needs to take care of a lot of the boilerplatey, infrastructure-focused code, and allows it to have this unique initial compile-time step where it is able to reason about your web app before it generates the code for it in the background (using that knowledge while generating it).

In practice, this means that all you have to do is describe your Wasp app at a high level in Wasp’s config file, and then implement everything else in technologies that you’re familiar with such as React, NodeJS, and Prisma. It also means that Wasp has a high modularity potential, meaning we are building it to also support other frontend frameworks in the future, like Vue, Solid or Svelte, and to even support additional back-end languages, like Python, Go or Rust.

If you’re the kind of developer that wishes a Rails or Laravel for JavaScript existed, then you should [give Wasp a try](https://wasp.sh/) (and then [head into our Discord](https://discord.gg/rzdnErX) and let us know what you think)!

## Where Are We Headed?

We firmly believe that there will be a full-stack framework for JavaScript as there is Laravel for PHP and Ruby-on-Rails for Ruby.

It just seems like, at the moment, that we’re still working towards it. It also seems very likely that we will get there soon, given the popularity of current meta-frameworks and stacks like NextJS and T3.

But this stuff takes time, and patience.

Plus, you have to be bold enough to try something new, knowing you will get criticized for your work by some of the loudest voices in the ecosystem.

That’s what we’re prepared for and why we’re going _all in_ with Wasp.

See you there!

![Image description](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/nig9t4pr85jqtz536frn.png)
