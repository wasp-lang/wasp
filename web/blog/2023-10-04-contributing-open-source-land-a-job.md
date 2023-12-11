---
title: "Contributing to Tech Communities: How Open-Source can land you a job and get you out of the Skill Paradox"
authors: [vinny]
image: /img/open-source-contribute.gif
tags: [career, web-development, open-source, hacktoberfest]
---

## TL;DR
![How to Open-Source](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/04lotyojmwdnzt7b2s8j.png)
In this article, we’re going to see how open-source can change your career for the better and get you out of the Skill Paradox — a point in which the skills you need to land a job are generally acquired after you get a job.

Besides that, we’ll check how you can start contributing to different open-source projects and get on the hype train of [Hacktoberfest](https://hacktoberfest.com/) while also learning some important topics on handling feedbacks and showcasing your contributions.

## 1. Introduction

Are you a beginner developer that lacks certain skills needed to land a job? But you feel that you could only gain those skills **on** the job itself?
If you answered “yes”, then you’re stuck in situation that I would call as the “**skill paradox**” ***—*** where you need skills to get a job, but those skills are the ones you would get if you had a job. It can generate a lot of stress and frustration when you start to realize that some skills cannot be obtained while working only on side hustles and therefore, you cannot learn only by yourself, but they’re generally required for job positions.

Collaboration and teamwork, learning how to code review (giving and receiving feedback), and getting started with bigger and existing codebases are things that cannot be taught while you work on some little projects. While, of course, you can learn those skills while getting a job in tech, sometimes those skills are necessary for you to get a job, making you stay in some kind of limbo where you need some skills to get a job, and those skills are precisely the ones you would get after the job.

In those cases, there’s still a way out of the limbo: you can contribute to open-source communities. Besides the value you are generating for the whole ecosystem, this can be an amazing selling point for your career and, since [Hacktoberfest](https://hacktoberfest.com/) is already around the corner, will be a great way to win a t-shirt or plant a tree too!

Now, let’s begin by teaching you how to actually do this.

## 2. First steps on Open-Source Contribution

### 2.1. Finding a project

First of all, we need to choose a project. If you’re a beginner, you’re probably looking for projects that have a few characteristics:

- It’s actively maintained.
- Has an open-source license that we can modify and use freely.
- It’s not insanely big (since these projects can have some really hard things to accomplish before submitting something).
- It must have good documentation on how to contribute.
- It must have well-characterized issues in order for you to search for something (in the case that you haven’t found the problem itself).

If you have matches in all of these points (or at least three of them), you’re good to go!

Throughout this article, I’m going to use our own repo, [Wasp Full-stack Framework](https://github.com/wasp-lang/wasp), since it gathers all the characteristics necessary for a good open-source repository. 

So, let me show you how to find all these characteristics:

- It’s actively maintained and the owners of the repo reply and care for the issues!
    - In the case of Wasp’s repo, the last commit was 13 hours ago, so, there’s definitely signs of life here!


![Last commit](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/gz3g6jl8hbvw66jmtizk.png)

- It’s not insanely big → Comparing an exaggerated example with the Linux repo (if you check it, you’ll see that all pull requests there usually take a lot of time to be merged since the project is so big)

![Linux repo](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/2cp5ptn8uwzb9qv81sey.png)

- It’s good to have a documentation on how to contribute
    - Searching for the docs, I found a file called [CONTRIBUTING.md](https://github.com/wasp-lang/wasp/blob/main/CONTRIBUTING.md) (which is a common name standard for contribution guidelines) and when we open it up:

![Contributing guidelines](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/8orfm43mllm2r5fs222o.png)

We have a whole documentation on how to start with things! Awesome!

- It’s good to have well characterized issues in order for you to search for something


![Issues](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/dk32gx185n9x9uoepobn.png)

Searching for the issues, we can easily see that they’re all labeled and that will help us A TON!

### 2.2. Searching for Issues

Great! Now that we have already chosen where we are going to contribute, let’s dive into the issues and search for something we want to do!

When searching for issues, the labels do us a great favor by already explicitly identifying all issues that can be good for newcomers! If you’re a beginner, **good first issues** and **documentation** are excellent labels for you to search for!


![Good labels to search for](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/jdyigcuhobk75vsi37oi.png)


![Issues on the repo labeled](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/yy6ek9gte26emr3bi182.png)

Opening the first issue, we can see that someone already manifested interest on it! So, since someone has already manifested interest in that one, let’s search for another one!

![The first issue](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/6phw5wcd5ik8bj8udj4u.png)

Finding another issue — it doesn’t look like anyone is working on the one below, so we can take it ourselves!

![Finding another issue](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/k1o3v1r5yzzcmotjbiqi.png)

By the way, it's of absolute importance that, when you find an issue, you comment and set yourself as assignee in order to let other people know that you're going to take the task at hand!


![Communicating](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/h2t7qjpv9syc2hi09xai.png)

In this case, GitHub is a great platform for us to discuss, but sometimes authors can be hard to find. In these cases, search for a link or a way to contact them directly (in the case of Wasp, they have a [Discord](https://discord.gg/rzdnErX) server, for example). Communicating your way through is really important to get things sorted out, and if you’re unsure of how to communicate well with people, you can read this other article [here](https://dev.to/llxd/how-to-deal-with-people-communication-5gef) and start to get the hang of it!

## 3. Guidelines for Contributing to Open-Source Projects

### 3.1. Reading the guidelines and writing some code

Now that we have selected a repo, an issue to work on and communicated with the authors, it’s time to check the guidelines for making Pull Requests (if you don’t know what this means, it’s basically a request to merge your modifications to the codebase, you can check some more basic git terms [here](https://rogerdudler.github.io/git-guide/) too). Sometimes, these guidelines are WAY too hard and sometimes they don’t even exist (that’s an awesome first issue actually), anyways look it up and see if you find something! 

You can check Wasp’s contributing guidelines [here](https://github.com/wasp-lang/wasp/blob/main/CONTRIBUTING.md) if you want to read it yourself! After reading it, it’s time to code the solution and get along with it.

Since the intent of this article is not to actually show the solving per se, I’ll skip this part and keep talking about the process itself. 

### 3.2 Handling Code Reviews and Feedback

It’s not rare that when we code things up (especially in open-source projects), there will be some problems. Code reviews and feedback are an amazing way for us to get the bigger picture and improve our code quality, so let’s check on how to properly read and answer code reviews and feedback.

We’re generally used to receiving criticism in a harsh way, so, when someone approaches you with feedbacks, we generally move into our defense zone. Unfortunately, these cases can teach you the wrong things as it’s generally a good way to think of feedbacks as **gifts**! Someone spent some time writing (or speaking) things in order for you get even better on what you’re trying to accomplish.

This does not mean that all feedback is well-made or that people will always provide great feedback. Sometimes, people can be harsh. However, as you receive more and more feedbacks, you will develop a sense of which feedbacks are genuinely meant to help you improve and which are simply baseless criticism. It is crucial to be open to receiving constructive feedbacks and not take them personally.

Let’s see an example of code review and feedback here:

![Code Review Example](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/ezsocw81o9d90erstx3n.png)

This is great feedback! It expresses the author’s opinion without being harsh and also suggests what to make in order to be perfect! The best way to answer this is simply:

- Thanking for the feedback
- Saying your opinion (agree or disagree) when it makes sense
- Work on it!

## Showcasing Contributions

After all that work, it’s time for us to showcase our contributions! Document it all. **GitHub** (or other git platforms), **personal portfolio sites, LinkedIn,** and other means of reaching people have become as important as resumes nowadays, so it’s really nice to have some statistics and data to display on:

- What open-source projects have you worked on? Try to think of this as writing a story. First, start by giving the initial context of the project and how it’s revelatory.
- How you contributed: Then, give the context of what you made, documentation, code, and problems you solved in general. Don’t forget to not focus a lot on the technical side since the person who could be reading this may not be technical.
- How big was the impact? Talk about how this affected the ecosystem; it can be as big or as small as you like. Never neglect the impact that changing documentation can have (remember that for us, programmers, the documentation is our source of truth, and fixes there are greatly appreciated).

Don’t forget to utilize the opportunity to engage with other developers and communities, make it so in order to get new connections and even greater opportunities later on! 

Now that the theory is set, let’s check a few examples on how I would showcase a few of my contributions:

### Case 1 - A big contribution

One of the ways to describe a big contribution is like this:

I made a few big contributions to a project called **[Coolify](https://coolify.io/)**, which was an open-source Heroku alternative. I refactored a lot of the UI, making it cleaner and more consistent throughout the application. Currently, more than 9000 instances are installed, and the UI affects all of them! You can check out the [contributions](https://github.com/coollabsio/coolify/commits?author=LLxD) here.

Of course, you can make this text as long or as short as you want, entering more detail about how this contribution was made and what exactly you did, but for this article, this is enough for you to get a general idea.

### Case 2- A small contribution

One way to describe a small contribution is like this:

I made a small change to the new documentation for [Sequelize](https://sequelize.org/)! I was just scrolling through the documentation and found this mistake that could lead others to weird debugging sessions, so as soon as I found it, I submitted a PR for them! You can check out the contribution [here](https://github.com/sequelize/website/commits?author=LLxD)!

## Conclusion

So, a lot was said, let’s make a quick recap on how to do contributions and how to showcase them:

- First of all, find a repo! If you don’t have any in mind, there loads of lists (like [this](https://github.com/collections/choosing-projects) one) that recommend some repos for you to take a look
- Search for an issue that is not being made and you can work on it, if you’re beginner, check for **documentation** and **good first issue** labels
- Comment and communicate that you’re going to fix the issue - take the opportunity to talk and get to know other developers
- Code, get you PR reviewed and ready to merge after the feedbacks
- Merge and showcase your contributions, showing that they are your way out of the Skill Paradox

![How to Open-Source](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/04lotyojmwdnzt7b2s8j.png)

The above steps can give you a really powerful experience in software engineering (which usually happens only when you’re already hired by a company). This is an awesome way to get some recognition while improving the open-source community — giving back to other developers and getting yourself out of the Skill Paradox!

And you? Have you contributed to open-source? Let me know in the comments below, and let’s share some experiences!