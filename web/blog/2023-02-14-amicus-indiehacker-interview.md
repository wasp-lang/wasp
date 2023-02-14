---
title: "From Idea to Paying Customers in 1 Week: An Interview with Amicus.work"
authors: [vinny]
image: /img/amicus-usecase/amicus-homepage.png
tags: [SaaS, IndieHacker, Solopreneur, Interview]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import ImgWithCaption from './components/ImgWithCaption'

<br/>

> *I guess it was less me having an idea and validating it, and more a valid idea coming to me and biting me in the ass, and me thinking ‘oh hey…’* 
> &nbsp; — **Erlis Kllogjri**

<br/>

Erlis Kllogjri, a computer engineer and the creator of [Amicus.work](https://amicus.work), went from idea to paying customers in just one week 🤯! In this interview, he tells how sometimes the best ideas come looking for you, and how moving quickly can help you stay inspired, motivated, and pull in your first satisfied customers.

<br/>
<ImgWithCaption
    alt="Amicus Homepage"
    source="/img/amicus-usecase/amicus-homepage.png"
    width="500px"
/>

<!--truncate-->

<hr/>

### Before we begin with the unlikely origin story of Amicus.work, can you tell us a bit about what it is?

[Amicus](https://amicus.work) is a SaaS tool for legal teams that helps keep you organized and on top of your legal needs. Think of it like "Asana for lawyers", but with features and workflows tailored to the domain of law. 

It allows attorneys and their clients to easily track the progress of the legal case they are dealing with, and collaborate with others involved in the case, all in one central location. For example, deadline reminders help with not missing key dates and workflow visualization allows lawyer and client to see where the process is stuck, and get it unstuck.


### Your time from initial idea to working MVP seemed fast. How long was it and how did you achieve it so quickly?

From the initial discussions to the launch of the initial prototype was probably a week or so. This is even quicker than it sounds because I was working a full time job at the time. The speed [of execution] was fully enabled by [Wasp](https://wasp-lang.dev), a full-stack web app framework.

I was looking at other solutions, but none of them were full-stack and sounded like a lot of work just to stitch everything together and get started. I just wanted to get the job done and didn’t care about picking the stack specifics myself. [Wasp](https://wasp-lang.dev) was really helpful as it set me up with the best practices and I had everything running in just a few minutes!

### How were you able to get these first customers so quickly?

The first user is a little bit of a cheat because I know them — my brother, who is a lawyer. But having read about other entrepreneurs, this is not that uncommon. Sometimes the first users we know are ourselves, sometimes they’re family or friends, and sometimes it’s someone you sought out. But I think it was important to have the client before the idea, because that way you have the problem before the solution.

### What advice would you give to other Solopreneurs regarding the validation process?

With regard to process, I spent a lot of time having discussions with my first user - my brother. The better you know the first user, the more careful you need to be I think. They’re going to give you slack and support your ideas. You don’t really want that, so you have to dive deeper into each problem/solution - like asking 5 why’s, so you can be more objective. 

Once more users came on, I began sending out surveys about the key things I wanted to know. I also started setting up SQL queries and adding logs to answer questions about what kind of user was using what features the most etc. Being a solopreneur means you have to be even more careful about what you spend your time building.

MRR is low at the moment, around ~$90, and the first goal is to get to an MRR around ~$2,000. At that point I would be able to throw more time and resources at the application, increase the utility, and kick off a virtuous cycle of more revenue and utility.

### That’s great. So rather than trying to find a clever idea, the idea found you.

It’s funny because I have all of these harebrained ideas that I’m always kicking around, thinking about how to validate them: MVPs, setting up a landing page that gets emails or deposits, etc. 

Meanwhile my brother was telling me about this pain of managing matters that no tool really helped with. Clients want to know where the process is, how many steps are left, how they need to be reminded of important dates like contract deadlines, etc. So I agreed to build something to see if it would help. [Wasp](https://wasp-lang.dev) was instrumental here because if these steps had taken too long I would have probably lost interest and gotten distracted by something else. It allowed me to abstract all the details of a full stack app and focus on the product itself.

I built the prototype and it was TERRIBLE, it hurts to think back on that first version. But it was being used, and terrible though it was, it was still providing utility. And that was the point where it clicked the idea would work - if my first crude attempt was useful, and it would only get better with each iteration, there is a space here to provide so much value that some of it can be captured.

I guess it was less me having an idea and validating it, and more a valid idea coming to me and biting me in the ass, and me thinking ‘oh hey…’.

### What’s been the biggest lessons learned as a result from building Amicus? If you could do it over, what would you do the same and what would you do differently?

I think one of the things I would do differently is spend a little more time at the beginning getting a full grasp on the use cases. I tried doing this with interviews with the first client. However once what was intended was built, I come across all of these questions that weren’t initially obvious. I have seen PMs in the past create paper mockups (or using [Figma](https://www.figma.com) if there is time) and walking a person through what they would do - then all of a sudden these assumptions you both had bubble up. [I] would probably do something like that if possible.

### What were your biggest concerns before getting started building Amicus? What problems did you know you wanted to avoid and how did you successfully achieve those goals?

[My] biggest concern when getting started building [Amicus](https://amicus.work) was honestly that it would go to the unfinished project graveyard. Once again, [Wasp](https://wasp-lang.dev) was key to resolving this. Being able to remove most of the redundancy involved in making a full stack app really helped me. It allowed me to focus on the interesting problems.

One of the things I have been trying to be careful to avoid is building things that aren’t needed or solving problems that don’t exist. It is very easy to get into the trap of thinking ‘oh this would be cool’ or ‘oh this extra thing might need to be build incase…’. I have been trying to be rigorous about validating features before building them (by talking to users or through the surveys), and unless theres a good reason to believe something is a problem I don’t spend my time fixing it. This is very hard, but it has allowed me to focus.

<br/>
<ImgWithCaption
    alt="Wasp Logo"
    source="/img/wasp-logo-wide.png"
    width="500px"
/>

### Have you done any form of advertising? press releases? How are you spreading the word about Amicus at the moment?

No advertising yet and no press releases either. Right now spreading of the word is mostly through word of mouth. Advertising can be a money pit, especially when you don’t know what you’re doing (and I probably don’t know what I am doing) so I want to first make sure I am at the point where users feel passionate enough about Amicus to where they tell others about it. Once I get there, advertising can have a bigger return even with my fumbling. 

### What made you decide to go it alone as a “Solopreneur”? Were you confident that you’d be able to tackle the challenge alone, and if so why?

This wasn’t so much a decision as something that came about one decision at a time. What initially started as just a handy app for my brother to use, naturally grew in scope and utility, and all of a sudden there was a business and I effectively became a solopreneur. Although I’ve always wanted to be an entrepreneur, I didn’t realize I had become a solopreneur until after the fact.

<hr/>

*Want to stay in the loop? → [Join our newsletter!](/#signup)*

