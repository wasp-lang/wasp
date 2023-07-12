# Getting Started

Answers: What is Wasp and how do I get it?

How to get started using wasp:
- Why you would use it/what it is
- How to install it


# Essentials

Answers: I have Wasp, but how do I use it?

Explains the workflow of using wasp and the most important parts:
- How do I make a new project
- What's in the project
- How do I add more pages to the website
- How do I store stuff in a database

How do we know if a feature belongs here?
- Is it a "core feature?" = basically every wasp project is going to use this
  feature, and it will be added to the project very early on in development.

Other issues:
- "Running Actions" page is a bad name
- Should a section on dependencies go in here like in the current tutorial?


# Data Model

Answers: I know what entities, queries, & actions are. But how does it really
         work and what are the limits?

name needs improvement probably.

How do we know if a feature belongs here vs somewhere else?
- What's the broad purpose of the feature? If the answer includes data or database,
  then it probably belongs here.

# Advanced Features

Answers: What else does Wasp have to offer?

name could be a bit better: why are these advanced? its more like "more features"
maybe this section needs to be split

How do we know if a feature belongs here vs somewhere else?
- It's not a core feature.
- It's one conceptual "unit." As in, the feature doesn't have several complex
  subcomponents (like how auth has several providers).


# Authentication

Answers: I know auth exists in Wasp, now tell me all the options!

--> Why is this not an Advanced Feature? Auth providers have large differences
    in use and how they are configured. Auth UI is also a large topic, as well
	  as best practices for using auth.


# Project Setup

Answers: How do I add/configure X in my project?

--> Why are these not Advanced Features? Each are pretty boring and small, and
    not really an interesting feature of Wasp, just something that can be done.
