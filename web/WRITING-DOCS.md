# Writing Documentation

To keep Wasp's documentation organized and consistent, follow the guidelines set
out in this document when updating or writing new documentation.

## Organization

When adding a new page to the docs, read the guidelines below to determine the
best place for the page to go.

If a new section is created, add it to the list below and include some information
on how to decide if a page belongs in that section or not.

### Getting Started

Answers: What is Wasp and how do I get it?

### Essentials

Answers: Now that I have Wasp, what can I do with it?

Explains the workflow of using Wasp and the most important parts:
- How do I make a new project
- What's in the project
- How do I add more pages to the website
- How do I store stuff in a database

How do we know if a feature belongs here?
- A feature belongs here when it's a _core feature_: nearly every Wasp project is
  going to use this feature in some capacity.

### Data Model

Answers: How do persist data in Wasp?

How do we know if a feature belongs here?
- What's the broad purpose of the feature? If the answer includes data or database,
  then it probably belongs here.

The data model is a core part of Wasp. Why is this separate from Essentials?
- Some parts of the data model are "extras." Projects will use some of them, but
  it varies from project to project.

### Advanced Features

Answers: What else does Wasp have to offer?

How do we know if a feature belongs here vs somewhere else?
- It's not a core feature.
- It's one conceptual "unit." As in, the feature doesn't have several complex
  subcomponents (like how auth has several providers requiring long explanation
  of how to use each one).

### Authentication

Answers: I know auth exists in Wasp, now tell me all the options!

--> Why is this not an Advanced Feature? Auth providers have large differences
    in use and how they are configured. Auth UI is also a large topic, as well
	  as best practices for using auth.

### Project Setup

Answers: How do I add/configure X in my project?

--> Why are these not Advanced Features? Each are pretty boring and small, and
    not really an interesting feature of Wasp, just something that can be done.
    And they all relate to configuring the project/what can exist within the
    project.
