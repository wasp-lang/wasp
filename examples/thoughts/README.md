Thoughts
==========

**Thoughts** is a note-taking app organized around the concept of hashtags.

Run `wasp start` to start the app in development mode.

This app is deployed at https://wasp-thoughts.netlify.app/ .

## TODO

## How it felt so far to build this app in Wasp

Here I write down how I felt while developing this app, so we can use this feedback in the future to improve Wasp. Subjective feedback is also written down.

- CSS is hard. Writing CSS globally is not fun. I would like to write it somehow better (inline)? I would also like to use a pre-processor (sass, stylus).
- I was modifying the entity and couldn't perform migration because it would destroy data. So I emptied the database. I did that by using `wasp clean`, but I am not sure if others would know they can do it this way, plus this method works only for SQLite.
- I had an error coming from an action -> it was not super clear from the error message where it came from.
- I have to remember to do migrate-dev.
- I have to remember to restart Wasp when I add dependency.
- Could we have immutable data in DB? Is that posible? This is just thought, we probably shouldn't think about this.
- UI is hard.
- Operations code and declaration should be closer and less boilerplatish.
- If calling now non-existent operation that existed previously, 404 is returned -> very hard to figure out what the error is from this! This happens because old generated operation file remains in the generated FE code. If it was removed, error message would be better.
- Wasp declares action where import stmt leads to nowhere -> hard to debug the error!
- I added creation of tags (via Prisma's `connect` mechanism) to action that had `Thought` under `entities` and forgot to add `Tag` under `entities` in action declaration in .wasp code! Is there a way to force this?
- `wasp db migrate-dev` after `wasp clean` takes long time with no output.
- Figuring out what should be a new page and what should not -> hard. Details: I wasn't sure if I should do multiple pages that share some components (sidebar, navbar), or if I should have just one page and then use in-page router for deciding what to show in the center of the page. This dillema would be present in pure React app also.
- Handling errors in React and fetching data -> boring, also not sure how to do it.
- I forgot to run `wasp db migrate-dev` after I switched db.system to PostgreSQL, and it took me some time to figure out why it is still using SQLite.
- I got a message from Prisma that I should remove my migrations directory. It can be confusing for the newcomers as to what really needs to be done.
- When I came back after some time, I forgot: how to run the database, what is the name of the heroku app. I luckily had command for running the database in the terminal history, and for deploying I opened the wasp docs and followed instructions.
- When deploying after making changes to schema that require data migration (adding required fields to entities), I deleted the whole database on Heroku. If I actually had to do a data migration, I am not sure how I would have done it though, so that is a very important question and something we need to figure out.
