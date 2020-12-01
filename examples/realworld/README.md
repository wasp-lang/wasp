Realworld app
=================

[Realworld app](https://github.com/gothinkster/realworld) (RWA) is a benchmark for implementing a relatively complex app in a specific web dev technology.

Here, we implement it in Wasp, by following their [specification](https://github.com/gothinkster/realworld/tree/master/spec).

Todo:
- [x] User + auth (JWT).
- [x] Login and signup pages.
- [x] Settings page with logout button (no user deletion needed).
- [x] Profile page that shows basic user info.
- [x] Home page with navbar.
- [x] CRUD Articles (created from Markdown).
- [x] On /article page, show author username and date of creation (of article).
- [x] Render article content as markdown.
- [x] For Article, use special id which contains title in the name, and display it in url?
- [x] CR*D Comments on articles.
- [x] Add tags to articles.
- [x] Show Popular Tags on home page.
- [x] Favorite articles.
- [x] Make tags work again (Prisma problems!).
- [x] Following other users.
- [x] Paginated lists of articles (on profile page, on home page).
- [ ] Implement design (use Bootstrap 4 styling?).
- [ ] Display proper error messages on login/signup.
- [ ] Improve error handling in React, we don't do a really good job there.


Thoughts while implementing the RWA:
- I wish I could write less boilerplate -> declaration in Wasp, implementation in JS, it is boilerplaitish, easy to forget smth.
  Solution is probably to introduce inline JS and Wasp modules.
- I wish I could write forms faster. Writing forms in React is too complicated, I don't like that, all the details,
  thinking about e.target value and what not. Might be cool if Wasp had sub-DSL for forms -> it could generate HTML that is semantically correct (correctly uses labels and correctly constructs form), takes case of stuff like validation, and then wasper can apply any CSS they want.
- I wish I didn't have to write signup and login forms + supporting logic -> it is boring and I did nothing smart there.
- I wish I had typing at some moments, it was hard tracking which operation takes what and what it returns. Solution: TypeScript.
- `auth` should complain if entity does not have email and password fields, but it did not.
- It is tricky to remember to run `wasp db migrate-save/up` -> but not terribly.
- I wish common Prisma errors were automatically converted into corresponding HttpErrors (although, would that be a security problem?) so I don't have to handle them explicitly.
- I wish I could implement unit tests, but that can't be done for now in Wasp.
- I wish I could easily implement some e2e tests (would probably be great if we had nice integration with Cypress).
- I found error handling in React to be something I don't want to invest much time into when implementing first version of my app, so I ended up with having almost no error handling. It would be great if there was some reasonable, out of the box support for error handling in React provided by Wasp, that already does a lot for me without me doing much.
- Couple of times I forgot to call parseInt() on url parameter - I wish it was typed, or already parsed for me.
- If there is no match in router for URL, blank screen is shown. This is hard to debug and instead, we should send them to 404 page by default.
- When I was adding react-markdown, I didn't know which version to install because I did not know which React version are we using. Having smth like `wasp dependencies` that lists dependencies should help.
- I happened to return `null` from a query when entity was not found, which made things in React confusing for me since I was not able to differentiate, when using react-query, between `undefined` when there is no data available (initially, or during fetching) and when `null` is returned because nothing was found. I could manage this by checking `isFetching`, `isError`, and other states, also by using special state for initial state of the query, but this feels overly complicated. I probably should have thrown HttpError(404), but initially I didn't do this and just returned `null`. I am guessing this is a common "mistake" to me and it might be interesting to look into how we can make this whole situation simpler, and figuring out actually what is the best way to go about it.
- When I would use a couple of `useQuery` statements in the same React component, I would quickly start feeling like there is too much state to manage. Each of these statements can fail with an error, it could be loading, it could be in its initial state, it could be loaded, it could be fetching but it is keeping previous data (`keepPreviousData`) -> so each statement results in a bunch of possible states, and if we have a few of them, we end up with a crazy amount of combined states. Not to mention dependent queries and the fact that we can't return early and similar. Writing logic that handles all the cases seems hard and over the top, but on the other hand not handling everything feels lacking. Maybe I just need to put more effort into this and it is really not so bad as it seems? Anyway, it might be interesting investigating if this could somehow be simplified with Wasp, maybe by putting some structure to queries and providing some initial infrastructure for handling all these cases -> in this case we would probably have queries as a part of Wasp directly.
- Writing logic in React components to check if user is logged in and than doing stuff based on that -> that was boring. We should have ACL on frontend that takes care of this and just passes user as a prop.
- Right now we add Navbar manually to each component -> maybe it would be cool to have some kind of support for Layouts instead?
- Sometimes, in React, I wish I would not have to deal with data being `null` at some moments and just say that I don't want anything else to execute until that becomes non-`null`. But I guess this is where Suspense might come in?
- Ensuring in Operations that I don't fetch and return stuff, via Prisma, that is senstive and should not be sent to the client, is tricky and error prone. Solution might be to declaratively describe, on Entities, what are the permissions for each field, and that would than be enforced when using Prisma / in operations. And we can override this in specific operation, but at least we have a good default security.
- Implementing article tags was tricky, it demanded more attention and effort than I expected, and resulting code was also more complex than I expected. I wonder if there is space there for simplification.
- Pagination, although so common, demanded non-trivial effort and attention. Since pagination is full-stack feature, I am pretty sure we could offer some support here through Wasp and simplify it / provide something out of the box. There are two types of pagination we should take into consideration: skip+take and cursor-based.
- I had a situation where I had the operation dealing with entity E2 but E2 did not have to be provided under "entities" in Wasp becuause through Prisma nesting it was accessed through E1. However, I still wanted cache invalidation to work on frontend so I added E2 under "entities". Situation could also be opposite: we might need to provide E2 under "entities" to do some operation, although we know it does not affect cache invalidation. It might be interesting in the future to give more attention to this and make sure Wasp does its best to provide smooth experience in these situations -> maybe by some special support.
- Wasp file ended up really big and hard to navigate! Even if we don't have a real module support, just splitting it into multiple files that are then merged together might be a good start (and it might work since order is not important so far).
