Realworld app
=================

[Realworld app](https://github.com/gothinkster/realworld) is a benchmark for implementing a relatively complex app in a specific web dev solution.

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
- [ ] Make article tags deleteable. Right now I have something but it does not delete them from the database!
      I think I am using Prisma wrong here, the problem is probably in connectOrCreate when updating,
      it does not seem to delete the ones that are missing now.
- [ ] Show Popular Tags on home page.
- [ ] Favorite articles.
- [ ] Following other users.
- [ ] Paginated lists of articles (on profile page, on home page).
- [ ] Use Bootstrap 4 styling.
- [ ] Use proposed url routes.

What I wish I could do (& other notes):
- Write less boilerplate -> declaration in Wasp, implementation in JS, it is boilerplaitish, easy to forget smth.
- Write forms faster. I need to know too much about how to write forms in React, I don't like that, all the details,
  thinking about e.target value and what not. Might be cool if Wasp had sub-DSL for forms -> it could generate HTML that is semantically correct, and then they can apply any CSS they want.
- Didn't have to write signup and login forms.
- I wish it was typed.
- `auth` should complain if entity does not have email and password fields.
- It is tricky to remember to run `wasp db migrate-save/up`.
- I wish common Prisma errors were automatically converted into corresponding HttpErrors (although, is this security problem?).

Can't implement for now due to lacking support in Wasp:
- Unit tests
