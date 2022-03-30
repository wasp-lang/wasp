---
title: State symmetry in web applications
authors: [martijnfaassen]
tags: [webdev, wasp]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';
import InBlogCta from './components/InBlogCta';
import WaspIntro from './\_wasp-intro.md';

We're thinking about state in web frameworks a lot in the context of
[Wasp](https://wasp-lang.dev/).

When we load a UI of a typical web application, it needs state. At some point
somewhere a _query_ or selection of state takes place. A query selects and
shapes state from a larger database. We also need to be able to modify state
from the UI when the user does something. Let's call this an _action_.

We see the query and action pattern everywhere:

- Redux offers selectors and actions.
- Relational databases offer a select (query) and update/create/delete
  statements (actions).
- GraphQL has query and mutations as well.

Wasp offers it query and mutation too, building on React Query. You can query
(select) data from the server, and update it again using a server action.

Now let's look at REST APIs. They also offer selection (`GET`) and actions
through `POST`. But most REST APIs also include an additional property: state
symmetry.

In a CRUD REST API you could create a new resource (using, say, JSON) with
`POST` (for instance `POST /user` to create a new user), update an existing
resource using `PUT` (`PUT /user/3`), and get it again with `GET` (`GET /user/3`).

We see symmetry here: the JSON returned by `GET` is identical, or at least very
similar, to the JSON supplied by `POST` and `PUT`.

This state symmetry is something a user of a web application expects. Let's say
I go to my user page, which has a form in it to edit my user information. If I
edit it, save it, and then reload the web page (or go back to it again later),
I expect to see the same form with the same data.

State symmetry is also present in the way we store data. Let's look at a
relational database. The ultimate shape of what we can query and what we modify
is the same: a record in a table, even though we may break that symmetry when
we create queries and mutations over this data.

An object relational mapper (ORMs) also establishes this symmetry. The same ORM
mapped object you use to retrieve data is also used to manipulate it.

Symmetry is a restriction of what you're allowed to do. The ability to query
and perform actions doesn't mandate any symmetry. To establish symmetry you
have to restrict the shape of the query result to the same shape an action
takes.

At first glance restrictions seem to be unnecessarily confining. Why not let
the developer always be free to create whatever queries and actions are
appropriate to the task at hand and not worry about symmetry?

But restrictions are also useful: restrictions give us powers. They give a
framework properties to build on.

What properties could we get from symmetry?

- Less to remember: once the developer understands the shape of the data
  retrieved from the server, they know they can exploit that same shape for
  sending data back to the server. So, you can just edit what you got from the
  server on the client before saving it back again.
- Automation: instead of making a developer write queries and actions
  themselves that retrieve and update exactly the right information (not too
  much, not too little) to build an UI, we can automate this.
- Getting security right is hard, especially for fine-grained rules like "this
  field is read-only". With automation we can let the developer declare which
  fields are read-only instead of having to worry about writing complicated
  update rules.
- Many web applications are CRUD applications, which have inherent symmetry.
  Symmetry can help us build high-level CRUD construction tools into the
  framework.

We want to offer these properties to users, so we should try to pursue this.
That doesn't mean that we think non-symmetric queries and actions are bad:
they're great to have around! We need them for those user interfaces
that are inherently unsymmetrical, like a collection UI.
