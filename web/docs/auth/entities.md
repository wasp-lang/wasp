---
title: Auth Entities
---

import ImgWithCaption from '../../blog/components/ImgWithCaption'

When you want to add authentication to your app, you need to specify a `User` entity in your Wasp file. This entity is a "business logic user" entity and is different from the `Auth` entity that is created behind the scenes.

You use the `User` entity to define the fields that you want to store for each user. For example, you might want to store the user's name or address. You can also use the `User` entity to define the relations between users and other entities in your app. For example, you might want to define a relation between a user and the tasks that they have created.

TODO: create a diagram `(User <1-1> Auth <1-N> AuthIdentity)`
<ImgWithCaption alt="Auth Entities Diagram" source="img/auth-entities/model.png" caption="Auth Entities Diagram"/>

On the other hand, the `Auth` entity is created behind the scenes and is used to store the user's login credentials. You as the developer don't need to care about this entity most of the time. In the case you want to create a custom login or signup actions, you will need to use the `Auth` and `AuthIdentity` entities.

## How are the Auth Entities Connected
TODO: explain which auth entities are created behind the scenes (Auth and AuthIdentity)

<ImgWithCaption alt="Example of Auth Entities" source="img/auth-entities/model-example.png" caption="Example of Auth Entities"/>

## `Auth` Entity

TODO: explain the fields of the `Auth` entity

## `AuthIdentity` Entity

TODO: explain the fields of the `AuthIdentity` entity

## Custom Login and Signup Actions

TODO: how do you create a new user by hand: show the Prisma code that can be used to create a new user

## Helper Methods

TODO: helper methods to access the username and email of a user