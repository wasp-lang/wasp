import { defineAbility } from '@casl/ability';
// import { AbilityBuilder, Ability } from '@casl/ability';
// import { PrismaAbility } from '@casl/prisma';

// Or should it just receive the whole context?
// Then we can attach it to any Operation, this `user` is already somewhat specific.
export function userAcl (user) {
  const userAbility = defineAbility((allow, forbid) => {
    // CASL doesn't care about terms 'create', 'read' and similar right?
    // So if we just define these that is not enough -> user needs to call them in correct place, right?

    // This means user can create any Task, with any values.
    // Do we want to restrict which fields can they set while creating Task?
    // Manual: they would need to call this before they create a Task.
    // Automatic: we call this for them upon any Prisma.Task.create() call (not sure how, but we do).
    //   But what if user is not allowed to create Tasks directly, however it is allowed to run an Action that as
    //   part of itself actually creates a Task (but in very specific/controled manner)?
    //   This is the whole story of what User can do vs what Operation can do. Hmmmm. But maybe that is ok.
    //   He can't create Task directly, but he can run that Action. And in that Action, when we create Task,
    //   we call Prisma.Task.create in such fashion that we make it clear that we don't care if user is
    //   allowed do to it or not. Therefore we need this ability to be able to specify in Action's code
    //   that specific Prisma operation is not to be checked against User's access rules.
    //   But what does this mean? Do we give them access to any Operation by default? Do they have to be given access
    //   for each of the Operations? If they are given access to Operation, do we still perform automatic checks on Entity
    //   level? I guess so hm!
    //   So if there is Operation O1 that creates entity E1 for User, but we want that Operation to be the only way for User
    //   to create entity E1, then we would forbid creation of E1 to User in general, allow usage of Operation O1 to User,
    //   and in O1 we would be clear that we don't want ACL checks to run automatically on Prisma creation.
    //   But do we really need to allow Operation O1 explicitely to User? What about other Operations?
    //   We need to enable all of them explicitely then?
    //   Or do we say that all Operations are implicitely enabled, however if they define their own rule for an Operation,
    //   then we use that?
    //   I think by default we want all the Operations to be non-public, however I am not sure if by default we want
    //   to completely forbid all of them to anybody? So either we by default allow them for any authed users, or we
    //   forbid them for everybody. If we forbid them for everybody, then they would need to both write Entity rules,
    //   plus write a rule for every Operation, to specify that user can access it.
    //   While it does make some sense to have Entity-oriented rules centralized,
    //   it makes a bit less sense to have Operation-oriented rules centralized - it would make more sense to have
    //   them collocated with their respected Operations.
    allow('create', 'Task')
    // This means user can read or update Task if it is owner of it.
    // Do we want to restrict which fields can they update? So they can't update createdAt, updatedAt and similar?
    //   But that could mess up with our semi-automatic approach, check below.
    // Manual: they would need to call this before they try to read or update any Task.
    // Automatic: we call this for them upon any Prisma.Task.find or Prisma.Task.update or updateMany or findMany and similar.
    //   But what if they use some other mechanism to find/update, for example they fetch some tasks while getting the user object? Can we also recognize that and run the checks?
    allow(['read', 'update'], 'Task', { userId: user.id })
    // For now we don't support any deletion. We don't really need to have this line though.
    forbid(['delete', 'Task'])

    // What is tricky is that while user might not be able to update some fields in Entity, our Action that is doing updating for them should probably be able to update some additional fields! So if we restrict the whole Action from doing anything outside of what user can do, we are being too restrictive! That is tricky, can we really do (semi)automatic checks then? Maybe not in this exact way. Btw for queries we said this is not such a big problem because often you do fetch only data that user can access, but with updating it can be quite different, I believe in updates you relatively often want to do some extra updates that user can't do directly (maintaining relationships between entities, updatedAt / createdAt times (although db can take care of those), ...).

    // Question: How testable will the code we produce in this fashion be? Easier to test or harder to test?

    // Summary of stuff above:
    // 1. Dev could define for each Operation who can access it. It could be something like:
    //    1. Anybody (public).
    //    2. Only authenticated users (private).
    //    3. Only authenticated users that satisfy certain conditions (~RBAC).
    //    4. Only authenticated users that satisfy certain conditions + Operation arguments + context (ABAC).
    //    All of these we could specify via CASL.
    //    Also, it would make sense that such role is specified next to the Operation.
    //    So role would target Operation, or maybe a group of Operations.
    //    So maybe in Wasp DSL, you can specify rules for specific Operation? Or a group of them?
    //    Just an ExtImport that leads to CASL rules?
    // 2. Dev could define for each Entity who can access it. It could be something like:
    //    1. Anybody (public).
    //    2. Only authenticated users, that might need to satisfy some conditions,
    //       and same goes for entity, and for context (ABAC).
    //    All of these we could specify via CASL.
    //    It does make sense that these rules are specified in a central place.
    //    But it makes even more sense that they are specified next to entities?
    //    So maybe in Wasp DSL, you can specify rules for specific entity?
    //    Just an ExtImport that leads to CASL rules?
    // 3. Operation-focused rules would be run automatically by Wasp when Operations is to be executed.
    //    We need to figure out default Operation rules though.
    // 4. Entity-focused rules would be run automatically by Wasp when Prisma is invoked for that Entity.
    //    Tricky question is: can we actually ensure we run them in ALL the situations? If we can't,
    //    is there still enough value in all this?
    //    We need to figure out default Entity rules though.
    //    Also, we need to allow devs to sometimes skip automatic application of these rules.
  })
  return userAbility
}

// ----------------------------------------------

// How devs could define ability for Query getTask:
// NOTE: Wasp would inject 'OperationName', and it expects you to use 'execute' as a verb.
//   Or maybe it could just inject its own 'allow' and 'forbid' functions that call CASL
//   with first two arguments preset to 'execute' and 'OperationName'.
export const getTaskAcl = (OperationName, args, context) => defineAbility((allow, forbid) => {
  // Allow anybody:
  allow('execute', OperationName)

  // Allow only authenticated users:
  if (context.user)
    allow('execute', OperationName)

  // Allow only authenticated users that satisfy certain conditions:
  if (context.user && user.role == 'Admin')
    allow('execute', OperationName)

  // Allow only authenticated users that satisfy certain conditions, and args, and context:
  if (context.user && user.role == 'Admin' && isBeforeNoon(getTime)) {
    allow('execute', OperationName, null, /* Can I describe here what arguments of operation have to look like? */)
  }

  // I am not quite sure if, when including operation arguments in the check, we want to describe them via 4th argument
  // in `allow`/`forbid`, or if we would rather handle them as injected `args` when defining rules?
  // Since we have to define rules for each call to the Operation anyway, in order to have fresh context,
  // then I don't see any reason why not to also pass `args` and use those directly, so that would be approach (2).
  // Approach (1) would be more interesting if we defined rules once per Operation, regardless of the call,
  // but that doesn't make much sense because we need fresh context anyway, we can't do much otherwise.
})

// Ok, so let's say that Wasp injects 'execute' and 'OperationName' inside already, then it would look like this:
export const getTaskAcl2 = (args, context, { opAllow, opForbid }) => {
  if (context.user)
    opAllow()
}

// But wait, if I am defining rules per Operation like this, why don't I just write this logic directly in the JS implementation of the Operation itself? What is the benefit of defining them first as CASL rules and then running them automatically? We are not using any fancy features of CASL like Prisma integration, or fields, or even conditions probably.
// Well, I guess we could play with regexes. Also, easier to reuse. But we can accomplish all of this with support for Operation middleware, can't we, once we add it? Ok hm so questionable.

// ----------------------------------------------

// How devs could define ability for Entity Task:
// Question: Where is this 'context' coming from? Are we assuming these rules are being checked in the context of Operation?
// Well they have to be, right? What if they are not hm? Is it some other context? I think in practice we want some context
// here that tells us if we have user, I am just not 100% sure if it is the same thing as context from Operation or not,
// maybe it is a subset instead.
export const taskAcl = (EntityName, context) => defineAbility((allow, forbid) => {
  // Allow anybody:
  allow(['read', 'create'], EntityName)

  // Allow only authenticated users:
  if (context?.user)
    allow(['read', 'create'], EntityName)

  // More complex case:
  if (context?.user && context.user.role == 'Admin') {
    allow(['update'], EntityName, { userId: context.user.id })
  }
})

// So above, Wasp cares if you use special strings to describe actions: 'read', 'create', 'update', 'delete'.
// Maybe these should be constants: W.Read, W.Create, ... , that we import in JS.
// Upon those, it will run automatic checks when Prisma commands are executed.
// You can also use other action verbs, but it will not do anything automatically about them. They are still valuable
// though since you can call them from code manually.
// As for EntityName, we could have that abstracted away by Wasp, like this:
export const taskAcl2 = (context, { taskAllow, taskForbid }) => {
  if (context?.user && context.user.role == 'Admin') {
    allow(['update'], { userId: context.user.id })
  }
}

// ------------------------------------------------

// Summary again:
// So looking at all this, we can conclude following:
// 1. We need Operation-focused ACL, which can already be done now, or we can use Wasp Operation Middleware, or we could have special support for it (but does it really bring anything?).
// 2. We need Entity-focused ACL
// All this sounds reasonable, the main idea is that devs define CASL rules in JS with a bit of special environment
// that Wasp sets up and then they register those with Wasp and they will be automatically executed.
// The main question that remains open is: can we effectively automate execution of Entity-focused ACL?

// TODO: How would we automate exeuction of Entity-focused ACL and can that work effectively / completely?
