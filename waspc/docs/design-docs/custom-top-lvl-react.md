# Support for custom code at top level React component

Generated React code is structured hierarhically, with one top level component.

That top level component (let's call it TLRC - Top Level React Component) is currently not accessible by Wasp developer.

However, they might need to be able to customize that top level component, e.g. if they want to add Redux to their app, or somemthing else that affects app on a "global" level.

Thus, we want to enable developers to be able to customize that top level component.

While at this, we might also want to think about layouts, since they also have to do with very top of React component hierarchy and they wrap other components under them.

Finally, we should also try to think out of the box -> maybe there is another solution to the problems here than allowing devs to customize the top level React component.
We should focus first on use cases and then decide from there.

## Use cases

TODO: Specific use cases when this is needed / useful.

## Requirements

TODO

## Implementation 

### Language design

TODO

### MVP

TODO

