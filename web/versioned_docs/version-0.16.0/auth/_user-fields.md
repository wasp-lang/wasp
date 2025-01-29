import { Required } from '@site/src/components/Tag';

The user entity needs to have the following fields:
- `id` <Required />

  It can be of any type, but it needs to be marked with `@id`

You can add any other fields you want to the user entity. Make sure to also define them in the `userSignupFields` field if they need to be set during the sign-up process.