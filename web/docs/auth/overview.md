---
title: Using Auth
---

### TOOD: intro

### TODO: mention `userEntity`,  `externalAuthEntity`, `methods`, `onAuthFailedRedirectTo`, `onAuthSucceededRedirectTo`


### TODO: list available auth methods

### TODO: write about the password validation rules here since they apply to username and email auth

There are some default validation rules for the user passwords:
```ts
const passwordValidators = [
  { validates: PASSWORD_FIELD, message: 'password must be present', validator: password => !!password },
  { validates: PASSWORD_FIELD, message: 'password must be at least 8 characters', validator: password => password.length >= 8 },
  { validates: PASSWORD_FIELD, message: 'password must contain a number', validator: password => /\d/.test(password) },
];
```

### TODO: getting logged in user on the client and the server

### TODO: protecting a page with `authRequired`

### TODO: logout action