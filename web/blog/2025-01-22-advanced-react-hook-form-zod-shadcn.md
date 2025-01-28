---
title: "Building Advanced React Forms Using React Hook Form, Zod and Shadcn"
authors: [martinovicdev]
image: /img/forms/advanced-react.jpeg
tags: [webdev, wasp, react, forms]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';

Developers usually say, "Once you've seen one form, you've seen them all," but that's not always true. Forms can range from basic to highly complex, depending on your project's needs.

If you're working on a simple contact form (e.g., email, subject, message), [check out part one](https://wasp-lang.dev/blog/2024/11/20/building-react-forms-with-ease-using-react-hook-form-and-zod). It covers the basics of React forms using React Hook Form, Shadcn UI, and Zod.

But what if your forms need advanced behaviors and validations? That's where this guide comes in. Whether you need conditional fields, custom validation logic, or dynamic form generation, this article will help you take your React forms to the next level.

![image](https://media4.giphy.com/media/xXXhLy1M4RML6/giphy.gif?cid=7941fdc6u3noalhw3o2rc6z04i9g7a16dk21oh3wkpbiqxdk&ep=v1_gifs_search&rid=giphy.gif&ct=g)

## What else is there to validate?

💡 Starting Code: The code we'll use here builds on the final version from part one. You can [find it here on GitHub](https://github.com/martinovicdev/wasp-form-tutorial).

In [part one](https://wasp-lang.dev/blog/2024/11/20/building-react-forms-with-ease-using-react-hook-form-and-zod), we explored the basics of form validation, including:

- **Type validation:** Ensuring input types match form fields.
- **Length validation:** Checking input length.
- **Maximum value validation:** Setting value limits.

Now, let's tackle **advanced validation scenarios**. These include:

- Custom validation logic.
- Validation tied to other fields (e.g., comparing values).
- Validation based on external data, like database values.

To demonstrate these, we'll expand the form with new fields: **username, address, postal code, city, and country**. We'll start by applying basic validations to these fields.

Here's your initial Zod schema:

```tsx
const formSchema = z
    .object({
      name: z.string().min(1, { message: 'Name is required' }),
      surname: z.string().min(1, { message: 'Surname is required' }),
      email: z
        .string()
        .email({ message: 'Invalid email address' }),
      dateOfBirth: z.date().max(new Date(), {
        message: 'Date of birth cannot be in the future',
      }),
      premiumUser: z.boolean(),
      username: z
        .string()
        .min(1, { message: 'Username is required' }),
      address: z.string().min(1, { message: 'Address is required' }),
      postalCode: z.string().min(1, { message: 'Postal code is required' }),
      city: z.string().min(1, { message: 'City is required' }),
      country: z.string().min(1, { message: 'Country is required' }),
    });
```

Expanding the form is straightforward since we're working with standard input fields. However, remember to update your **Prisma schema**, queries, and actions to include the new values.

The final form should look similar to this:

![image.png](/img/forms/form-2.png)

The most important part of the equation will be Zod's `refine` and `superRefine` refinement functions. We use them to implement custom validation logic, which cannot be represented with simple types. 

### Refine

**Refine** is a straightforward method for custom validation. It takes two arguments:

1. A **validation function**.
2. Optional **additional options** (e.g., custom error messages).

If the validation function returns `true`, the validation passes; otherwise, it fails.

Refine also supports **asynchronous functions**, which will play an important role in this article.

### SuperRefine

A more advanced alternative to `refine` is `superRefine`, which provides greater flexibility for customizing validations.

The key difference with `superRefine` is that it allows you to add multiple validation issues by manually throwing errors using `ctx.addIssue`. If you don't explicitly add an issue, the error won't be triggered.

Another significant distinction is that `superRefine` operates on the **entire schema** rather than a single field, making it ideal for scenarios where validation depends on relationships between multiple fields.

![doggo](https://media1.giphy.com/media/1PstC8Jk5enGx3Gufw/giphy.gif?cid=7941fdc6xukyg2lvj7uap5dtac4x9w2bg0nvlbe1oyylfg28&ep=v1_gifs_search&rid=giphy.gif&ct=g)

### Finding this article useful?

[Wasp](https://wasp.sh/) team is working hard to create content like this, not to mention building a modern, open-source React/NodeJS framework.

The easiest way to show your support is just to star Wasp repo! 🐝 Click on the button below to give Wasp a star and show your support!

![https://dev-to-uploads.s3.amazonaws.com/uploads/articles/axqiv01tl1pha9ougp21.gif](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/axqiv01tl1pha9ougp21.gif)

<div className="cta">
  <a href="https://github.com/wasp-lang/wasp" target="_blank" rel="noopener noreferrer">
    ⭐️ Thank You For Your Support 💪
  </a>
</div>

## Custom validation

A great starting point for **advanced form validation** is implementing **custom validation**. This comes into play when requirements fall outside the “usual” set of rules.

For example, let's validate a **username** to ensure it **doesn't contain spaces**. Spaces in usernames can cause UI issues or user confusion, so we'll disallow them.

While `refine` is a common choice for custom validations, we'll use `superRefine` instead. This allows us to perform **multiple checks** simultaneously, making it ideal for more complex validation scenarios. With this in mind, we'll add a **`superRefine`** function at the end of our validation schema to include two checks: one for the **username** and another for the **city**.

Believe it or not, some cities in the world have numbers in their names (as noted in this [Wikipedia article](https://en.wikipedia.org/wiki/List_of_places_with_numeric_names)). While the city field should initially validate as a string, we also want to ensure it doesn't consist exclusively of numbers. If it does, we'll throw an error.

To throw the error, we'll use `ctx.addIssue`, specifying the **error code**, **message**, and **path**.

Similarly, we'll validate the username to ensure it doesn't contain any space characters, applying the same structured error handling as for the city.

```tsx
//this comes after our validation schema 
.superRefine((data, ctx) => {
      if (data.username.includes(' ')) {
        ctx.addIssue({
          code: z.ZodIssueCode.custom,
          message: 'Username cannot contain spaces',
          path: ['username'],
        });
      }

      if (/^\d+$/.test(data.city)) {
        ctx.addIssue({
          code: z.ZodIssueCode.custom,
          message: 'City name cannot be purely numeric',
          path: ['city'],
        });
      }
  });
```

## Conditional validation

Conditional validation is useful when the validity of one field depends on the value or validity of another. It's a common and practical type of advanced validation.

For this example, we'll implement a simple condition: if the customer's country is the United Kingdom, their postal code must follow the UK-specific format.

To achieve this, we'll first check if the country field is set to "UK." If it is, we'll validate the postal code against a regex pattern. Using `superRefine` at the end of the validation schema is particularly convenient here, as it provides easy access to the values of the entire form, allowing us to implement this conditional logic seamlessly.

```tsx
//this comes after our validation schema 
.superRefine((data, ctx) => {
      if (data.username.includes(' ')) {
        ctx.addIssue({
          code: z.ZodIssueCode.custom,
          message: 'Username cannot contain spaces',
          path: ['username'],
        });
      }

      if (/^\d+$/.test(data.city)) {
        ctx.addIssue({
          code: z.ZodIssueCode.custom,
          message: 'City name cannot be purely numeric',
          path: ['city'],
        });
      }
      
      
     if (!isValidUKPostcode(data.postalCode, data.country)) {
      ctx.addIssue({
        code: z.ZodIssueCode.custom,
        message: 'Invalid UK postal code format (e.g., SW1A 1AA)',
        path: ['postalCode'],
      });  
    }
  });
```

## Asynchronous validation

Async validation might sound complex, but with React Hook Form, it's surprisingly straightforward and builds on what we've already covered. The only additional requirement is a function to check whether the username or email already exists.

For this example, we'll focus on validating the username, but the same approach can be applied to email or any other field. I've created two functions that return a `Promise<boolean>` to make their usage within the Zod validation schema as simple as possible.

As shown in the code examples, the schema uses these functions to return `true` or `false` based on whether a customer with the given username or email already exists.

```tsx
  const checkUsername = async (value: string): Promise<boolean> => {
    return getCustomersWithUsername({ username: value }).then((data) => {
      return !!data;
    });
  };
```

In the Zod schema, we'll use the `refine` function to validate the field. However, we won't always perform a database check.

For example:

- If the username hasn't changed (i.e., it's the same as the current customer's username) or is still empty, we'll skip the database query.
- Otherwise, we'll check the database for an existing customer with the same username. If a match is found, it will trigger a validation error.

This approach ensures efficient validation by minimizing unnecessary database queries.

```tsx
username: z
        .string()
        .min(1, { message: 'Username is required' })
        .refine(
          async (username) => {
            if (username === customer.username && customer.username !== '')
              return true;
            return !(await checkUsername(username));
          },
          { message: 'Username already exists' }
        ),
```

Finally, the entire Zod schema should look like this: 

```tsx
  const formSchema = z
    .object({
      name: z.string().min(1, { message: 'Name is required' }),
      surname: z.string().min(1, { message: 'Surname is required' }),
      email: z
        .string()
        .email({ message: 'Invalid email address' })
        .refine(
          async (email) => {
            if (email === customer.email && customer.email !== '') return true;
            return !(await checkEmail(email));
          },
          { message: 'Email already exists' }
        ),
      dateOfBirth: z.date().max(new Date(), {
        message: 'Date of birth cannot be in the future',
      }),
      premiumUser: z.boolean(),
      username: z
        .string()
        .min(1, { message: 'Username is required' })
        .refine(
          async (username) => {
            if (username === customer.username && customer.username !== '')
              return true;
            return !(await checkUsername(username));
          },
          { message: 'Username already exists' }
        ),
      address: z.string().min(1, { message: 'Address is required' }),
      postalCode: z.string().min(1, { message: 'Postal code is required' }),
      city: z.string().min(1, { message: 'City is required' }),
      country: z.string().min(1, { message: 'Country is required' }),
    })
    .superRefine((data, ctx) => {
       if (!isValidUKPostcode(data.postalCode, data.country)) {
        ctx.addIssue({
          code: z.ZodIssueCode.custom,
          message: 'Invalid UK postal code format (e.g., SW1A 1AA)',
          path: ['postalCode'],
        });
      }

      if (data.username.includes(' ')) {
        ctx.addIssue({
          code: z.ZodIssueCode.custom,
          message: 'Username cannot contain spaces',
          path: ['username'],
        });
      }

      if (/^\d+$/.test(data.city)) {
        ctx.addIssue({
          code: z.ZodIssueCode.custom,
          message: 'City name cannot be purely numeric',
          path: ['city'],
        });
      }
    });
```

I hope this article has made working with forms easier and more understandable! If you want to see to see the complete source code of the application, check out the [GitHub repo here](https://github.com/martinovicdev/wasp-advanced-form-tutorial). 

Please consider starring [**Wasp**](https://github.com/wasp-lang/wasp) on GitHub if you liked this post! Your support helps us continue making web development easier and smoother for everyone. 🐝