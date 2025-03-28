---
title: 'Building React Forms with Ease Using React Hook Form, Zod and Shadcn'
authors: [martinovicdev]
image: /img/forms/banner.webp
tags: [webdev, wasp, react, forms]
---

import Link from '@docusaurus/Link';
import useBaseUrl from '@docusaurus/useBaseUrl';

import InBlogCta from './components/InBlogCta';
import WaspIntro from './_wasp-intro.md';
import ImgWithCaption from './components/ImgWithCaption'

Forms are something every developer encounters, whether as a user or on the developer side. They’re essential on most websites, but their complexity can vary wildly—from simple 3-field contact forms to giga-monster-t-rex, multi-page forms with 150 fields, dynamic validation, and asynchronous checks. 

In this post, we’ll explore how React Hook Form, Zod, and Shadcn can be used to create an adaptable, developer-friendly solution that handles a wide range of form requirements with ease.

![david and victoria meme](/img/forms/meme.webp)

## The form we’ll be building

Here’s the form we’ll be developing in this post. I plan on writing another post about an advanced use of forms that will have even more complexity as a follow-up, so stay tuned 😃

![example form](/img/forms/form.png)

## Meet the tools

Let’s look at the stack we’ll use to build and manage our forms.

### **React and Wasp**

- Framework: [**Wasp**](https://github.com/wasp-lang/wasp) (full-stack framework for React, Node.js, and Prisma).
- Enables fast, efficient full-stack web development and deployment with React.

### **React Hook Form**

- Lightweight library for crafting forms in React, mainly via its `useForm` hook.
- Handles form validation, error management, and offers flexible validation method and integration with various UI component libraries.

### **Zod**

- TypeScript-first validation library for creating detailed, reusable validation schemas.
- Integrates with TypeScript types to keep validation unified and avoid duplication.

### **Shadcn/UI**

- Collection of reusable UI components which are embedded directly in project, which allows developers to take only what they need and customize those components as well.
- Offers built-in support for React Hook Form and Zod.

Here’s an example snippet showcasing a form field in Shadcn library:

```tsx
<FormField
  control={form.control}
  name="name"
  render={({ field }) => (
    <FormItem>
      <FormLabel>Name</FormLabel>
      <FormControl>
        <Input {...field} />
      </FormControl>
      <FormMessage />
    </FormItem>
  )}
/>
```

Even if you prefer using a different flavor of the stack, as long as you stick with React and RHF, this is still a valid example that will get you going.

## Let’s build a simple user dashboard

The application we'll use to demonstrate basic forms is an admin panel with essential CRUD operations. It will include email and password authentication and consist of two pages: a main screen displaying a table of all users, and a user creation page, which will be the star of this article.

![example data](/img/forms/data.png)

![example form](/img/forms/form.png)

Our form will include validation to ensure users cannot submit it (i.e., create a new user) without meeting the specified requirements. The User object is an excellent candidate for validation examples, as it contains a variety of data types suitable for different validations: strings, dates (e.g., date of birth), email strings, and booleans (e.g., premium user status). The complete Prisma schema file is shown below.

```sql
model Customer {
  id    Int    @id @default(autoincrement())
  name  String
  surname String
  email String
  dateOfBirth DateTime
  premiumUser Boolean
}
```

To jumpstart our project, we’ll use a predefined [Wasp template](https://wasp.sh/docs/project/starter-templates) with TypeScript, called **todo-ts**. This template comes with ready-made components and routing for authentication, including login and signup screens. It also offers a solid example of how CRUD operations work in Wasp, ideal if you’re new to the framework. Additionally, we’ll leverage the new Wasp TypeScript SDK to manage our configuration, as it provides extended flexibility for customization.

### Finding this article useful?

[Wasp](https://wasp.sh/) team is working hard to create content like this, not to mention building a modern, open-source React/NodeJS framework.

The easiest way to show your support is just to star Wasp repo! 🐝 But it would be greatly appreciated if you could take a look at the [repository](https://github.com/wasp-lang/wasp) (for contributions, or to simply test the product). Click on the button below to give Wasp a star and show your support!

![https://dev-to-uploads.s3.amazonaws.com/uploads/articles/axqiv01tl1pha9ougp21.gif](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/axqiv01tl1pha9ougp21.gif)

<div className="cta">
  <a href="https://github.com/wasp-lang/wasp" target="_blank" rel="noopener noreferrer">
    ⭐️ Thank You For Your Support 💪
  </a>
</div>

## Putting it all together - Zod schema + React Hook Form instance + layout

To work with forms, we’ll start by defining a Zod validation schema. Our form has three data types: strings, a date, and a boolean. We’ll apply validation to most fields: `name` and `surname` are required, while `email` utilises the built-in e-mail validation. Zod simplifies validating common string types with built-in validations for different types, like emails, URLs, and UUIDs, which is helpful for our email field.

For additional validations, the date can’t be set to a future date, and the `premiumUser` field simply needs to be a boolean. Zod also provides default validation error messages, but these can be customized. For example, instead of `name: z.string().min(1)`, we could specify `name: z.string().min(1, 'Name is required')`.

 

```tsx
  const formSchema = z.object({
    name: z.string().min(1, { message: 'Name is required' }),
    surname: z.string().min(1, { message: 'Surname is required' }),
    email: z.string().email({ message: 'Invalid email address' }),
    dateOfBirth: z
      .date()
      .max(new Date(), { message: 'Date cannot be in the future' }),
    premiumUser: z.boolean(),
  });
```

Our form is managed by the `useForm` hook from [React Hook Form](https://react-hook-form.com/docs/useform), which provides extensive options for handling and validating form values, checking errors, and managing form state. To integrate our Zod validation schema, we’ll use a Zod resolver, allowing React Hook Form to apply the validations we defined earlier.

The form’s `defaultValues` are derived from the customer object. Since this component is used for both adding new customers and editing existing ones, we’ll pass the necessary data as input. For a new customer, some sensible default values are used; for existing customers, data is retrieved from the database. Apart from setting default values and determining whether to call `createCustomer` or `updateCustomer`, all other aspects of form handling remain the same.

```tsx
type FormData = z.infer<typeof formSchema>
const form = useForm<FormData>({
  resolver: zodResolver(formSchema),
  defaultValues: customer,
});
```

The final step is to create the form itself and assemble it in the TSX file. As shown earlier, this process is straightforward. Whether we’re using text inputs, date pickers, or checkboxes with Shadcn controls, we follow a similar structure:

- Start by creating the `FormField` element and setting its `control`, `name`, and `render` properties.
- The `render` property is key, as it contains the form element itself.
- Typically, we wrap everything in `FormItem`, add a `FormLabel` for the label, and place the controlled form element inside `FormControl` with the appropriate value and setter method.
- Finally, we include `FormMessage` below, which displays the Zod validation message if validation fails.

![form with errors](/img/forms/form-error.png)

```tsx

// Defining form schema
const formSchema = z.object({   
    dateOfBirth: z.date().max(new Date(), {
      message: 'Date of birth cannot be today, or in the future',
    }),    
});

// Defining form  
const form = useForm<FormData>({
    resolver: zodResolver(formSchema),
    defaultValues: defaultValues,
});

 // Creating form control
  <FormField
  control={form.control}
  name="dateOfBirth"
  render={({ field }) => (
    <FormItem className="flex flex-col">
      <FormLabel>Date of birth</FormLabel>
      <FormControl>
        <DatePicker date={field.value} setDate={field.onChange} />
      </FormControl>
      <FormMessage />
    </FormItem>
  )}
/>
```

If you’re curious to see the complete application, check out the GitHub repository here: [GitHub Repo](https://github.com/martinovicdev/wasp-form-tutorial). I hope this article has made working with forms easier, and if you're interested in more form-related content, stay tuned for part two! In the next part, we'll dive into advanced patterns and validation techniques to enhance your applications.

Please consider starring [**Wasp**](https://github.com/wasp-lang/wasp) on GitHub if you liked this post! Your support helps us continue making web development easier and smoother for everyone. 🐝