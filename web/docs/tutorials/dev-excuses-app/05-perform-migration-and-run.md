---
id: 05-perform-migration-and-run
title: Perform migration and run the app
---

import useBaseUrl from '@docusaurus/useBaseUrl';
import DiscordLink from '../../../blog/components/DiscordLink';

Before we run our app, we need to execute a database migration. We changed the DB schema by adding new entities. By doing the migration, we sync the database schema with the entities we defined. If you’ve had something running in the terminal – stop it and run:

```
wasp db migrate-dev
```

You’ll be prompted to enter a name for the migration. Something like `init` will be ok. Now we can start the application!

```
wasp start
```
<img alt="Final empty result"
     src={useBaseUrl('img/final-result.png')}
/>

Now you can click the “Get excuse” button to receive an excuse. You should also be able to save the ones you like with the “Save excuse” button. Our final project should look like this:

<img alt="Final result"
     src={useBaseUrl('img/final-excuse-app.png')}
/>

Now we can think of some additional improvements. For example:
 
- Add a unique constraint to Entity’s ID so we won’t be able to save duplicated excuses. 
- Add exceptions and edge cases handling. 
- Make the markup prettier.
- Optimize and polish the code 

So, we’ve been able to build a full-stack application with a database and external API call in a couple of minutes. And now we have a box full of excuses for all our development needs. 

<img alt="Box of excuses for the win!"
     src={useBaseUrl('img/accessible-website-excuse.jpg')}
/>


P.S: now you're familiar with Wasp and can build full-stack apps, horaay! 🎉 How did it go? Was it fun? Drop us a message at our <DiscordLink />. Now it's time to look at [Todo App in Wasp](/docs/tutorials/todo-app) if you haven't already. It will introduce some additional concepts so you'd be able to become a true Wasp overlord! 