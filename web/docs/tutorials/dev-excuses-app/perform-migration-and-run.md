---
title: Perform migration and run the app
---

import useBaseUrl from '@docusaurus/useBaseUrl';

Before starting an app – we need to execute database migration because we changed the DB schema by adding new entities. If you’ve had something running in the terminal – stop it and run:

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

Now you can click the “Get excuse” button to receive an excuse. And save the ones you like into the DB with the “Save excuse” button. Our final project should look like this:

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