---
id: 01-creating-the-project
title: Creating the project
---

import useBaseUrl from '@docusaurus/useBaseUrl';

By now you've already learned [how to install Wasp and create a new project](/getting-started.md). So let’s create a new web app appropriately named `ItWaspsOnMyMachine`.

```
wasp new ItWaspsOnMyMachine
```

Changing the working directory:
```
cd ItWaspsOnMyMachine
```

Starting the app:
```
wasp start
```

Now your default browser should open up with a simple predefined text message. That’s it! 🥳 For now – the codebase consists of only two files! `main.wasp` is the config file that defines the application’s functionality. And `MainPage.js` is the front-end. Everything else, like `Main.css` can be ignored.

<img alt="Initial page"
     src={useBaseUrl('img/init-page.png')}
/>