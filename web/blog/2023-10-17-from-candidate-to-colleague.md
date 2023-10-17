---
title: '👥 From Candidate to Colleague: Acing a competitive Junior-level recruitment challenge 📋'
authors: [lucas]
image: img/from-candidate-to-colleague.gif
tags: [career, web-development, tutorial, learning]
---

## Hello there!

## TL;DR

![Project Overview](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/9g5owahcqzhpo331wol3.png)

In this article, we want to emphasize the importance of coding challenges by giving you the skills you’ll need to pass a common junior-level interview challenge: building a full-stack CRUD app (which we will use the [Wasp framework](https://wasp-lang.dev/) and it’s stack — React, Node and Prisma to do so). The application focuses on making a web app for trading and rating Pokémon based on their Experience Points (XP), using the [PokéAPI](https://pokeapi.co/) to get the data that we need.

We will also discuss the important factors that recruiters look for when hiring new developers, such as code quality, readability, stack and architectural choices and a more modern approach to AI use in code generation. It's a helpful read if you want to improve your coding skills and succeed in the hiring process!

You can find the deployed version of the app [here](https://client-production-55c6.up.railway.app/) and the repository with the final code [here](https://github.com/LLxD/Poketrader-Wasp)!

### Before we Begin

![funny gif](https://media1.giphy.com/media/QF0qzNUO8FfX2/giphy.gif?cid=7941fdc6xrdr0zrg9zteorw0fxnkjz5q6ony465zf9uekqnx&ep=v1_gifs_search&rid=giphy.gif&ct=g)

In this article, I'll be using the [Wasp Framework](https://github.com/wasp-lang/wasp). It provides a great way to start a project with its AI app generator and a really awesome developer experience for when you want to create a full-stack web application with all batteries included, front-end, back-end, and database.

I'm currently a member of the Wasp team and since it’s also an open-source project, you can contribute too!

The easiest way to show your support is just to star our repo! 🐝 But it would be greatly appreciated if you could take a look at our repository (for contributions or to simply test the product) too!

[⭐️ Give Wasp Framework a Star! ⭐️](https://www.github.com/wasp-lang/wasp)

## Introduction

Currently, in the world of tech recruitment, coding challenges have become a common way to test candidates' technical skills and problem-solving abilities. In this blog post, we will discuss the importance of coding challenges in the hiring process and create one ourselves using the [Wasp Framework](https://wasp-lang.dev/), so you can improve your results when faced with a coding challenge!

For this to work, we will simulate a junior-level role challenge and take it on ourselves. Generally, we have some time to do this (usually a week or so) but, to make things fair, I’m going to set my timer to 4 hours, in order to forcefully make some mistakes and put myself under pressure.

![Here’s my stats in the end 👀.](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/y1xi4nn6n0pfjgkfhai6.png)

Here’s my stats in the end 👀.

Understanding the expectations and requirements of recruiters is crucial to increase your chances of success in the job market. Besides that, in the end, I’ll evaluate the project from the perspective of a Senior Dev, so you can check out all improvements that I would like to see on this project!

## **Overview of the Recruitment Challenge**

![Project Requirements](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/y4vhf8f2fmj695j59gry.png)

Great! Now that we have set up the context, let me explain to you what the challenge consists of:

We will implement a web app where the user must be able to do the following:

- Have two sides with 6 Pokémon slots on each
- Set up Pokémon on both sides (one Pokémon for each slot)
- Register the current state of the sides as a Trade
- See a history of all trades that were registered
- Get real-time information on the status of the trade:
  - If the total sum of the experience points of each Pokémon on any side has a 10% difference than the other side, it's unfair (users can still register it though)
  - Otherwise, it's a fair trade

With this challenges, recruiters want to evaluate a few things of the current skills of a candidate:

- Code quality: Recruiters want to assess a candidate's ability to write clean, understandable, and well-structured code. This includes following best practices, using appropriate naming conventions, and organizing code into reusable components or functions.
- Problem-solving skills: Recruiters want to evaluate a candidate's ability to analyze problems, break them down into smaller tasks, and come up with effective solutions. This can involve logical thinking, algorithm design, and debugging skills.
- Technical knowledge: Recruiters want to assess a candidate's knowledge of programming languages, frameworks, and tools relevant to the job. This can include languages like JavaScript, frameworks like React or Node.js, and tools like Git.
- Communication skills: Recruiters want to evaluate a candidate's ability to communicate effectively with team members and stakeholders. This can involve explaining technical concepts, collaborating on projects, and providing clear documentation.

We’ll talk generally about all of those things on this article, but, I’ll leave some links on the end for further learning on all those topics!

To make this challenge possible, we'll use the [PokéAPI](https://pokeapi.co/)'s endpoints to fetch Pokémon's data and XP value. Here’s a preview of how this is gonna end-up looking like (consider this your prototype, if you’re coding along):

![Project image](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/ypcq702ai4m74xqlapso.png)

![Project image](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/adyu5ltg9kbrjf92lo3y.png)

In those images, we can clearly see a trade area, the ability to insert Pokémons and register some trades!

## Setting up our repo

![Stack](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/ichbtc64y1bfnzgcnkj0.png)

So, first and foremost, let's go to the [Wasp's GPT app generator](https://usemage.ai/) to get an amazing way to start our project up since it uses GPT and the Wasp Framework to generate a coherent full-stack web application.

![Mage](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/7r5giwuu54jcgfg7i7gl.png)

After opening up the web page, I started to describe the basic functionalities of the application. I tried not to be very specific (which is generally not recommended for AI generation), but the purpose of that is to check if the AI can give us a great start even with an imperfect prompt. Here is the prompt I used:

> Generate a web app where users can choose Pokémon for two sides: Trade Area A and Trade Area B. After choosing, the web app evaluates if the trade is fair or unfair based on the sum of experience points of both sides (a 10% difference or higher is considered unfair). Users can also register the trades to their history and view this history on a profile page.

With this prompt, the AI was able to generate this [result](https://usemage.ai/result/c7793841-8733-4346-a248-6cdb405a9cf1):

![Mage Results](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/vdxyf97ld4duujztw6e8.png)

The result contains some impressive things and the Wasp framework makes web app development easier by using a configuration file (`main.wasp`) and source files to describe your app's details and logic. When you compile your Wasp project, the compiler creates the complete source code for the front-end, back-end, and deployment.

This method is what enables Wasp to create the whole project using an AI generator (which is exactly what we are doing here) while maintaining consistency and quality. The generator examines the prompt and produces code for components, routes, actions, and queries based on the necessities of the user. Now, let's check some of the created code and structure and evaluate them:

- The front-end ✅:
  - It generated a Login and Signup route and pages (which is excellent, for reference, so excellent that I only made some minor style changes to those; it was working out of the box!) ✅
  - It generated three pages:
    - Home - which is just a series of buttons referencing other pages ✅
    - Profile - which is the user profile page ✅
    - Trade - which is the page where users can register new trades ✅
- The back-end ⚠️:
  - It generated two actions (which are simply functions to interact with our back-end): evaluateTrade and registerTrade — which are precisely the two actions that the app needs ✅
  - It generated two queries: getPokémon and getUserTrade — getUserTrade was perfect from the beginning, and getPokémon would need some adjustments (it misunderstood a few things on the database layer, as we can check below) ⚠️
- The database ⚠️ :
  - Here is where it suffered the most — generally, the idea was correct, but in this case, since we're not creating each one of our Pokémons on our database (just fetching them from an API), the ideas where it used the Pokémon model were not correct. Everything else was basically okay! ⚠️

## Implementation

So, simply by using this [AI generator](https://usemage.ai/), we were able to generate an amazing start to get it going. After that, it was a simple matter of downloading the generated app, initializing a repository with it, and following [Wasp's getting started documentation.](https://wasp-lang.dev/docs/quick-start)

To install Wasp’s framework, we can run the command:

**`curl** -sSL https://get.wasp-lang.dev/installer.sh | **sh**`

To get an even better developer experience, we can download the [Wasp extension for VS Code](https://marketplace.visualstudio.com/items?itemName=wasp-lang.wasp) too!

BTW: If you want to just check the code result, here is the [Github Repository link](https://github.com/LLxD/Poketrader-Wasp) for it!

After setting up the basic structure and preparing the project for development, it is time to start developing our application!

So, firstly, let's retrieve some Pokémon, since their information is used everywhere on the app! We have the option to fetch each endpoint using the [fetch API](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API), for example, but I will take advantage of the fact that there is an [npm library specifically for Pokémon](https://www.npmjs.com/package/pokedex-promise-v2) that handles this for us.

You can add this dependency to your wasp project simply by adding it (and its version) to your main.wasp file.

```tsx
//main.wasp
//adding the dependencies array
app Poketrader {
  wasp: {
    version: "^0.11.1"
  },
  title: "Poketrader",
  client: {
    rootComponent: import { Layout } from "@client/Layout.jsx",
  },
  dependencies: [
    ("pokedex-promise-v2","^4.1.1")
  ],
...
}
```

Now, that we added this dependency, it’s time to fetch them per se, so in order to do this, I have refactored the getPokémon function to the following:

```jsx
//src/server/queries.js
// a simples async await fetch to get the pokemon data
async function loadPokemons() {
  const P = new Pokedex()
  const response = await P.getGenerationByName('generation-i')
  return response.pokemon_species
}

export const getPokemon = async () => {
  const pokemons = await loadPokemons()

  return pokemons
}
```

Basically, we are fetching Pokémon from the first generation only (which is the one I am most familiar with, but you can choose others if you prefer 🤣) and returning them to the front-end.

Using this variable on the front-end side, we can populate our `<select>` options with all Pokémon and add some conditional logic to handle saving the chosen Pokémon.

```jsx
//src/client/pages/Trade.jsx
//creating a state to hold all selected pokemons
//mapping all pokemons to options for our selector
//adding the onChange method to insert selected pokemons in our state
const [selectedPokemon, setSelectedPokemon] = useState({
    areaA: null,
    areaB: null,
  });
...
return(
<h3>Trade Area A</h3>
<select
	onChange={(e) => {
    setSelectedPokemon({ ...selectedPokemon, areaA: e.target.value });
  }}
>
<option disabled selected value>
	-- select an option --
</option>
  {pokemons.map((pokemon) => (
    <option key={pokemon.name} value={pokemon}>
      {pokemon.name}
    </option>
  ))}
</select>
)
```

This will generate our select with the Pokémons correctly. Here’s how it looks:

![Select](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/hn212zn3enfo2h7bjmdl.png)

Afterwards, I made this component more generic so that it can be reused in trade area B. I also modified the loadPokémons function to retrieve not only the Pokémon's information but also their images and experience. The final result looks like this:

```jsx
//src/server/queries.js
//refactoring loadPokemons to also get images and base_experience values
async function loadPokemons() {
  const P = new Pokedex()
  const response = await P.getGenerationByName('generation-i')
  const pokemons = response.pokemon_species
  const pokemonsWithImages = await Promise.all(
    pokemons.map(async (pokemon) => {
      const pokemonData = await P.getPokemonByName(pokemon.name)
      return {
        id: pokemonData.id,
        name: pokemonData.name,
        base_experience: pokemonData.base_experience,
        image: pokemonData.sprites.front_default,
      }
    })
  )

  return pokemonsWithImages
}

export const getPokemon = async () => {
  const pokemons = await loadPokemons()

  return pokemons
}
```

So, here we are making additional requests for each Pokémon to retrieve all the necessary data, including images and base_experience. Now, we can call those images on the front-end component in order to show them to our user!

So let’s create a generic component for both trade areas (since they share a lot of common things). First, create a new component (we will call it `TradeArea.jsx` ), then we can make a few additions.

We will add a title (which must be dynamic for the area) and a select element with Pokémon options.

Additionally, we should add a new button that sets the trade area (which are simply arrays of Pokémons that were selected) with the current Pokémon while keeping the old ones. We also should include a section to display the current trade area, showing the image and name of the selected Pokémon.

```jsx
//src/client/components/TradeArea.jsx
// adding those imgs that the back-end now sends
// adding a button to add to the trade area
// also adding a max size of pokémons on each side (6)
import React from 'react'

function TradeArea({
  pokemons,
  tradeAreaName,
  setSelectedPokemon,
  selectedPokemon,
  setTradeArea,
  tradeArea,
}) {
  const displayName =
    tradeAreaName === 'areaA' ? 'Trade Area A' : 'Trade Area B'
  return (
    <div>
      <h3 className="text-xl">{displayName}</h3>
      {tradeArea.map((pokemon) => (
        <div className="grid grid-flow-col items-center">
          <p>
            Pokemon: <strong className="capitalize"> {pokemon.name} </strong>
          </p>
          <img className="w-10" src={pokemon.image} alt={pokemon.name} />
        </div>
      ))}
      <div className="grid gap-2">
        <select
          className="border border-gray-400 rounded-md"
          onChange={(e) => {
            const targetPokemon = pokemons.find(
              (pokemon) => pokemon.name === e.target.value
            )
            setSelectedPokemon({
              ...selectedPokemon,
              [tradeAreaName]: targetPokemon,
            })
          }}
        >
          <option disabled selected value>
            -- select an option --
          </option>
          {pokemons.map((pokemon) => (
            <option key={pokemon.name} value={pokemon.name}>
              {pokemon.name}
            </option>
          ))}
        </select>
        <button
          className="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded"
          onClick={() => {
            if (!selectedPokemon[tradeAreaName]) return

            if (tradeArea.length < 6) {
              setTradeArea([...tradeArea, selectedPokemon[tradeAreaName]])
            }
          }}
        >
          Add to {displayName}
        </button>
      </div>
    </div>
  )
}

export default TradeArea
```

Here is how it looks (but you can improve the styles as you want):

![Trade Area](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/yxhnlhpe1le6oej84ajq.png)

Now that we have obtained the basic experience and images of Pokémon, we can begin evaluating those trades. This involves adding up the XP of each Pokémon on both sides, determining the lower total, and setting that as the maximum threshold for a 10% difference.

To calculate the sum, I used the reduce function. If the trade is fair, the word "fair" is returned; if it is unfair, the word "unfair" is returned. Here's how it looks:

```jsx
//src/server/actions.js
export const evaluateTrade = async (args) => {
  const pokemonAList = args.tradeAreaA
  const pokemonBList = args.tradeAreaB

  const totalA = pokemonAList.reduce(
    (acc, pokemon) => acc + pokemon.base_experience,
    0
  )
  const totalB = pokemonBList.reduce(
    (acc, pokemon) => acc + pokemon.base_experience,
    0
  )

  const difference = Math.abs(totalA - totalB)
  const lowerTotal = Math.min(totalA, totalB)
  const fairThreshold = lowerTotal * 0.1

  if (difference < fairThreshold) {
    return 'fair'
  } else {
    return 'unfair'
  }
}
```

Now, on our front-end, I have implemented a functionality where the fairness is recalculated using the useEffect hook every time the trade areas change.

```jsx
//src/client/pages/Trade.jsx
// if the tradeAreaA or tradeAreaB changes, useEffect will execute handleEvaluteTrade()
const handleEvaluateTrade = async () => {
  const result = await evaluateTradeFn({
    tradeAreaA: tradeAreaA,
    tradeAreaB: tradeAreaB,
  })
  setFairness(result)
}

useEffect(() => {
  if (tradeAreaA.length > 0 && tradeAreaB.length > 0) {
    handleEvaluateTrade()
  }
}, [tradeAreaA, tradeAreaB])
```

And I made it so that the words "fair" and "unfair" (with separate styles - green for fair and red for unfair) actually appear in the user interface as well.

![Fair Trade](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/ylqafgn38k43vfqnbaeq.png)

![Unfair Trade](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/r2p3xoaqgykh048q9w6l.png)

```jsx
//src/client/pages/Trade.jsx
<h2
  className={`text-2xl mb-4 font-bold ${
    fairness === 'fair' ? 'text-green-500' : 'text-red-500'
  } `}
>
  Trade {fairness && `- is ${fairness}`}
</h2>
```

Here, we are almost 60% done with our app! It currently allows users to retrieve Pokémon, add them to two different trade areas, and determine if the trades are fair or not. Now, for the final 40%, we need to implement the ability to register trades in the user's history.

To do this:

- First, add a button that will call the `registerTrade` function.

```jsx
//src/client/pages/Trade.jsx
<button
  onClick={handleRegisterTrade}
  className="bg-green-500 hover:bg-green-700 text-white font-bold py-2 px-4 rounded mt-4"
>
  Register Trade
</button>
```

- And then, we define the function to register the trades:

```jsx
//src/client/pages/Trade.jsx
import registerTrade from '@wasp/actions/registerTrade'

const handleRegisterTrade = async () => {
  await registerTrade({
    tradeAreaA: tradeAreaA,
    tradeAreaB: tradeAreaB,
    fairness: fairness,
  })

  setTradeRegisteredStatus(true)
}
```

Now, we just need to make some small changes to our database to support these tradeArea objects. We also need to verify the function that registers them in the database.

First, let's make the following changes to the database:

```tsx
//main.wasp
//added the tradeAreaA and tradeAreaB fields as String type (we will JSON.stringify()
//they later)
entity Trade {=psl
    id         Int      @id @default(autoincrement())
    userId     Int
    user      User     @relation(fields: [userId], references: [id])
    tradeAreaA String
    tradeAreaB String
    fairness   String @default("unfair")
psl=}
```

And after that, let’s run a new migration by using the command `wasp db migrate-dev`.

I created two String trade areas on the Trade entity. We have chosen to use String because we are using SQLite as our database, which does not support JSON object types. As a workaround, we will convert the objects to strings and parse them later. Although this is not an optimal solution, I will discuss this and other potential solutions in more detail in an upcoming topic from the perspective of a Senior Developer.

Now, let's discuss the changes in the function responsible for registering trades in the database:

```js
//src/server/actions.js
//here we convert the trade areas to string and save to our db!
export const registerTrade = async (args, context) => {
  const pokemonAList = args.tradeAreaA
  const pokemonBList = args.tradeAreaB
  const { user, entities } = context

  const stringifiedPokemonAList = JSON.stringify(pokemonAList)
  const stringifiedPokemonBList = JSON.stringify(pokemonBList)

  if (!user) {
    throw new HttpError(401)
  }

  const trade = await entities.Trade.create({
    data: {
      tradeAreaA: stringifiedPokemonAList,
      tradeAreaB: stringifiedPokemonBList,
      fairness: args.fairness,
      user: { connect: { id: user.id } },
    },
  })

  return trade
}
```

Here, we can observe that we are "stringifying" our JSONs, as mentioned earlier, and registering all of it as a trade!

Finally, we can verify this in the UI.

![Register Trade Button](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/o4ue3wxakc8jcphizl1x.png)

What? Were you expecting more? It's just a button (although we know it's not that simple on the back-end side of things, hahaha). After that, I made some minor fixes to the AI-generated code for the profile page, such as parsing the JSON and improving some CSS. Luckily, the code that the AI generated for fetching the user trades is completely correct, so there's no need to work on it!

```jsx
//src/client/pages/Profile.jsx
//map all trades, for each one parse the tradeAreas and render them on the screen
{
  trades.map((trade) => {
    const tradeAreaA = JSON.parse(trade.tradeAreaA)
    const tradeAreaB = JSON.parse(trade.tradeAreaB)
    return (
      <div
        className={`mb-4 border rounded shadow p-4 ${
          trade.fairness === 'fair' ? 'bg-green-300' : 'bg-red-300'
        }`}
      >
        <div className="mb-2">
          <span className="font-bold">Trade ID: </span>
          <span className="capitalize">{trade.id}</span>
        </div>
        <div className="mb-2">
          <span className="font-bold">Trade Area A: </span>
          <span className="capitalize">
            {tradeAreaA.map((pokemon) => {
              return pokemon.name + ' '
            })}
          </span>
        </div>
        <div className="mb-2">
          <span className="font-bold">Trade Area B: </span>
          <span className="capitalize">
            {tradeAreaB.map((pokemon) => {
              return pokemon.name + ' '
            })}
          </span>
        </div>
        <div className="mb-2">
          <span className="font-bold">Fairness: </span>
          <span>{trade.fairness}</span>
        </div>
      </div>
    )
  })
}
```

And finally, we can check the user's history on the UI. This concludes the scope of this challenge!

![Trade History](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/esgsfii3zgwthi9u0jpn.png)

Great! Now that we have finished everything, let's take a look at the application as a whole:

![Home](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/yqg009a0tc1p7gzif0c8.png)

![Trade Areas](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/r082q4kaccofzw0gx9tc.png)

![Trade History](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/d3qw6pn9m4z0e1uo5af5.png)

Looks really good to me! Let’s make some minor changes to make it deployment ready:

Let’s start by adding PostgresSQL as our main database here!

```tsx
//main.wasp -> in the db section, add the PostgresSQL system!
db: {
    prisma: {
      clientPreviewFeatures: ["extendedWhereUnique"],
    },
      system: PostgreSQL,
  },
  auth: {
    userEntity: User,
```

After that, we need to re-run the migrations by deleting the old ones and running `wasp db migrate-dev` and then, finally, we can just [follow the deployment guide on Wasp’s documentation](https://wasp-lang.dev/docs/advanced/deployment/overview).

## **Evaluation from a Senior Developer's Perspective**

![Evaluation](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/8gqvxn9giqos9chqqktq.png)

Now that we have the [final version of our app](https://github.com/LLxD/Poketrader-Wasp/tree/master), let's evaluate it from the perspective of a more senior developer to identify areas for improvement and anticipate the type of feedback we might receive:

- Front-end:

  - Good:
    - Consistency across the UI: The UI maintains consistent styling, with buttons and color meanings applied correctly (green for success, red for errors/dangerous scenarios).
    - Codebase reusability: The TradeArea component was successfully made reusable, demonstrating the potential for other complex components to be transformed into reusable ones.
    - Good code quality and structure: Functions and variables fulfill their intended purposes and have appropriate names (to check the importance of naming, you can give a read [here](https://dev.to/wasp/why-naming-is-1-skill-for-writing-clean-code-4a5p)).
      - Here are some examples of that:
        - `loadPokémons()`, `registerTrade()`, `setSelectedPokémon()` are all good naming examples in which we can get the basic sense of what the function does just by reading their name.
    - Responsiveness: The UI is designed with a mobile-first approach (thanks to [TailwindCSS](https://tailwindcss.com/)).
  - Bad:

    - Lack of unit tests for complex behaviors.
    - Consider using a more user-friendly method to display all Pokémon options, rather than a simple select with 200 options.

      - For example: There are alternative [select components](https://react-select.com/home) available that could be used.
      - You can see that this select has an input attached to it (which would bring a much better UX for the user if he wanted a Pokémon who is on the end of the list)

      - ![Select Example](https://dev-to-uploads.s3.amazonaws.com/uploads/articles/2kb1pmn2hpmvulb893wz.png)

    - The front-end currently parses the JSON.stringify data from the back-end. It would be better if the back-end sent the data already correctly parsed.

- Back-end and database:
  - Good:
    - Clear separation of scopes: The structures are well-organized, and functions are appropriately placed.
    - Error handling: A few error handlers are in place, enhancing the application's resiliency.
      - We have some private insertions validations in place (e.g. `if (!user){ throw new HttpError(401); }`)
      - And also some basic auth validations that the auth method provides by itself via the Wasp Framework.
    - Effective use of async/await and promise handling.
  - Bad:
    - Suboptimal choice of database: The absence of JSON in our database is a drawback, but it's commendable that a creative solution was found for this issue.
    - Lack of unit tests.

Overall, we have a simple application that functions well. There is evident consideration given to the user interface and user experience, which is a positive aspect. The candidate also demonstrated creativity in resolving encountered issues. However, it would have been beneficial for the candidate to include unit tests and consider a more suitable database choice. Nevertheless, good work overall! 👍

## Conclusion

Just in case you haven't starred [Wasp's repository](https://www.github.com/wasp-lang/wasp), I would recommend doing so! It's a great repository for you to test it out on a full-stack application!

[⭐️ Give it a star! ⭐️](https://www.github.com/wasp-lang/wasp)

In this test, we would have successfully completed a challenge by creating an impressive application. The application allowed users to trade Pokémon and check if the trades were fair, which is precisely the scope of our challenge.

Our code consistently handled data storage and retrieval as well, and we also paid some attention to styling. Although we could have improved the implementation by adding tests and using a better database, the creativity and problem-solving skills were evident, which is enough in most cases (remember: no app is perfect).

It's important to remember that even when you are working on a minor recruitment challenge, paying attention to UI, UX, and code quality will greatly enhance your deliverables. Of course, the technical challenge is not everything; you will also need to have good communication skills and other soft-skills (which I’ll leave some links down below where you can see some additional resources).

And hey! Since you made it until the end here, leave some comments below on how you would rate this project too! I’ll be happy to hear them!

## **Additional Resources and Further Learning**

- For learning some soft-skills and improving your communications skills: [How to deal with people: Communication](https://dev.to/llxd/how-to-deal-with-people-communication-5gef)
- For UI/UX Laws: [Laws of UX](https://lawsofux.com/)
- For design patterns and code structure ideas: [Refactoring Guru](https://refactoring.guru/)
- The importance of naming things correctly: [Why Naming is #1 Skill for Writing Clean Code 🧼🧑‍💻](https://dev.to/wasp/why-naming-is-1-skill-for-writing-clean-code-4a5p)
