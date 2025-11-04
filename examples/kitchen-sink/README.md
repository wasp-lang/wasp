# Kitchen Sink Application

The **`kitchen-sink`** app serves two main purposes:

1. The **primary** purpose of the `kitchen-sink` app is for contributors to test the development version of the Wasp CLI easily.
2. The **secondary** purpose is to demonstrate as many Wasp features as possible.
   Not all features can be included at once (e.g., `email` vs `usernameAndPassword` auth are mutually exclusive), but the goal is to cover as many as we can.

The `kitchen-sink` app is also the main playground for Wasp’s e2e application tests.  
Any new or modified features should be added and tested here whenever possible.

## Setup

### Environment Variables

To run the app with minimal setup:

```sh
cp .env.server.example .env.server
```

This command creates a `.env.server` file with basic env variables set to dummy values.
The app will run, but not all features will work unless you provide the genuine API keys.

Check the [Wasp docs](https://wasp.sh/docs) for instructions on configuring supported providers such as Google Auth or GitHub Auth.

#### For Wasp team members

You can use the shared `.env.server` with genuine API keys:

```sh
npm run env:pull
```

To persist the local changes back to the shared config:

```sh
npm run env:push
```

## Running

The `kitchen-sink` app runs like any other Wasp application.

1. Start the database:

```sh
wasp start db
```

2. Migrate the database (if needed, in a separate terminal):

```sh
wasp db migrate-dev
```

3. Start the app (in a separate terminal):

```sh
wasp start
```

4. Open `localhost:3000` in the browser to see the app!

### Development

To run the application with the development version of Wasp, please use the `./run wasp-cli` script located in the `waspc/` directory.

For example:

```sh
# From inside the kitchen-sink directory:
../../waspc/run wasp-cli start db

# Or use an alias with the absolute path to the script:
wrun wasp-cli start db

# Or if you installed the development `waspc` binary globally:
wasp-cli start db
```

## Testing

### End-to-End Tests

We use `playwright` for e2e tests.
Run the e2e tests with:

```sh
npm run test
```
