# Wasp Studio

Wasp Studio has two components:
- the server
- the client

To develop the studio, you need to run both of them. First, run the server, then the client.

### Server

Install dependencies:

```bash
npm install
```

Running the server for some data file:

```bash
npm run dev -- -- -d <path_to_data_file>
```

For example, running the server with the data file from the `examples` directory:

```bash
npm run dev -- -- -d ../../examples/crud-testing/.wasp/out/.wasp-studio-data.json
```

### Client

Install dependencies:

```bash
cd client
npm install
```

Running the client:

```bash
npm run dev
```

Then open `http://localhost:5173` in your browser.