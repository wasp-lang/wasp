# Ask The Documents (Embeddings / RAG / ChatGPT) with Wasp & PG Vector

![Ask The Documents Cover](./github.png)

## What does it do?

This is an example Wasp app that supports:

- scraping whole link hierarchies (great for docs)
- scraping a single link
- generating embeddings for page content
- semantic search using PG Vector
- chatting with the documents using OpenAI's ChatGPT

## Running it locally

First, makes sure you have Wasp installed.

```bash
curl -sSL https://get.wasp-lang.dev/installer.sh | sh
```

Then, close this repo, position yourself in `custom-db` and start the database with:

```bash
cd custom-db
./run_db.sh
```

Follow the instructions in the script.

Fill the env variables in `.env.server`:

```
OPENAI_API_KEY=<your_key>
DATABASE_URL=postgresql://postgres:devpass@localhost:5432/postgres
```

Migrate the database with:

```bash
wasp db migrate-dev
```

Then, start the server with:

```bash
wasp start
```
