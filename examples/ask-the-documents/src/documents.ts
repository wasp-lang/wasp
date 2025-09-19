import { type Document } from "wasp/entities";

import {
  type AskDocuments,
  type DeleteAllDocuments,
  type DeleteDocument,
  type EmbedDocument,
  type GetDocuments,
  type GetScrapeCandidates,
  type SearchDocuments,
} from "wasp/server/operations";

import { HttpError, env, prisma } from "wasp/server";
// @ts-ignore
import openai from "openai";
import { toSql } from "pgvector/utils";
import { getContent, getLinksToScrape } from "./scrape.js";

const api = new openai.OpenAI({
  apiKey: env.OPENAI_API_KEY,
});

type EmbedDocumentInput = {
  url: string;
  selector?: string;
};
type EmbedDocumentOutput = {
  success: boolean;
};

export const embedDocument: EmbedDocument<
  EmbedDocumentInput,
  EmbedDocumentOutput
> = async (args, { user }) => {
  if (!user) {
    throw new HttpError(401, "You must be logged in to embed documents");
  }
  const { url, selector } = args;

  // Scrape url to get the title and content
  const { title, markdownContent } = await getContent(url, selector);

  const embedding = toSql(await createEmbedding(markdownContent));

  await prisma.$queryRaw`
    INSERT INTO "Document" ("id", "title", "content", "embedding", "url", "updatedAt")
    VALUES (gen_random_uuid(), ${title}, ${markdownContent}, ${embedding}::vector, ${url}, ${new Date()})
    RETURNING "id";
  `;

  return { success: true };
};

type GetDocumentsInput = void;
type GetDocumentsOutput = Document[];

export const getDocuments: GetDocuments<
  GetDocumentsInput,
  GetDocumentsOutput
> = async (_args, context) => {
  return context.entities.Document.findMany();
};

type SearchDocumentsInput = {
  query: string;
};
type SearchDocumentsOutput = {
  document: Document;
  score: number;
}[];

export const searchDocuments: SearchDocuments<
  SearchDocumentsInput,
  SearchDocumentsOutput
> = async (args, { user }) => {
  if (!user) {
    throw new HttpError(401, "You must be logged in to search documents");
  }
  const { query } = args;

  const embedding = toSql(await createEmbedding(query));

  const result = (await prisma.$queryRaw`
    SELECT "id", "title", "content", "embedding" <-> ${embedding}::vector AS "score", "createdAt", "updatedAt", "url"
    FROM "Document"
    ORDER BY "embedding" <-> ${embedding}::vector
    LIMIT 10;
  `) as {
    id: string;
    title: string;
    content: string;
    score: number;
    createdAt: Date;
    updatedAt: Date;
    url: string;
  }[];

  return result.map((result) => ({
    document: {
      id: result.id,
      title: result.title,
      content: result.content,
      url: result.url,
      createdAt: result.createdAt,
      updatedAt: result.updatedAt,
    },
    score: result.score,
  }));
};

type DeleteDocumentInput = {
  id: string;
};
type DeleteDocumentOutput = void;

export const deleteDocument: DeleteDocument<
  DeleteDocumentInput,
  DeleteDocumentOutput
> = async (args, context) => {
  if (!context.user) {
    throw new HttpError(401, "You must be logged in to delete documents");
  }
  const { id } = args;
  await context.entities.Document.delete({
    where: { id },
  });
};

type DeleteAllDocumentsInput = void;
type DeleteAllDocumentsOutput = void;

export const deleteAllDocuments: DeleteAllDocuments<
  DeleteAllDocumentsInput,
  DeleteAllDocumentsOutput
> = async (_args, context) => {
  if (!context.user) {
    throw new HttpError(401, "You must be logged in to delete documents");
  }

  await context.entities.Document.deleteMany();
};

type GetScrapeCandidatesInput = {
  url: string;
};
export const getScrapeCandidates = (async (args, context) => {
  const { url } = args;
  return getLinksToScrape(url);
}) satisfies GetScrapeCandidates<GetScrapeCandidatesInput>;

type AskDocumentsInput = {
  query: string;
};
type AskDocumentsOutput = {
  answer: string;
  sources: Array<{ part_of_text: string; url: string }>;
};

export const askDocuments: AskDocuments<
  AskDocumentsInput,
  AskDocumentsOutput
> = async (args) => {
  const { query } = args;
  const queryEmbedding = await createEmbedding(query);

  const documents = (await prisma.$queryRaw`
    SELECT "content", "embedding" <-> ${toSql(
      queryEmbedding,
    )}::vector AS "score", "url"
    FROM "Document"
    ORDER BY "embedding" <-> ${toSql(queryEmbedding)}::vector
    LIMIT 2;
  `) as {
    content: string;
    score: number;
    url: string;
  }[];

  const tools = [
    {
      type: "function" as const,
      function: {
        name: "answer_with_sources",
        description:
          "Answer the question using the provided documents and cite sources.",
        parameters: {
          type: "object",
          properties: {
            answer: {
              type: "string",
              description: "The answer to the question.",
            },
            sources: {
              type: "array",
              items: {
                type: "object",
                properties: {
                  part_of_text: {
                    type: "string",
                    description:
                      "The relevant part of the document used for the answer.",
                  },
                  url: {
                    type: "string",
                    description: "URL of the source document.",
                  },
                },
                required: ["part_of_text", "url"],
              },
              description: "List of sources used to generate the answer.",
            },
          },
          required: ["answer", "sources"],
        },
      },
    },
  ];

  const completion = await api.chat.completions.create({
    messages: [
      {
        role: "system",
        content:
          "You are a Q&A system. Respond concisely. Extract relevant parts from the documents to use as sources. If the answer is not clear from the documents, respond with 'I don't know'. Don't include links in the final answer.",
      },
      {
        role: "user",
        content: query,
      },
      {
        role: "system",
        content: `Source documents:
        ${documents
          .map(
            (r) =>
              `"""${r.content}"""\nScore: ${10 / r.score}\nSource URL: ${r.url}`,
          )
          .join("\n\n")}`,
      },
    ],
    model: "gpt-4o",
    tools,
    tool_choice: {
      type: "function",
      function: { name: "answer_with_sources" },
    },
  });

  const toolCall = completion.choices[0].message?.tool_calls?.[0];

  if (toolCall && toolCall.type === "function" && toolCall.function.arguments) {
    try {
      const response = JSON.parse(toolCall.function.arguments);
      return {
        answer: response.answer,
        sources: response.sources,
      };
    } catch (e) {
      console.error("Failed to parse tool call response:", e);
      return {
        answer: "Sorry, I couldn't process the response.",
        sources: [],
      };
    }
  }

  return {
    answer: "Sorry, I don't know the answer to that.",
    sources: [],
  };
};

async function createEmbedding(text: string): Promise<number[]> {
  const apiResult = await api.embeddings.create({
    model: "text-embedding-3-small",
    input: text,
  });
  const embedding = apiResult.data[0].embedding;
  return embedding;
}
