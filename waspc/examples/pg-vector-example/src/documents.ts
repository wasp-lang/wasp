import { type Document } from "wasp/entities";

import {
  type EmbedDocument,
  type SearchDocuments,
  type AskDocuments,
  type DeleteDocument,
  type GetScrapeCandidates,
  type GetDocuments,
} from "wasp/server/operations";

import { prisma } from "wasp/server";
// @ts-ignore
import { toSql } from "pgvector/utils";
import openai from "openai";
import { getContent, getLinksToScrape } from "./scrape.js";

if (!process.env.OPENAI_API_KEY) {
  throw new Error("OPENAI_API_KEY env var is not set");
}

const api = new openai.OpenAI({
  apiKey: process.env.OPENAI_API_KEY,
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
> = async (args) => {
  const { url, selector } = args;

  // Scrape url to get the title and content
  const { title, content } = await getContent(url, selector);

  const embedding = toSql(await createEmbedding(content));

  await prisma.$queryRaw`
    INSERT INTO "Document" ("id", "title", "content", "embedding", "url", "updatedAt")
    VALUES (gen_random_uuid(), ${title}, ${content}, ${embedding}::vector, ${url}, ${new Date()})
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
> = async (args) => {
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
  const { id } = args;
  await context.entities.Document.delete({
    where: { id },
  });
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
};

export const askDocuments: AskDocuments<
  AskDocumentsInput,
  AskDocumentsOutput
> = async (args, context) => {
  const { query } = args;
  // Find top 3 most relevant documents
  const queryEmbedding = await createEmbedding(query);

  const result = (await prisma.$queryRaw`
    SELECT "content", "embedding" <-> ${toSql(
      queryEmbedding
    )}::vector AS "score", "url"
    FROM "Document"
    ORDER BY "embedding" <-> ${toSql(queryEmbedding)}::vector
    LIMIT 2;
  `) as {
    content: string;
    score: number;
    url: string;
  }[];

  const prompt = `Provide an aswer to the following: ${query}
  
  You can use the following documents:
  ${result.map((r) => `${r.content}\nSource URL: ${r.url}`).join("\n\n")}`;

  const completion = await api.chat.completions.create({
    messages: [
      {
        role: "system",
        content:
          "You are a Q&A system. Respond concisiely. Do not make it conversational. Mention the source URL. Respond in Markdown.",
      },
      { role: "user", content: prompt.slice(0, 4000) },
    ],
    model: "gpt-3.5-turbo",
  });

  const content = completion.choices[0].message.content;

  if (!content) {
    return { answer: "Sorry, I don't know the answer to that." };
  }

  return { answer: content };
};

async function createEmbedding(text: string): Promise<number[]> {
  const apiResult = await api.embeddings.create({
    model: "text-embedding-ada-002",
    input: text,
  });
  const embedding = apiResult.data[0].embedding;
  return embedding;
}
