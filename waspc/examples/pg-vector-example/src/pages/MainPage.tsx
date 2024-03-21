import {
  embedDocument,
  searchDocuments,
  deleteDocument,
  askDocuments,
  getScrapeCandidates,
  useQuery,
  getDocuments,
} from "wasp/client/operations";

import { useState, useEffect } from "react";
import {
  Card,
  CardHeader,
  CardBody,
  Button,
  Divider,
  Input,
  Tabs,
  Tab,
  CardFooter,
  ScrollShadow,
  Listbox,
  ListboxItem,
} from "@nextui-org/react";

import { useForm } from "react-hook-form";
import { SearchIcon } from "../components/SearchIcon";
import ReactMarkdown from "react-markdown";

export function Main() {
  return (
    <div className="pt-8 min-h-screen">
      <div className="max-w-4xl mx-auto p-4">
        <Tabs aria-label="Options" variant="bordered" color="primary">
          <Tab key="ask" title="Ask">
            <AskTheDocuments />
          </Tab>
          <Tab key="add" title="Add Document">
            <Card>
              <CardHeader>Let's embed with Wasp and pg-vector</CardHeader>
              <Divider />
              <CardBody>
                <Tabs aria-label="Adding Options">
                  <Tab key="text" title="URL Tree">
                    <UrlTreeForm />
                  </Tab>
                  <Tab key="url" title="Single URL">
                    <SingleDocumentForm />
                  </Tab>
                </Tabs>
              </CardBody>
            </Card>
            <DocumentsList />
          </Tab>
          <Tab key="search" title="Search">
            <SearchForm />
          </Tab>
        </Tabs>
      </div>
    </div>
  );
}

function DocumentsList() {
  const { data: documents } = useQuery(getDocuments);
  return (
    <>
      {documents && documents.length > 0 && (
        <div className="mt-4">
          <h2 className="text-2xl font-bold">Documents</h2>
          {documents.map((doc) => (
            <Card className="mt-2" key={doc.id}>
              <CardHeader className="flex justify-between items-center">
                <div className="p-2">
                  <h2 className="text-xl font-bold ">{doc.title}</h2>
                  <p className="text-content4">{doc.url}</p>
                </div>
                <div>
                  <Button
                    color="danger"
                    size="sm"
                    onClick={() =>
                      deleteDocument({
                        id: doc.id,
                      })
                    }
                  >
                    Delete
                  </Button>
                </div>
              </CardHeader>
              <Divider />
              <CardBody>
                <ScrollShadow className="max-h-[200px]">
                  {doc.content}
                </ScrollShadow>
              </CardBody>
            </Card>
          ))}
        </div>
      )}
      {documents && documents.length === 0 && (
        <Card className="mt-4">
          <CardBody>
            <h2 className="text-2xl font-bold">Documents</h2>
            <p>No documents yet.</p>
          </CardBody>
        </Card>
      )}
    </>
  );
}

function UrlTreeForm() {
  const form = useForm<{
    url: string;
    selector?: string;
  }>();
  const [results, setResults] = useState<Awaited<
    ReturnType<typeof getScrapeCandidates>
  > | null>(null);
  const onSubmit = form.handleSubmit(async (data) => {
    const response = await getScrapeCandidates(data);
    setResults(response);
  });

  const [selectedKeys, setSelectedKeys] = useState<Set<string>>(new Set([]));
  const [scrapedKeys, setScrapedKeys] = useState<Set<string>>(new Set([]));
  const [isScraping, setIsScraping] = useState(false);

  const selector = form.watch("selector");

  useEffect(() => {
    if (results) {
      setSelectedKeys(new Set(results.links));
    }
  }, [results]);

  async function startScraping() {
    setIsScraping(true);
    const selectedUrls = Array.from(selectedKeys.values());
    for (const url of selectedUrls) {
      try {
        await embedDocument({
          url,
          selector,
        });
        setScrapedKeys((keys) => new Set(keys.add(url)));
      } catch (e) {
        // Ignore for now
      }
    }
    setIsScraping(false);
  }

  return (
    <form onSubmit={onSubmit}>
      <div className="mb-2">
        <Input
          {...form.register("url", {
            required: "Website URL is required",
            validate: (value) => {
              try {
                new URL(value);
                return true;
              } catch (e) {
                return "Website URL is invalid";
              }
            },
          })}
          label="Website URL"
          variant="bordered"
          errorMessage={form.formState.errors.url?.message}
          isDisabled={!!results}
        />
      </div>
      <Button type="submit" color="primary" isDisabled={!!results}>
        Get Candidate URLs
      </Button>

      {results && (
        <div>
          <div className="flex justify-between items-center">
            <h2 className="text-2xl font-bold mt-4">Candidate URLs</h2>
            <Button
              onClick={() => {
                setResults(null);
                setSelectedKeys(new Set([]));
                setScrapedKeys(new Set([]));
              }}
            >
              Clear
            </Button>
          </div>
          <div className="mt-2">
            <ListboxWrapper>
              <Listbox
                aria-label="Multiple selection example"
                variant="flat"
                disallowEmptySelection
                selectionMode="multiple"
                selectedKeys={selectedKeys}
                onSelectionChange={(selectedKeys) => {
                  if (isScraping) {
                    return;
                  }
                  // @ts-ignore
                  setSelectedKeys(selectedKeys);
                }}
              >
                {results.links.map((link) => (
                  <ListboxItem
                    key={link}
                    startContent={
                      <span>{scrapedKeys.has(link) ? "✅" : "🔗"}</span>
                    }
                  >
                    {link}
                  </ListboxItem>
                ))}
              </Listbox>
            </ListboxWrapper>
          </div>
          <div className="mt-2">
            <Input
              {...form.register("selector")}
              description="Where to pull the content from? (it's `body` by default)"
              label="Content CSS Selector"
              variant="bordered"
              isDisabled={isScraping}
            />
          </div>
          <div className="mt-2">
            <Button
              onClick={startScraping}
              color="primary"
              isDisabled={isScraping}
            >
              Scrape Selected URLs
            </Button>
          </div>
        </div>
      )}
    </form>
  );
}

function SingleDocumentForm() {
  const addForm = useForm<{
    url: string;
    selector?: string;
  }>();
  const onSubmit = addForm.handleSubmit((data) => embedDocument(data));
  return (
    <form onSubmit={onSubmit}>
      <div className="mb-2">
        <Input
          {...addForm.register("url", {
            required: "Website URL is required",
            validate: (value) => {
              try {
                new URL(value);
                return true;
              } catch (e) {
                return "Website URL is invalid";
              }
            },
          })}
          label="Website URL"
          variant="bordered"
          errorMessage={addForm.formState.errors.url?.message}
        />
      </div>
      <div className="mb-2">
        <Input
          {...addForm.register("selector")}
          description="Where to pull the content from? (it's `body` by default)"
          label="Content CSS Selector"
          variant="bordered"
        />
      </div>
      <Button type="submit" color="primary">
        Add Document
      </Button>
    </form>
  );
}

function SearchForm() {
  const searchForm = useForm<{
    query: string;
  }>();
  const [results, setResults] = useState<Awaited<
    ReturnType<typeof searchDocuments>
  > | null>(null);
  const onSearch = searchForm.handleSubmit(async (data) => {
    const response = await searchDocuments(data);
    setResults(response);
  });
  return (
    <>
      <Card>
        <CardHeader>Search documents</CardHeader>
        <Divider />
        <CardBody>
          <form onSubmit={onSearch}>
            <Input
              {...searchForm.register("query")}
              placeholder="Type to search..."
              startContent={<SearchIcon size={18} />}
              type="search"
              variant="bordered"
            />
          </form>
        </CardBody>
      </Card>

      {results && (
        <div className="mt-4">
          {results.map((result) => (
            <Card className="mt-2" key={result.document.id}>
              <CardHeader className="text-xl font-bold">
                {result.document.title}
              </CardHeader>
              <Divider />
              <CardBody>
                <ScrollShadow className="max-h-[200px]">
                  {result.document.content}
                </ScrollShadow>
              </CardBody>
              <Divider />
              <CardFooter className="text-sm text-primary">
                Distance to query: {result.score}
              </CardFooter>
            </Card>
          ))}
        </div>
      )}
    </>
  );
}

function AskTheDocuments() {
  const form = useForm<{
    query: string;
  }>();
  const [answer, setAnswer] = useState<Awaited<
    ReturnType<typeof askDocuments>
  > | null>(null);
  const [isLoading, setIsLoading] = useState(false);
  const onSearch = form.handleSubmit(async (data) => {
    try {
      setIsLoading(true);
      const answer = await askDocuments(data);
      setAnswer(answer);
    } finally {
      setIsLoading(false);
    }
  });
  return (
    <>
      <Card>
        <CardHeader>Ask the documents</CardHeader>
        <Divider />
        <CardBody>
          <form onSubmit={onSearch}>
            <Input
              {...form.register("query")}
              placeholder="Type your question..."
              startContent={<SearchIcon size={18} />}
              type="search"
              variant="bordered"
              isDisabled={isLoading}
            />
          </form>
        </CardBody>
      </Card>
      {answer && (
        <div className="mt-4">
          <Card>
            <CardBody>
              <ReactMarkdown>{answer.answer}</ReactMarkdown>
            </CardBody>
          </Card>
        </div>
      )}
    </>
  );
}

const ListboxWrapper = ({ children }: { children: React.ReactNode }) => (
  <div className="border-small px-1 py-2 rounded-small border-default-200 dark:border-default-100">
    {children}
  </div>
);
