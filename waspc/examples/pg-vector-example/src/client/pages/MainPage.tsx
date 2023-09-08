import { useState } from "react";
import {
  Card,
  CardHeader,
  CardBody,
  Button,
  Textarea,
  Divider,
  Input,
  Tabs,
  Tab,
  CardFooter,
  ScrollShadow,
} from "@nextui-org/react";

import { useForm } from "react-hook-form";
import embedDocument from "@wasp/actions/embedDocument";
import getDocuments from "@wasp/queries/getDocuments";
import searchDocuments from "@wasp/actions/searchDocuments";
import deleteDocument from "@wasp/actions/deleteDocument";
import { useQuery } from "@wasp/queries";
import { SearchIcon } from "../components/SearchIcon";

export function Main() {
  const [results, setResults] = useState<Awaited<
    ReturnType<typeof searchDocuments>
  > | null>(null);
  const addForm = useForm<{
    url: string;
    selector?: string;
  }>();
  const searchForm = useForm<{
    query: string;
  }>();
  const onSubmit = addForm.handleSubmit((data) => embedDocument(data));
  const onSearch = searchForm.handleSubmit(async (data) => {
    const response = await searchDocuments(data);
    setResults(response);
  });
  const { data: documents } = useQuery(getDocuments);

  return (
    <div className="pt-8 min-h-screen">
      <div className="max-w-4xl mx-auto p-4">
        <Tabs aria-label="Options">
          <Tab key="add" title="Add Document">
            <Card>
              <CardHeader>Let's embed with Wasp and pg-vector</CardHeader>
              <Divider />
              <CardBody>
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
              </CardBody>
            </Card>
            {documents && documents.length > 0 && (
              <div className="mt-4">
                <h2 className="text-2xl font-bold">Documents</h2>
                {documents.map((doc) => (
                  <Card className="mt-2" key={doc.id}>
                    <CardHeader className="text-xl font-bold flex justify-between items-center">
                      <div className="p-2">{doc.title}</div>
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
          </Tab>
          <Tab key="search" title="Search">
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
          </Tab>
        </Tabs>
      </div>
    </div>
  );
}
