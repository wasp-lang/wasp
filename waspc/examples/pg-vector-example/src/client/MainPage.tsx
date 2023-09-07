import { useState } from "react";
import {
  Card,
  CardHeader,
  CardBody,
  Button,
  Textarea,
  Divider,
  Input,
} from "@nextui-org/react";
import { useForm } from "react-hook-form";
import embedDocument from "@wasp/actions/embedDocument";
import getDocuments from "@wasp/queries/getDocuments";
import searchDocuments from "@wasp/actions/searchDocuments";
import { useQuery } from "@wasp/queries";

export function Main() {
  const [results, setResults] = useState<Awaited<
    ReturnType<typeof searchDocuments>
  > | null>(null);
  const addForm = useForm<{
    title: string;
    content: string;
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
  const query = searchForm.watch("query");

  return (
    <div className="max-w-4xl mx-auto p-4">
      <div className="grid grid-cols-2 gap-4">
        <Card className="w-[400px]">
          <CardHeader>Let's embed with Wasp and pg-vector</CardHeader>
          <Divider />
          <CardBody>
            <form onSubmit={onSubmit}>
              <div className="mb-2">
                <Input
                  {...addForm.register("title")}
                  label="Title"
                  placeholder="Title"
                />
              </div>
              <div className="mb-2">
                <Textarea
                  {...addForm.register("content")}
                  label="Content"
                  placeholder="Type something..."
                />
              </div>
              <Button type="submit" color="primary">
                Submit
              </Button>
            </form>
          </CardBody>
        </Card>
        <Card className="w-[400px]">
          <CardHeader>Search documents</CardHeader>
          <Divider />
          <CardBody>
            <form onSubmit={onSearch}>
              <div className="mb-2">
                <Input
                  {...searchForm.register("query")}
                  label="Query"
                  placeholder="Query"
                />
              </div>
              <Button type="submit" color="primary">
                Search
              </Button>
            </form>
          </CardBody>
        </Card>
      </div>

      {results && (
        <div className="mt-4">
          <h2 className="text-2xl font-bold">Results</h2>
          {results.map((result) => (
            <Card className="mt-2" key={result.document.id}>
              <CardHeader className="text-xl font-bold">
                {result.document.title} ({result.score})
              </CardHeader>
              <CardBody>
                <code>{result.document.content}</code>
              </CardBody>
            </Card>
          ))}
        </div>
      )}

      {documents && (
        <div className="mt-4">
          <h2 className="text-2xl font-bold">Documents</h2>
          {documents.map((doc) => (
            <Card className="mt-2" key={doc.id}>
              <CardHeader className="text-xl font-bold">{doc.title}</CardHeader>
              <CardBody>
                <code>{doc.content}</code>
              </CardBody>
            </Card>
          ))}
        </div>
      )}
    </div>
  );
}
