import { askDocuments } from "wasp/client/operations";
import { useState } from "react";
import { useForm } from "react-hook-form";
import { Card, CardHeader, CardBody, Divider, Input } from "@nextui-org/react";

import ReactMarkdown from "react-markdown";
import { SearchIcon } from "../SearchIcon";

export function AskTheDocumentsForm() {
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
              <ReactMarkdown className="markdown">
                {answer.answer}
              </ReactMarkdown>
            </CardBody>
          </Card>
        </div>
      )}
    </>
  );
}
