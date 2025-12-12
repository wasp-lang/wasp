import {
  Accordion,
  AccordionItem,
  Card,
  CardBody,
  CardHeader,
  Divider,
  Input,
  Link,
} from "@heroui/react";
import { useForm } from "react-hook-form";
import { askDocuments } from "wasp/client/operations";

import { useMutation } from "@tanstack/react-query";
import { useMemo } from "react";
import Markdown from "react-markdown";
import { SearchIcon } from "../SearchIcon";

export function AskTheDocumentsForm() {
  const form = useForm<{
    query: string;
  }>();
  const askDocumentsMutation = useMutation(askDocuments);
  const onSearch = form.handleSubmit(async (data) => {
    await askDocumentsMutation.mutateAsync(data);
  });

  // Group sources by URL
  const groupedSources = useMemo(() => {
    if (!askDocumentsMutation.data?.sources) return [];

    const sourceMap = new Map<string, Array<string>>();

    askDocumentsMutation.data.sources.forEach((source) => {
      if (!sourceMap.has(source.url)) {
        sourceMap.set(source.url, [source.part_of_text]);
      } else {
        sourceMap.get(source.url)?.push(source.part_of_text);
      }
    });

    return Array.from(sourceMap).map(([url, texts], index) => ({
      index,
      url,
      texts,
    }));
  }, [askDocumentsMutation.data?.sources]);

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
              isDisabled={askDocumentsMutation.isLoading}
            />
          </form>
        </CardBody>
      </Card>
      {askDocumentsMutation.data && (
        <div className="mt-4">
          <Card>
            <CardBody>
              <div className="markdown">
                <Markdown>{askDocumentsMutation.data.answer}</Markdown>
              </div>

              {groupedSources.length > 0 && (
                <>
                  <Divider className="my-4" />
                  <div>
                    <div className="space-y-3">
                      {groupedSources.map((source) => (
                        <div key={source.index}>
                          <div className="flex items-center space-x-2 px-2">
                            <Link href={source.url} className="text-blue-500">
                              {source.url}
                            </Link>
                          </div>
                          <Accordion className="mt-1">
                            <AccordionItem
                              key={`source-${source.index}`}
                              aria-label={`Show source text ${source.index}`}
                              title={
                                <span className="text-base">
                                  Show source text ({source.texts.length}{" "}
                                  {source.texts.length === 1
                                    ? "excerpt"
                                    : "excerpts"}
                                  )
                                </span>
                              }
                            >
                              <div className="space-y-4">
                                {source.texts.map((text, i) => (
                                  <div key={i} className="text-sm">
                                    {text}
                                  </div>
                                ))}
                              </div>
                            </AccordionItem>
                          </Accordion>
                        </div>
                      ))}
                    </div>
                  </div>
                </>
              )}
            </CardBody>
          </Card>
        </div>
      )}
    </>
  );
}
