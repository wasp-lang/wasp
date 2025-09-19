import { searchDocuments } from "wasp/client/operations";
import { useState } from "react";
import { useForm } from "react-hook-form";

import { Card, CardHeader, CardBody, Divider, Input } from "@heroui/react";
import { SearchIcon } from "../SearchIcon";
import { DocumentCard } from "../DocumentCard";

export function SearchForm() {
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
            <DocumentCard
              key={result.document.id}
              document={result.document}
              footerContent={`Score: ${10 / result.score}`}
            />
          ))}
        </div>
      )}
    </>
  );
}
