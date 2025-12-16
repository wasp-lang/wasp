import {
  Button,
  Card,
  CardBody,
  CardFooter,
  CardHeader,
  Divider,
  ScrollShadow,
} from "@heroui/react";
import { searchDocuments } from "wasp/client/operations";

import Markdown from "react-markdown";
import { LinkIcon } from "./LinkIcons";

type Document = Awaited<ReturnType<typeof searchDocuments>>[number]["document"];

export function DocumentCard({
  document,
  onDelete,
  footerContent,
}: {
  document: Document;
  onDelete?: () => void;
  footerContent?: React.ReactNode;
}) {
  return (
    <Card className="mt-2" key={document.id}>
      <CardHeader className="flex items-center justify-between">
        <div className="p-2">
          <h2 className="mb-1 text-xl font-bold">{document.title}</h2>
          <p className="text-content4">
            <a
              href={document.url}
              target="_blank"
              className="flex items-center gap-1"
            >
              <LinkIcon size={16} /> {document.url}
            </a>
          </p>
        </div>
        {onDelete && (
          <div>
            <Button color="danger" size="sm" onClick={onDelete}>
              Delete
            </Button>
          </div>
        )}
      </CardHeader>
      <Divider />
      <CardBody>
        <ScrollShadow className="max-h-[200px]">
          <div className="markdown">
            <Markdown>{document.content}</Markdown>
          </div>
        </ScrollShadow>
      </CardBody>
      {footerContent && (
        <>
          <Divider />
          <CardFooter className="text-primary text-sm">
            {footerContent}
          </CardFooter>
        </>
      )}
    </Card>
  );
}
