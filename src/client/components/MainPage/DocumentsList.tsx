import {
  Card,
  CardHeader,
  CardBody,
  Button,
  Divider,
  ScrollShadow,
} from "@nextui-org/react";

import getDocuments from "@wasp/queries/getDocuments";
import deleteDocument from "@wasp/actions/deleteDocument";
import { useQuery } from "@wasp/queries";
import { DocumentCard } from "../DocumentCard";

export function DocumentsList() {
  const { data: documents } = useQuery(getDocuments);
  return (
    <>
      {documents && documents.length > 0 && (
        <div className="mt-4">
          <h2 className="text-2xl font-bold">Documents</h2>
          {documents.map((doc) => (
            <DocumentCard
              key={doc.id}
              document={doc}
              onDelete={() => deleteDocument({ id: doc.id })}
            />
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
