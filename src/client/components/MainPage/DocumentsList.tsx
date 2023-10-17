import { Card, CardBody } from "@nextui-org/react";

import getDocuments from "@wasp/queries/getDocuments";
import deleteDocument from "@wasp/actions/deleteDocument";
import { useQuery } from "@wasp/queries";
import { DocumentCard } from "../DocumentCard";

export function DocumentsList() {
  const { data: documents, isLoading } = useQuery(getDocuments);
  return (
    <>
      <h2 className="text-2xl font-bold">Documents</h2>
      {documents && documents.length > 0 && (
        <div className="mt-4">
          {documents.map((doc) => (
            <DocumentCard
              key={doc.id}
              document={doc}
              onDelete={() => deleteDocument({ id: doc.id })}
            />
          ))}
        </div>
      )}
      {isLoading && (
        <Card className="mt-4">
          <CardBody>
            <p>Loading...</p>
          </CardBody>
        </Card>
      )}
      {documents && documents.length === 0 && (
        <Card className="mt-4">
          <CardBody>
            <p>No documents yet.</p>
          </CardBody>
        </Card>
      )}
    </>
  );
}
