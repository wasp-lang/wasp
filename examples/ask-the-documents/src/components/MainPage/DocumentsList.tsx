import {
  Button,
  Card,
  CardBody,
  Modal,
  ModalBody,
  ModalContent,
  ModalFooter,
  ModalHeader,
  useDisclosure,
} from "@heroui/react";
import {
  deleteAllDocuments,
  deleteDocument,
  getDocuments,
  useQuery,
} from "wasp/client/operations";

import { DocumentCard } from "../DocumentCard";

export function DocumentsList() {
  const { data: documents, isLoading } = useQuery(getDocuments);
  const deleteAllDocumentsModal = useDisclosure();

  const handleDeleteAll = async () => {
    await deleteAllDocuments();
    deleteAllDocumentsModal.onClose();
  };

  return (
    <>
      <div className="my-4 flex items-center justify-between">
        <h2 className="text-2xl font-bold">Documents</h2>
        {documents && documents.length > 0 && (
          <Button
            color="danger"
            variant="flat"
            onPress={deleteAllDocumentsModal.onOpen}
          >
            Delete All
          </Button>
        )}
      </div>

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

      <Modal
        isOpen={deleteAllDocumentsModal.isOpen}
        onOpenChange={deleteAllDocumentsModal.onOpenChange}
      >
        <ModalContent>
          <ModalHeader className="flex flex-col gap-1">
            Delete All Documents
          </ModalHeader>
          <ModalBody>
            <p>
              Are you sure you want to delete all documents? This action cannot
              be undone.
            </p>
          </ModalBody>
          <ModalFooter>
            <Button
              color="default"
              variant="light"
              onPress={deleteAllDocumentsModal.onClose}
            >
              Cancel
            </Button>
            <Button color="danger" onPress={handleDeleteAll}>
              Delete All
            </Button>
          </ModalFooter>
        </ModalContent>
      </Modal>
    </>
  );
}
