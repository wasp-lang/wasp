import {
  Button,
  Input,
  Modal,
  ModalBody,
  ModalContent,
  ModalFooter,
  ModalHeader,
} from "@nextui-org/react";
import { useState } from "react";

interface AddEntityModalProps {
  isOpen: boolean;
  onClose: () => void;
  onEntityAdd: (entityName: string) => void;
}

export function AddEntityModal({
  isOpen,
  onClose,
  onEntityAdd,
}: AddEntityModalProps) {
  const [entityName, setEntityName] = useState("");

  const handleSubmit = () => {
    if (entityName.trim()) {
      onEntityAdd(entityName);
      setEntityName("");
      onClose();
    }
  };

  const handleOnOpenChange = (open: boolean) => {
    if (!open) {
      setEntityName("");
      onClose();
    }
  };

  return (
    <Modal isOpen={isOpen} onOpenChange={handleOnOpenChange}>
      <ModalContent>
        <ModalHeader className="flex flex-col gap-1">
          Add New Entity
        </ModalHeader>
        <ModalBody>
          <Input
            autoFocus
            label="Entity Name"
            placeholder="Enter entity name"
            variant="bordered"
            value={entityName}
            onChange={(e) => setEntityName(e.target.value)}
            onKeyDown={(e) => {
              if (e.key === "Enter") {
                handleSubmit();
              }
            }}
          />
        </ModalBody>
        <ModalFooter>
          <Button color="default" variant="light" onPress={onClose}>
            Cancel
          </Button>
          <Button
            color="primary"
            onPress={handleSubmit}
            isDisabled={!entityName.trim()}
          >
            Add Entity
          </Button>
        </ModalFooter>
      </ModalContent>
    </Modal>
  );
}
