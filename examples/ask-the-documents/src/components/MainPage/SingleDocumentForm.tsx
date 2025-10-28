import { useForm } from "react-hook-form";
import { embedDocument } from "wasp/client/operations";

import { Button, Input } from "@heroui/react";

export function SingleDocumentForm() {
  const addForm = useForm<{
    url: string;
    selector?: string;
  }>();
  const onSubmit = addForm.handleSubmit((data) => embedDocument(data));
  return (
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
  );
}
