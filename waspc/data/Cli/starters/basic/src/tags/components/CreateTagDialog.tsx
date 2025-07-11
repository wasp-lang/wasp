import React from "react";
import { Button } from "../../shared/components/Button";
import { Dialog } from "../../shared/components/Dialog";
import { Portal } from "../../shared/components/Portal";
import { CREATE_TAG_FORM_ID, CreateTagForm } from "./CreateTagForm";

export function CreateTagDialog() {
  const [tagDialogOpen, setTagDialogOpen] = React.useState(false);

  return (
    <>
      <Button
        className="flex items-center gap-2"
        type="button"
        size="sm"
        onClick={() => setTagDialogOpen(true)}
      >
        <span>Add a Tag</span>
        <span>+</span>
      </Button>
      {tagDialogOpen && (
        <Portal>
          <Dialog open={tagDialogOpen} onClose={() => setTagDialogOpen(false)}>
            <section className="card relative flex flex-col">
              <header className="px-4 py-6 lg:px-6 lg:py-8">
                <h2 className="text-xl font-semibold">Create a new tag</h2>
              </header>
              <div className="overflow-y-auto p-4 lg:p-6">
                <CreateTagForm onTagCreated={() => setTagDialogOpen(false)} />
              </div>
              <footer className="flex justify-end gap-2 px-4 py-6 lg:px-6 lg:py-8">
                <Button form={CREATE_TAG_FORM_ID} type="submit">
                  Create
                </Button>
                <Button
                  form={CREATE_TAG_FORM_ID}
                  type="button"
                  onClick={() => setTagDialogOpen(false)}
                  variant="ghost"
                >
                  Cancel
                </Button>
              </footer>
            </section>
          </Dialog>
        </Portal>
      )}
    </>
  );
}
