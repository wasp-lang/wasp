import { useId, useState } from "react";
import { Button } from "../../shared/components/Button";
import { Dialog } from "../../shared/components/Dialog";
import { CreateTagForm } from "./CreateTagForm";

export function CreateTagDialog() {
  const formId = useId();
  const [tagDialogOpen, setTagDialogOpen] = useState(false);

  return (
    <>
      <Button
        className="flex items-center gap-2"
        type="button"
        size="sm"
        variant="secondary"
        onClick={() => setTagDialogOpen(true)}
      >
        <span>Add a Tag</span>
        <span>+</span>
      </Button>
      {tagDialogOpen && (
        <Dialog open={tagDialogOpen} onClose={() => setTagDialogOpen(false)}>
          <section className="card relative flex flex-col">
            <header className="border-b border-neutral-900 px-4 py-4 lg:px-6">
              <h2 className="font-mono text-sm font-bold tracking-wide uppercase">
                Create a new tag
              </h2>
            </header>
            <div className="overflow-y-auto p-4 lg:p-6">
              <CreateTagForm
                id={formId}
                onTagCreated={() => setTagDialogOpen(false)}
              />
            </div>
            <footer className="flex justify-end gap-2 border-t border-neutral-900 px-4 py-4 lg:px-6">
              <Button form={formId} type="submit">
                Create
              </Button>
              <Button
                type="button"
                onClick={() => setTagDialogOpen(false)}
                variant="secondary"
              >
                Cancel
              </Button>
            </footer>
          </section>
        </Dialog>
      )}
    </>
  );
}
