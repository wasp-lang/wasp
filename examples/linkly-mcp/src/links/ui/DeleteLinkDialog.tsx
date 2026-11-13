import { toast } from "sonner";
import { deleteLink } from "wasp/client/operations";
import { errorMessage } from "../../app/ui/errors";
import {
  AlertDialog,
  AlertDialogAction,
  AlertDialogCancel,
  AlertDialogContent,
  AlertDialogDescription,
  AlertDialogFooter,
  AlertDialogHeader,
  AlertDialogTitle,
} from "../../components/ui/alert-dialog";

// The dashboard's delete confirmation: the human is already in the product,
// so an in-UI dialog is enough (no confirmation link).
export function DeleteLinkDialog({
  link,
  open,
  onOpenChange,
  onDeleted,
}: {
  link: { id: string; slug: string; destinationUrl: string };
  open: boolean;
  onOpenChange: (open: boolean) => void;
  onDeleted?: () => void;
}) {
  return (
    <AlertDialog open={open} onOpenChange={onOpenChange}>
      <AlertDialogContent>
        <AlertDialogHeader>
          <AlertDialogTitle>Delete /r/{link.slug}?</AlertDialogTitle>
          <AlertDialogDescription>
            The short link to {link.destinationUrl} stops working and its click
            history is deleted. This can't be undone.
          </AlertDialogDescription>
        </AlertDialogHeader>
        <AlertDialogFooter>
          <AlertDialogCancel>Cancel</AlertDialogCancel>
          <AlertDialogAction
            variant="destructive"
            onClick={async () => {
              try {
                await deleteLink({ id: link.id });
                toast.success(`Deleted /r/${link.slug}`);
                onDeleted?.();
              } catch (e) {
                toast.error(errorMessage(e));
              }
            }}
          >
            Delete link
          </AlertDialogAction>
        </AlertDialogFooter>
      </AlertDialogContent>
    </AlertDialog>
  );
}
