import React from "react";
import { twJoin } from "tailwind-merge";

interface DialogProps extends React.PropsWithChildren {
  open: boolean;
  onClose: () => void;
  closeOnClickOutside?: boolean;
}

export function Dialog({
  open,
  onClose,
  children,
  closeOnClickOutside = true,
}: DialogProps) {
  const dialogRef = React.useRef<HTMLDialogElement>(null);

  React.useEffect(
    function handleShowOrCloseDialog() {
      const dialog = dialogRef.current;
      if (!dialog) return;

      if (open && !dialog.open) {
        dialog.showModal();
      } else if (!open && dialog.open) {
        dialog.close();
      }
    },
    [open],
  );

  React.useEffect(
    function handleCloseOnClickOutside() {
      if (!closeOnClickOutside) return;

      const dialog = dialogRef.current;
      if (!dialog) return;

      const handleClick = (e: MouseEvent) => {
        const rect = dialog.getBoundingClientRect();
        const clickedOutside =
          e.clientX < rect.left ||
          e.clientX > rect.right ||
          e.clientY < rect.top ||
          e.clientY > rect.bottom;

        if (clickedOutside) {
          onClose();
        }
      };

      dialog.addEventListener("click", handleClick);
      return () => {
        dialog.removeEventListener("click", handleClick);
      };
    },
    [closeOnClickOutside, onClose],
  );

  React.useEffect(
    function handlePreventScroll() {
      if (!open) return;

      const originalOverflow = document.body.style.overflow;
      document.body.style.overflow = "hidden";
      return () => {
        document.body.style.overflow = originalOverflow;
      };
    },
    [open],
  );

  return (
    <dialog
      ref={dialogRef}
      className={twJoin(
        "max-h top-[20vh] my-0 flex max-h-[55vh]",
        "bg-transparent backdrop:bg-black/50 backdrop:backdrop-blur-sm",
      )}
      onClose={onClose}
    >
      {children}
    </dialog>
  );
}
