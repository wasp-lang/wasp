import { MouseEvent, ReactNode, useCallback, useEffect, useState } from "react";
import { createPortal } from "react-dom";
import { twJoin } from "tailwind-merge";

interface DialogProps {
  open: boolean;
  onClose: () => void;
  closeOnClickOutside?: boolean;
  children?: ReactNode;
}

export function Dialog({
  open,
  onClose,
  children,
  closeOnClickOutside = true,
}: DialogProps) {
  const [dialogRef, setDialogRef] = useState<HTMLDialogElement | null>(null);

  useEffect(
    function handleShowOrCloseDialog() {
      if (!dialogRef) return;

      if (open && !dialogRef.open) {
        dialogRef.showModal();
      } else if (!open && dialogRef.open) {
        dialogRef.close();
      }
    },
    [open, dialogRef],
  );

  const handleClick = useCallback(
    (e: MouseEvent) => {
      if (!closeOnClickOutside || !dialogRef) return;

      const rect = dialogRef.getBoundingClientRect();
      const clickedOutside =
        e.clientX < rect.left ||
        e.clientX > rect.right ||
        e.clientY < rect.top ||
        e.clientY > rect.bottom;

      if (clickedOutside) {
        onClose();
      }
    },
    [closeOnClickOutside, onClose, dialogRef],
  );

  return createPortal(
    <dialog
      ref={setDialogRef}
      className={twJoin(
        "max-h top-[20vh] my-0 flex max-h-[55vh]",
        "bg-transparent backdrop:bg-black/50 backdrop:backdrop-blur-sm",
      )}
      onClose={onClose}
      onClick={handleClick}
    >
      {children}
    </dialog>,
    document.body,
  );
}
