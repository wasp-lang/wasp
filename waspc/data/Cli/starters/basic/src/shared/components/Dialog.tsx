import { MouseEvent, ReactNode, useCallback, useEffect, useRef } from "react";
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
  const dialogRef = useRef<HTMLDialogElement >(null);

  useEffect(
    function handleShowOrCloseDialog() {
      const dialog = dialogRef.current;
      if (!dialog) return;

      if (open && !dialog.open) {
        dialog.showModal();
      } else if (!open && dialog.open) {
        dialog.close();
      }
    },
    [open, dialogRef],
  );

  const handleClick = useCallback(
    (e: MouseEvent) => {
      const dialog = dialogRef.current;
      if (!closeOnClickOutside || !dialog) return;

      const rect = dialog.getBoundingClientRect();
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
      ref={dialogRef}
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
