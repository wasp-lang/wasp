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
  const dialogRef = useRef<HTMLDialogElement>(null);

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
    [open],
  );

  const handleClick = useCallback(
    (e: MouseEvent) => {
      const dialog = dialogRef.current;
      if (!closeOnClickOutside || !dialog) return;
      // Keyboard-generated clicks have (0, 0) coordinates. Only clicks on the
      // dialog itself can be backdrop clicks, never clicks on its children.
      if (e.target !== dialog) return;

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
    [closeOnClickOutside, onClose],
  );

  return createPortal(
    <dialog
      ref={dialogRef}
      className={twJoin(
        "top-[10dvh] mx-auto my-0 flex max-h-[80dvh] w-[min(32rem,100%-2rem)] p-0",
        "text-ink bg-transparent backdrop:bg-black/50",
      )}
      onClose={onClose}
      onClick={handleClick}
    >
      {children}
    </dialog>,
    document.body,
  );
}
