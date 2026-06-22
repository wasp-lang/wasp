import { Ellipsis, SquarePen, Trash2 } from "lucide-react";
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuTrigger,
} from "../../../client/components/ui/dropdown-menu";

export function DropdownEditDelete() {
  return (
    <DropdownMenu>
      <DropdownMenuTrigger asChild>
        <button>
          <Ellipsis className="size-4" />
        </button>
      </DropdownMenuTrigger>
      <DropdownMenuContent align="end" className="w-40">
        <DropdownMenuItem>
          <SquarePen className="mr-2 size-4" />
          Edit
        </DropdownMenuItem>
        <DropdownMenuItem>
          <Trash2 className="mr-2 size-4" />
          Delete
        </DropdownMenuItem>
      </DropdownMenuContent>
    </DropdownMenu>
  );
}
