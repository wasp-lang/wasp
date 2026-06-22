import { Moon, Sun } from "lucide-react";
import { Label } from "../../client/components/ui/label";
import { useColorMode } from "../hooks/useColorMode";
import { cn } from "../utils";

export function DarkModeSwitcher() {
  const [colorMode, setColorMode] = useColorMode();
  const isInLightMode = colorMode === "light";

  return (
    <div>
      <Label
        className={cn(
          "bg-muted h-7.5 relative m-0 block w-14 cursor-pointer rounded-full transition-colors duration-300 ease-in-out",
        )}
      >
        <input
          type="checkbox"
          aria-label="Toggle dark mode"
          checked={!isInLightMode}
          onChange={() => {
            if (typeof setColorMode === "function") {
              setColorMode(isInLightMode ? "dark" : "light");
            }
          }}
          className="absolute top-0 z-50 m-0 h-full w-full cursor-pointer opacity-0"
        />
        <span
          className={cn(
            "border-border absolute left-[3px] top-1/2 flex h-6 w-6 -translate-y-1/2 translate-x-0 items-center justify-center rounded-full border bg-white shadow-md transition-all duration-300 ease-in-out",
            {
              "right-[3px]! translate-x-full!": !isInLightMode,
            },
          )}
        >
          <ModeIcon isInLightMode={isInLightMode} />
        </span>
      </Label>
    </div>
  );
}

function ModeIcon({ isInLightMode }: { isInLightMode: boolean }) {
  const iconStyle =
    "absolute inset-0 flex items-center justify-center transition-opacity ease-in-out duration-300";
  return (
    <>
      <span
        className={cn(iconStyle, isInLightMode ? "opacity-100" : "opacity-0")}
      >
        <Sun className="size-4 fill-amber-500 stroke-amber-500" />
      </span>
      <span
        className={cn(iconStyle, !isInLightMode ? "opacity-100" : "opacity-0")}
      >
        <Moon className="size-4 fill-slate-600 stroke-slate-600" />
      </span>
    </>
  );
}
