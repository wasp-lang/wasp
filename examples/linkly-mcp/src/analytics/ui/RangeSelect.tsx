import {
  Select,
  SelectContent,
  SelectItem,
  SelectTrigger,
  SelectValue,
} from "../../components/ui/select";
import { RANGE_LABELS, RANGES, type Range } from "../ranges";

export function parseRange(value: string | null): Range {
  return RANGES.includes(value as Range) ? (value as Range) : "7d";
}

export function RangeSelect({
  value,
  onChange,
}: {
  value: Range;
  onChange: (range: Range) => void;
}) {
  return (
    <Select value={value} onValueChange={(v) => onChange(v as Range)}>
      <SelectTrigger aria-label="Time range" className="w-44">
        <SelectValue />
      </SelectTrigger>
      <SelectContent>
        {RANGES.map((r) => (
          <SelectItem key={r} value={r}>
            {RANGE_LABELS[r]}
          </SelectItem>
        ))}
      </SelectContent>
    </Select>
  );
}
