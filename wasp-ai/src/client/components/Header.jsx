import { StatusPill } from "./StatusPill";
import { Title } from "./Title";

export function Header({ currentStatus, isStatusVisible }) {
  return (
    <div className="mb-4 bg-slate-50 p-8 rounded-xl md:flex justify-between items-center">
      <Title />
      {isStatusVisible && (
        <StatusPill status={currentStatus.status} className="hidden md:flex">
          {currentStatus.message}
        </StatusPill>
      )}
    </div>
  );
}
