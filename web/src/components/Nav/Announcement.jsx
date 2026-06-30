import { useHistory } from "@docusaurus/router";

const ANNOUNCEMENT_LINK = "/blog/2026/06/05/wasp-launch-week-12-ts-spec";

const Announcement = () => {
  const history = useHistory();

  const handleClick = () => {
    history.push(ANNOUNCEMENT_LINK);
  };

  return (
    <div
      onClick={handleClick}
      className="cursor-pointer border-b-2 border-wasp-black bg-wasp-yellow text-wasp-black transition-colors hover:bg-wasp-yellow-dark"
    >
      <div className="mx-auto flex items-center justify-center gap-3 px-4 py-2 font-mono text-xs font-medium tracking-wide lg:container lg:px-16 lg:text-sm">
        <span className="hidden sm:inline">🦋</span>
        <span>
          <b className="uppercase tracking-widest">Launch Week #12 — TS Spec</b>
          <span className="mx-2 hidden sm:inline">·</span>
          <span className="hidden sm:inline">
            Wasp goes fully TypeScript-native.
          </span>
        </span>
        <span className="inline-flex items-center border-2 border-wasp-black bg-wasp-bg px-2 py-0.5 font-mono text-[10px] font-bold uppercase tracking-widest lg:text-xs">
          Kickoff Mon, Jun 15 <span className="ml-1">→</span>
        </span>
      </div>
    </div>
  );
};

export default Announcement;
