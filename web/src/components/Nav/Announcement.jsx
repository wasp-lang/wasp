import { useHistory } from "@docusaurus/router";

const ANNOUNCEMENT_LINK = "/blog/2026/06/15/wasp-typescript-spec";

const Announcement = () => {
  const history = useHistory();

  const handleClick = () => {
    history.push(ANNOUNCEMENT_LINK);
  };

  return (
    <div
      onClick={handleClick}
      className="group cursor-pointer border-b-2 border-wasp-black bg-[#3178C6] text-wasp-white transition-colors hover:bg-[#235A97]"
    >
      <div className="mx-auto flex items-center justify-center gap-3 px-4 py-2 font-mono text-xs font-medium tracking-wide lg:container lg:px-16 lg:text-sm">
        <b className="uppercase tracking-widest">
          Wasp is now 100% TypeScript
        </b>
        <span className="inline-flex items-center border-2 border-wasp-black bg-wasp-yellow px-2 py-0.5 font-mono text-[10px] font-bold uppercase tracking-widest text-wasp-black transition-colors group-hover:bg-wasp-yellow-dark lg:text-xs">
          Meet TS Spec <span className="ml-1">→</span>
        </span>
      </div>
    </div>
  );
};

export default Announcement;
