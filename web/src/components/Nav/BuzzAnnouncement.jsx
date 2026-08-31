import { useHistory } from "@docusaurus/router";

const ANNOUNCEMENT_LINK = "/blog/2026/07/30/buzz-wasp-one-message-app";

const BuzzAnnouncement = () => {
  const history = useHistory();

  const handleClick = () => {
    history.push(ANNOUNCEMENT_LINK);
  };

  return (
    <div
      onClick={handleClick}
      className="group cursor-pointer border-b-2 border-wasp-black bg-wasp-purple-light text-wasp-black transition-colors hover:bg-wasp-purple hover:text-wasp-white"
    >
      <div className="mx-auto flex items-center justify-center gap-3 px-4 py-2 font-mono text-xs font-medium tracking-wide lg:container lg:px-16 lg:text-sm">
        <b className="uppercase tracking-widest">
          how to use Wasp with{" "}
          <b className="inline-block scale-125 px-1.5 text-base tracking-normal blur-[0.75px] lg:text-lg">
            Buzz
          </b>{" "}
          agents 🐝
        </b>
        <span className="inline-flex items-center border-2 border-wasp-black bg-wasp-yellow px-2 py-0.5 font-mono text-[10px] font-bold uppercase tracking-widest text-wasp-black transition-colors group-hover:bg-wasp-yellow-dark lg:text-xs">
          Get the guide <span className="ml-1">→</span>
        </span>
      </div>
    </div>
  );
};

export default BuzzAnnouncement;
