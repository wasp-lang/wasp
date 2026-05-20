import Link from "@docusaurus/Link";

const Logo = () => (
  <div className="wasp-logo flex flex-shrink-0 items-center">
    <Link to="/">
      <img
        src="/img/wasp-logo.svg"
        alt="Wasp Logo"
        className="border-2 border-black"
        width="44"
        height="44"
      />
    </Link>
    <span className="wasp-logo-text ml-3 font-mono text-xl font-extrabold tracking-tight text-wasp-black">
      wasp<span className="ml-1.5 text-sm font-normal text-wasp-g4">(beta)</span>
    </span>
  </div>
);

export default Logo;
