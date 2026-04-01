import { VCSection } from "./vcVariant";

const cards = [
  {
    title: "Open Source",
    detail: "Free forever. MIT licensed. No hidden costs.",
  },
  {
    title: "Self-Hostable",
    detail: "Deploy to your own server. No vendor dependency.",
  },
  {
    title: "Eject Anytime",
    detail: "Wasp compiles to standard code. Leave whenever you want.",
  },
];

const VCSelfHosting = ({ variant }) => {
  return (
    <VCSection variant={variant}>
      <div className="mx-auto max-w-3xl text-center">
        <h2 className="mb-4 text-xl text-neutral-700 lg:text-2xl">
          Your code. Your server.{" "}
          <span className="underline decoration-yellow-500">No lock-in.</span>
        </h2>
        <p className="text-neutral-500">
          Wasp compiles to standard React, Node.js, and Prisma. Deploy anywhere
          — your own server, Fly.io, Railway, or any cloud provider. If you ever
          want to leave, just eject and take your code with you.
        </p>
      </div>

      <div className="mt-16 grid grid-cols-1 gap-8 md:grid-cols-3">
        {cards.map((card) => (
          <div key={card.title} className="text-center">
            <h3 className="mb-2 text-lg font-semibold text-neutral-700">
              {card.title}
            </h3>
            <p className="text-neutral-500">{card.detail}</p>
          </div>
        ))}
      </div>
    </VCSection>
  );
};

export default VCSelfHosting;
