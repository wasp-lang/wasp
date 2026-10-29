import { FormEvent, useState } from "react";

export const WaspV1Signup = ({ source }: { source: string }) => {
  const [email, setEmail] = useState("");
  const [message, setMessage] = useState("");

  const handleSubmit = async (event: FormEvent<HTMLFormElement>) => {
    event.preventDefault();
    const handleError = (error: unknown) => {
      console.error("Loops signup failed:", error);
      setMessage("🛑 Oops! Something went wrong. Please try again.");
    };
    try {
      const response = await fetch(
        "https://app.loops.so/api/newsletter-form/clg0zndc9000ajn0f8a1bhgmu",
        {
          method: "POST",
          body: new URLSearchParams({
            email,
            mailingLists: "cmvfc4mns050y0kzb72vw51px",
            source,
          }).toString(),
          headers: { "Content-Type": "application/x-www-form-urlencoded" },
        },
      );
      if (response.ok) {
        setMessage("Thank you for subscribing! 🙏");
      } else {
        handleError(`HTTP ${response.status}: ${await response.text()}`);
      }
    } catch (error) {
      handleError(error);
    }
  };

  return (
    <div className="mb-6 flex flex-col items-start space-y-3 rounded-lg border border-yellow-500/25 bg-yellow-500/20 p-8">
      <p className="text-xl font-semibold">Get an email when Wasp 1.0 lands!</p>
      {message ? (
        <p className="border-2 border-wasp-black bg-wasp-yellow-light px-4 py-3 font-mono text-sm font-bold text-wasp-black">
          {message}
        </p>
      ) : (
        <form onSubmit={handleSubmit} className="self-stretch sm:flex">
          <input
            aria-label="Email address"
            type="email"
            required
            autoComplete="email"
            placeholder="you@awesomedev.com"
            value={email}
            onChange={(e) => setEmail(e.target.value)}
            className="w-full appearance-none border-2 border-wasp-black bg-wasp-white px-4 py-2.5 font-mono text-sm text-wasp-black placeholder:text-wasp-g4 focus:outline-none focus:ring-2 focus:ring-wasp-yellow-dark"
          />
          <div className="mt-3 sm:ml-3 sm:mt-0">
            <button
              type="submit"
              className="w-full whitespace-nowrap border-2 border-wasp-black bg-wasp-yellow px-5 py-2.5 font-mono text-sm font-bold uppercase tracking-wide text-wasp-black transition duration-150 ease-out hover:bg-wasp-yellow-dark"
            >
              Subscribe
            </button>
          </div>
        </form>
      )}
    </div>
  );
};
