import React, { Suspense, lazy } from "react";
import {
  BreakPointHooks,
  breakpointsTailwind,
} from "@react-hooks-library/core";
import SectionContainer from "./Layouts/SectionContainer";
import { DiscordIcon } from "./Nav/SocialIcons";

const { useGreater } = BreakPointHooks(breakpointsTailwind);

const DaBoi = lazy(() => import("./DaBoi"));

export function DiscordCallout() {
  const isGreaterMd = useGreater("md");

  return (
    <SectionContainer className="space-y-16 lg:py-18">
      <div className="flex justify-between items-center bg-gray-200 rounded-xl p-16 pl-16 relative">
        <div className="space-y-6 max-w-prose">
          <div className="space-y-2">
            <h2 className="text-4xl font-bold text-gray-900">Meet Da Boi</h2>
            <p className="text-gray-600 text-lg leading-relaxed">
              Da Boi is our community mascot. He's a friendly little guy who
              loves to code and hang out with his friends. Join our Discord to
              meet him and the rest of the community!
            </p>
          </div>
          <a
            href="https://discord.gg/rzdnErX"
            className="inline-flex gap-2 items-center space-x-2
            px-3 py-2 rounded
            bg-yellow-500 text-white text-sm leading-4
            border border-yellow-500 hover:border-yellow-400
            hover:bg-yellow-400
            transition ease-out duration-200"
          >
            Join Discord <DiscordIcon />
          </a>
        </div>
        <div
          className="absolute right-1 top-1/2 -translate-y-1/2"
          style={{
            width: 400,
            height: 400,
          }}
        >
          {isGreaterMd && (
            <Suspense fallback={null}>
              <DaBoi height={400} width={400} />
            </Suspense>
          )}
        </div>
      </div>
    </SectionContainer>
  );
}
