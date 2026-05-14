import { jsxs, jsx } from "react/jsx-runtime";
const Logo = "data:image/svg+xml,%3csvg%20id='Layer_1'%20data-name='Layer%201'%20xmlns='http://www.w3.org/2000/svg'%20viewBox='0%200%20161%20161'%3e%3cdefs%3e%3cstyle%3e.cls-1{fill:%23f5cc05;}.cls-2{fill-rule:evenodd;}%3c/style%3e%3c/defs%3e%3cg%20id='Page-1'%3e%3cg%20id='Group-23'%3e%3ccircle%20id='Oval'%20class='cls-1'%20cx='80.5'%20cy='80.5'%20r='79'/%3e%3cg%20id='Group-36'%3e%3cg%20id='_'%20data-name='}'%3e%3cpath%20id='path-2'%20class='cls-2'%20d='M88.67,114.33h2.91q6,0,7.87-1.89c1.22-1.25,1.83-3.9,1.83-7.93V93.89c0-4.46.65-7.7,1.93-9.73s3.51-3.43,6.67-4.2q-4.69-1.08-6.65-4.12c-1.3-2-2-5.28-2-9.77V55.44q0-6-1.83-7.93t-7.87-1.88H88.67V39.5h2.65q10.65,0,14.24,3.15t3.59,12.62V65.56c0,4.28.77,7.24,2.29,8.87s4.3,2.44,8.32,2.44h2.74V83h-2.74q-6,0-8.32,2.49c-1.52,1.65-2.29,4.64-2.29,9v10.25q0,9.47-3.59,12.64T91.32,120.5H88.67Z'/%3e%3cpath%20id='path-2-2'%20data-name='path-2'%20class='cls-2'%20d='M88.67,114.33h2.91q6,0,7.87-1.89c1.22-1.25,1.83-3.9,1.83-7.93V93.89c0-4.46.65-7.7,1.93-9.73s3.51-3.43,6.67-4.2q-4.69-1.08-6.65-4.12c-1.3-2-2-5.28-2-9.77V55.44q0-6-1.83-7.93t-7.87-1.88H88.67V39.5h2.65q10.65,0,14.24,3.15t3.59,12.62V65.56c0,4.28.77,7.24,2.29,8.87s4.3,2.44,8.32,2.44h2.74V83h-2.74q-6,0-8.32,2.49c-1.52,1.65-2.29,4.64-2.29,9v10.25q0,9.47-3.59,12.64T91.32,120.5H88.67Z'/%3e%3c/g%3e%3cg%20id='text831'%3e%3cg%20id='_2'%20data-name='='%3e%3cpath%20id='path-3'%20class='cls-2'%20d='M38.5,85.15H75.83v7.58H38.5Zm0-17.88H75.83v7.49H38.5Z'/%3e%3cpath%20id='path-3-2'%20data-name='path-3'%20class='cls-2'%20d='M38.5,85.15H75.83v7.58H38.5Zm0-17.88H75.83v7.49H38.5Z'/%3e%3c/g%3e%3c/g%3e%3c/g%3e%3c/g%3e%3c/g%3e%3c/svg%3e";
function MainPage() {
  return /* @__PURE__ */ jsxs("main", { className: "container", children: [
    /* @__PURE__ */ jsx("img", { className: "logo", src: Logo, alt: "wasp" }),
    /* @__PURE__ */ jsx("h2", { className: "title", children: "Welcome to Wasp!" }),
    /* @__PURE__ */ jsxs("p", { className: "content", children: [
      "This is page ",
      /* @__PURE__ */ jsx("code", { children: "MainPage" }),
      " located at route ",
      /* @__PURE__ */ jsx("code", { children: "/" }),
      ".",
      /* @__PURE__ */ jsx("br", {}),
      "Open ",
      /* @__PURE__ */ jsx("code", { children: "src/MainPage.tsx" }),
      " to edit it."
    ] }),
    /* @__PURE__ */ jsxs("div", { className: "buttons", children: [
      /* @__PURE__ */ jsx(
        "a",
        {
          className: "button button-filled",
          href: "https://wasp.sh/docs/tutorial/create",
          target: "_blank",
          rel: "noreferrer noopener",
          children: "Take the Tutorial"
        }
      ),
      /* @__PURE__ */ jsx(
        "a",
        {
          className: "button button-outlined",
          href: "https://discord.com/invite/rzdnErX",
          target: "_blank",
          rel: "noreferrer noopener",
          children: "Chat on Discord"
        }
      )
    ] })
  ] });
}
export {
  MainPage
};
