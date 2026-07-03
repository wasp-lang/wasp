import { createElement, Fragment, type ReactElement } from "react";
import { describe, expect, test } from "vitest";
import { mapHeadElements } from "../../src/spec/headElements.js";
import { SpecUserError } from "../../src/spec/specUserError.js";

describe("mapHeadElements", () => {
  test("should print a self-closing element with string attributes", () => {
    expect(
      mapHeadElements([
        createElement("link", { rel: "icon", href: "/favicon.ico" }),
      ]),
    ).toStrictEqual(['<link rel={"icon"} href={"/favicon.ico"} />']);
  });

  test("should print one JSX string per head entry", () => {
    expect(
      mapHeadElements([
        createElement("link", { rel: "manifest", href: "/manifest.json" }),
        createElement("meta", { name: "author", content: "Wasp" }),
      ]),
    ).toStrictEqual([
      '<link rel={"manifest"} href={"/manifest.json"} />',
      '<meta name={"author"} content={"Wasp"} />',
    ]);
  });

  test("should print text children as string literal expressions", () => {
    expect(
      mapHeadElements([createElement("script", null, "console.log('hello')")]),
    ).toStrictEqual(["<script>{\"console.log('hello')\"}</script>"]);
  });

  test("should print boolean attributes like JSX does", () => {
    expect(
      mapHeadElements([
        createElement("script", {
          async: true,
          noModule: false,
          src: "/analytics.js",
        }),
      ]),
    ).toStrictEqual(['<script async src={"/analytics.js"} />']);
  });

  test("should skip null and undefined attributes", () => {
    expect(
      mapHeadElements([
        createElement("meta", {
          name: "description",
          content: "App",
          media: null,
          lang: undefined,
        }),
      ]),
    ).toStrictEqual(['<meta name={"description"} content={"App"} />']);
  });

  test("should print number attributes as expressions", () => {
    expect(
      mapHeadElements([createElement("meta", { content: 42 })]),
    ).toStrictEqual(["<meta content={42} />"]);
  });

  test("should print object attributes as expressions", () => {
    expect(
      mapHeadElements([
        createElement("style", {
          style: { color: "red", fontSize: 12 },
        }),
      ]),
    ).toStrictEqual(['<style style={{"color":"red","fontSize":12}} />']);
  });

  test("should print dangerouslySetInnerHTML", () => {
    expect(
      mapHeadElements([
        createElement("script", {
          dangerouslySetInnerHTML: { __html: "window.a = 1;" },
        }),
      ]),
    ).toStrictEqual([
      '<script dangerouslySetInnerHTML={{"__html":"window.a = 1;"}} />',
    ]);
  });

  test("should escape attribute values JSX would misinterpret", () => {
    expect(
      mapHeadElements([
        createElement("meta", { content: 'quote " and entity &amp;' }),
      ]),
    ).toStrictEqual(['<meta content={"quote \\" and entity &amp;"} />']);
  });

  test("should escape text children JSX would misinterpret", () => {
    expect(
      mapHeadElements([createElement("title", null, "Tom & Jerry")]),
    ).toStrictEqual(['<title>{"Tom & Jerry"}</title>']);
  });

  test("should print nested elements", () => {
    expect(
      mapHeadElements([
        createElement(
          "noscript",
          null,
          createElement("link", { rel: "stylesheet", href: "/no-js.css" }),
        ),
      ]),
    ).toStrictEqual([
      '<noscript><link rel={"stylesheet"} href={"/no-js.css"} /></noscript>',
    ]);
  });

  test("should print multiple children, skipping the ones React would not render", () => {
    expect(
      mapHeadElements([
        createElement(
          "title",
          null,
          "My App",
          null,
          false,
          " - ",
          createElement("span", null, "v2"),
        ),
      ]),
    ).toStrictEqual(['<title>{"My App"}{" - "}<span>{"v2"}</span></title>']);
  });

  test("should flatten fragments", () => {
    expect(
      mapHeadElements([
        createElement(
          Fragment,
          null,
          createElement("meta", { name: "a", content: "1" }),
          createElement("meta", { name: "b", content: "2" }),
        ),
      ]),
    ).toStrictEqual([
      '<meta name={"a"} content={"1"} /><meta name={"b"} content={"2"} />',
    ]);
  });

  test("should reject components", () => {
    function MyHead(): null {
      return null;
    }
    expect(() => mapHeadElements([createElement(MyHead)])).toThrowError(
      expect.objectContaining({
        constructor: SpecUserError,
        message: expect.stringContaining("only plain HTML elements"),
      }),
    );
  });

  test("should reject nested components", () => {
    function MyMeta(): null {
      return null;
    }
    expect(() =>
      mapHeadElements([createElement("noscript", null, createElement(MyMeta))]),
    ).toThrowError(SpecUserError);
  });

  test("should reject function attributes", () => {
    expect(() =>
      mapHeadElements([createElement("script", { onLoad: () => undefined })]),
    ).toThrowError(
      expect.objectContaining({
        constructor: SpecUserError,
        message: expect.stringContaining("onLoad"),
      }),
    );
  });

  test("should reject non-serializable object attributes", () => {
    expect(() =>
      mapHeadElements([
        createElement("script", {
          dangerouslySetInnerHTML: { __html: () => undefined },
        }),
      ]),
    ).toThrowError(SpecUserError);
  });

  test("should reject entries that are not elements at all", () => {
    expect(() =>
      mapHeadElements([
        '<link rel="icon" href="/favicon.ico" />' as unknown as ReactElement,
      ]),
    ).toThrowError(
      expect.objectContaining({
        constructor: SpecUserError,
        message: expect.stringContaining('the string "<link'),
      }),
    );
  });

  test("should reject non-finite number attributes and children", () => {
    expect(() =>
      mapHeadElements([createElement("meta", { content: NaN })]),
    ).toThrowError(SpecUserError);
    expect(() =>
      mapHeadElements([createElement("title", null, Infinity)]),
    ).toThrowError(SpecUserError);
  });

  test("should reject circular object attributes instead of overflowing the stack", () => {
    const circular: { self?: unknown } = {};
    circular.self = circular;
    expect(() =>
      mapHeadElements([createElement("meta", { content: circular })]),
    ).toThrowError(
      expect.objectContaining({
        constructor: SpecUserError,
        message: expect.stringContaining("circular"),
      }),
    );
  });

  test("should allow repeated (non-circular) references to the same object", () => {
    const shared = { a: 1 };
    expect(
      mapHeadElements([
        createElement("meta", { content: { x: shared, y: shared } }),
      ]),
    ).toStrictEqual(['<meta content={{"x":{"a":1},"y":{"a":1}}} />']);
  });

  test("should reject invalid tag names", () => {
    expect(() =>
      mapHeadElements([
        { type: "script>", props: {} } as unknown as ReactElement,
      ]),
    ).toThrowError(
      expect.objectContaining({
        constructor: SpecUserError,
        message: expect.stringContaining("not a valid HTML tag name"),
      }),
    );
  });
});
