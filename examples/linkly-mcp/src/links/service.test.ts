import { beforeEach, describe, expect, it } from "vitest";
import { DomainError } from "../shared/errors";
import {
  principalFromMcp,
  principalFromSession,
  type Principal,
} from "../shared/principal";
import { createTestUser, prisma, resetDb } from "../test/db";
import {
  createLink,
  deleteLink,
  getLink,
  listLinks,
  updateLink,
} from "./service";

const deps = { prisma };
let alice: Principal;
let bob: Principal;

beforeEach(async () => {
  await resetDb();
  alice = principalFromSession(await createTestUser());
  bob = principalFromSession(await createTestUser());
});

async function expectDomainError(
  promise: Promise<unknown>,
  code: DomainError["code"],
) {
  await expect(promise).rejects.toSatisfy(
    (e: unknown) => e instanceof DomainError && e.code === code,
  );
}

describe("createLink", () => {
  it("generates a 7-character slug when none is given", async () => {
    const link = await createLink(
      alice,
      { destinationUrl: "https://example.com" },
      deps,
    );
    expect(link.slug).toMatch(/^[a-z0-9]{7}$/);
    expect(link.shortUrl).toBe(`http://localhost:3001/r/${link.slug}`);
    expect(link.dashboardUrl).toBe(`http://localhost:3000/links/${link.id}`);
  });

  it("rejects a taken slug with conflict, even across users", async () => {
    await createLink(
      alice,
      { destinationUrl: "https://a.com", slug: "promo" },
      deps,
    );
    await expectDomainError(
      createLink(bob, { destinationUrl: "https://b.com", slug: "promo" }, deps),
      "conflict",
    );
  });

  it("validates input", async () => {
    await expectDomainError(
      createLink(alice, { destinationUrl: "ftp://nope" }, deps),
      "invalid",
    );
    await expectDomainError(
      createLink(alice, { destinationUrl: "https://a.com", slug: "UP" }, deps),
      "invalid",
    );
  });

  it("normalizes and dedupes tags", async () => {
    const link = await createLink(
      alice,
      { destinationUrl: "https://a.com", tags: ["Promo", "promo", "q3"] },
      deps,
    );
    expect(link.tags).toEqual(["promo", "q3"]);
  });

  it("requires links:write", async () => {
    const readOnly = principalFromMcp({
      user: { id: alice.userId },
      scopes: ["links:read"],
      connection: { clientName: "Claude" },
    });
    await expectDomainError(
      createLink(readOnly, { destinationUrl: "https://a.com" }, deps),
      "forbidden",
    );
  });
});

describe("getLink", () => {
  it("finds a link by ID or by slug", async () => {
    const link = await createLink(
      alice,
      { destinationUrl: "https://a.com", slug: "by-slug" },
      deps,
    );
    expect((await getLink(alice, { id: link.id }, deps)).id).toBe(link.id);
    expect((await getLink(alice, { id: "by-slug" }, deps)).id).toBe(link.id);
  });

  it("reports other users' links as not found", async () => {
    const link = await createLink(
      alice,
      { destinationUrl: "https://a.com", slug: "alices" },
      deps,
    );
    await expectDomainError(getLink(bob, { id: link.id }, deps), "not_found");
    await expectDomainError(getLink(bob, { id: "alices" }, deps), "not_found");
  });

  it("gives agents an actionable not-found message", async () => {
    await expect(getLink(alice, { id: "missing" }, deps)).rejects.toThrow(
      /Use list_links/,
    );
  });
});

describe("updateLink and deleteLink ownership", () => {
  it("won't let another user modify or delete a link", async () => {
    const link = await createLink(
      alice,
      { destinationUrl: "https://a.com" },
      deps,
    );
    await expectDomainError(
      updateLink(bob, { id: link.id, title: "hijacked" }, deps),
      "not_found",
    );
    await expectDomainError(
      deleteLink(bob, { id: link.id }, deps),
      "not_found",
    );
    expect((await getLink(alice, { id: link.id }, deps)).title).toBeNull();
  });

  it("updates fields and rejects a taken slug", async () => {
    const a = await createLink(
      alice,
      { destinationUrl: "https://a.com", slug: "first" },
      deps,
    );
    await createLink(
      alice,
      { destinationUrl: "https://b.com", slug: "second" },
      deps,
    );
    const updated = await updateLink(
      alice,
      { id: a.id, title: "Hello", tags: ["x"], archived: true },
      deps,
    );
    expect(updated).toMatchObject({
      title: "Hello",
      tags: ["x"],
      archived: true,
    });
    await expectDomainError(
      updateLink(alice, { id: a.id, slug: "second" }, deps),
      "conflict",
    );
  });

  it("deletes by slug", async () => {
    await createLink(
      alice,
      { destinationUrl: "https://a.com", slug: "gone" },
      deps,
    );
    expect(await deleteLink(alice, { id: "gone" }, deps)).toMatchObject({
      status: "deleted",
      slug: "gone",
    });
    await expectDomainError(getLink(alice, { id: "gone" }, deps), "not_found");
  });
});

describe("listLinks", () => {
  it("returns only the user's active links, newest first, with cursor pagination", async () => {
    for (let i = 0; i < 5; i++)
      await createLink(
        alice,
        { destinationUrl: `https://a.com/${i}`, slug: `link-${i}` },
        deps,
      );
    await createLink(bob, { destinationUrl: "https://bob.com" }, deps);

    const first = await listLinks(alice, { limit: 2 }, deps);
    expect(first.data.map((l) => l.slug)).toEqual(["link-4", "link-3"]);
    expect(first.nextCursor).not.toBeNull();

    const second = await listLinks(
      alice,
      { limit: 2, cursor: first.nextCursor! },
      deps,
    );
    expect(second.data.map((l) => l.slug)).toEqual(["link-2", "link-1"]);
    const third = await listLinks(
      alice,
      { limit: 2, cursor: second.nextCursor! },
      deps,
    );
    expect(third.data.map((l) => l.slug)).toEqual(["link-0"]);
    expect(third.nextCursor).toBeNull();
  });

  it("filters by search, tag, and archived", async () => {
    const a = await createLink(
      alice,
      {
        destinationUrl: "https://shop.example.com",
        title: "Summer Sale",
        tags: ["promo"],
      },
      deps,
    );
    await createLink(
      alice,
      { destinationUrl: "https://blog.example.com", title: "Blog" },
      deps,
    );
    const archived = await createLink(
      alice,
      { destinationUrl: "https://old.example.com", title: "Old sale" },
      deps,
    );
    await updateLink(alice, { id: archived.id, archived: true }, deps);

    expect(
      (await listLinks(alice, { search: "SALE" }, deps)).data.map((l) => l.id),
    ).toEqual([a.id]);
    expect(
      (await listLinks(alice, { tag: "promo" }, deps)).data.map((l) => l.id),
    ).toEqual([a.id]);
    expect(
      (await listLinks(alice, { archived: true }, deps)).data.map((l) => l.id),
    ).toEqual([archived.id]);
    expect(
      (await listLinks(alice, { archived: "true" as never }, deps)).data,
    ).toHaveLength(1);
  });

  it("rejects a malformed cursor", async () => {
    await expectDomainError(
      listLinks(alice, { cursor: "garbage" }, deps),
      "invalid",
    );
  });
});
