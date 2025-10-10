import { Button, Input, Listbox, ListboxItem } from "@heroui/react";
import { useEffect, useState } from "react";
import { useForm } from "react-hook-form";
import { embedDocument, getScrapeCandidates } from "wasp/client/operations";

import { ListboxWrapper } from "../ListBoxWrapper";

export function UrlTreeForm() {
  const form = useForm<{
    url: string;
    selector?: string;
  }>();
  const [results, setResults] = useState<Awaited<
    ReturnType<typeof getScrapeCandidates>
  > | null>(null);
  const onSubmit = form.handleSubmit(async (data) => {
    const response = await getScrapeCandidates(data);
    setResults(response);
  });

  const [selectedKeys, setSelectedKeys] = useState<Set<string>>(new Set([]));
  const [scrapedKeys, setScrapedKeys] = useState<Set<string>>(new Set([]));
  const [isScraping, setIsScraping] = useState(false);

  const selector = form.watch("selector");

  useEffect(() => {
    if (results) {
      setSelectedKeys(new Set(results.links));
    }
  }, [results]);

  async function startScraping() {
    setIsScraping(true);
    const selectedUrls = Array.from(selectedKeys.values());
    for (const url of selectedUrls) {
      try {
        await embedDocument({
          url,
          selector,
        });
        setScrapedKeys((keys) => new Set(keys.add(url)));
      } catch (e) {
        // Ignore for now
      }
    }
    setIsScraping(false);
  }

  return (
    <form onSubmit={onSubmit}>
      <div className="mb-2">
        <Input
          {...form.register("url", {
            required: "Website URL is required",
            validate: (value) => {
              try {
                new URL(value);
                return true;
              } catch (e) {
                return "Website URL is invalid";
              }
            },
          })}
          label="Website URL"
          variant="bordered"
          errorMessage={form.formState.errors.url?.message}
          isDisabled={!!results}
        />
      </div>
      <Button type="submit" color="primary" isDisabled={!!results}>
        Get Candidate URLs
      </Button>

      {results && (
        <div>
          <div className="flex items-center justify-between">
            <h2 className="mt-4 text-2xl font-bold">Candidate URLs</h2>
            <Button
              onClick={() => {
                setResults(null);
                setSelectedKeys(new Set([]));
                setScrapedKeys(new Set([]));
              }}
            >
              Clear
            </Button>
          </div>
          <div className="mt-2">
            <ListboxWrapper>
              <Listbox
                aria-label="Multiple selection example"
                variant="flat"
                disallowEmptySelection
                selectionMode="multiple"
                selectedKeys={selectedKeys}
                onSelectionChange={(selectedKeys) => {
                  if (isScraping) {
                    return;
                  }
                  // @ts-ignore
                  setSelectedKeys(selectedKeys);
                }}
              >
                {results.links.map((link) => (
                  <ListboxItem
                    key={link}
                    startContent={
                      <span>{scrapedKeys.has(link) ? "✅" : "🔗"}</span>
                    }
                  >
                    {link}
                  </ListboxItem>
                ))}
              </Listbox>
            </ListboxWrapper>
          </div>
          <div className="mt-2">
            <Input
              {...form.register("selector")}
              description="Where to pull the content from? (it's `body` by default)"
              label="Content CSS Selector"
              variant="bordered"
              isDisabled={isScraping}
            />
          </div>
          <div className="mt-2">
            <Button
              onClick={startScraping}
              color="primary"
              isDisabled={isScraping}
            >
              Scrape Selected URLs
            </Button>
          </div>
        </div>
      )}
    </form>
  );
}
