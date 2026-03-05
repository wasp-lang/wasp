import { useAuth } from "wasp/client/auth";

import { Card, CardBody, CardHeader, Divider, Tab, Tabs } from "@heroui/react";
import { AskTheDocumentsForm } from "../components/MainPage/AskTheDocumentsForm";
import { DocumentsList } from "../components/MainPage/DocumentsList";
import { SearchForm } from "../components/MainPage/SearchForm";
import { SingleDocumentForm } from "../components/MainPage/SingleDocumentForm";
import { UrlTreeForm } from "../components/MainPage/UrlTreeForm";

export function Main() {
  const { data: user } = useAuth();

  if (!user) {
    return (
      <div className="min-h-screen pt-8">
        <div className="mx-auto max-w-4xl p-4">
          <AskTheDocumentsForm />
        </div>
      </div>
    );
  }

  return (
    <div className="min-h-screen pt-8">
      <div className="mx-auto max-w-4xl p-4">
        <Tabs aria-label="Options" variant="bordered" color="primary">
          <Tab key="ask" title="Ask">
            <AskTheDocumentsForm />
          </Tab>
          <Tab key="add" title="Add Document">
            <Card>
              <CardHeader>Let's embed with Wasp and pg-vector</CardHeader>
              <Divider />
              <CardBody>
                <Tabs aria-label="Adding Options">
                  <Tab key="text" title="URL Tree">
                    <UrlTreeForm />
                  </Tab>
                  <Tab key="url" title="Single URL">
                    <SingleDocumentForm />
                  </Tab>
                </Tabs>
              </CardBody>
            </Card>
            <DocumentsList />
          </Tab>
          <Tab key="search" title="Search">
            <SearchForm />
          </Tab>
        </Tabs>
      </div>
    </div>
  );
}
