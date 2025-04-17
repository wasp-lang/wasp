import { ReactElement } from 'react';
import { type SetupServer } from 'msw/node';
import { RenderResult } from '@testing-library/react';
import { Query } from 'wasp/client/operations/rpc';
import { Route } from 'wasp/client';
export type MockQuery = <Input, Output, MockOutput extends Output>(query: Query<Input, Output>, resJson: MockOutput) => void;
export type MockApi = (route: Route, resJson: unknown) => void;
export declare function renderInContext(ui: ReactElement): RenderResult;
export declare function mockServer(): {
    server: SetupServer;
    mockQuery: MockQuery;
    mockApi: MockApi;
};
//# sourceMappingURL=helpers.d.ts.map