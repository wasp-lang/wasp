export type Data = {
  entities: {
    name: string;
  }[];
  operations: {
    entities: {
      name: string;
    }[];
    name: string;
    type: "query" | "action";
    auth: boolean | null;
  }[];
  apis: {
    entities: {
      name: string;
    }[];
    httpRoute: {
      method: string;
      path: string;
    };
    name: string;
    auth: boolean | null;
  }[];
  jobs: {
    schedule: string | null;
    entities: {
      name: string;
    }[];
    name: string;
  }[];
  pages: {
    authRequired: boolean | null;
    name: string;
  }[];
  routes: {
    name: string;
    toPage: {
      name: string;
    };
    path: string;
  }[];
  app: {
    name: string;
    auth: {
      userEntity: {
        name: string;
      };
      methods: string[];
    } | null;
  };
};
