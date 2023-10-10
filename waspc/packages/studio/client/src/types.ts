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
    auth: string;
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
    auth: string;
  }[];
  jobs: {
    schedule: string;
    entities: {
      name: string;
    }[];
    name: string;
  }[];
  pages: {
    authRequired: string;
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
    };
    db: {
      system: string;
    };
  };
};
