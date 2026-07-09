export type TodoItem = {
  id: number;
  description: string;
  isDone: boolean;
};

export type TodoItems = {
  items: TodoItem[];
  totalCount: number;
};

export type HostContext = {
  user?: { id: number };
  entities: {
    Task: any;
  };
};
