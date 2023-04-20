type RailwayService = {
    name: string;
    url: string;
};

export type RailwayDeploymentConfig = {
	environment: string;
	projectId: string;
    clientService: RailwayService;
    serverService: RailwayService;
};
