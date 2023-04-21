import { CommonOptions } from '../../shared/CommonOptions';
import { RailwayDeploymentConfig } from '../types';

export type RwDeploymentInfo = {
    commonOptions: CommonOptions;
};

type RwClientDeploymentInfo = RwDeploymentInfo & {
    clientService: RailwayDeploymentConfig['clientService'];
};

type RwServerDeploymentInfo = RwDeploymentInfo & {
    serverService: RailwayDeploymentConfig['serverService'];
};
