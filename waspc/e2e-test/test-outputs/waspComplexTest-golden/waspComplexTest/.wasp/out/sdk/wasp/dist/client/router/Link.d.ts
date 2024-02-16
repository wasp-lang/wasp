/// <reference types="react" />
import { Link as RouterLink } from 'react-router-dom';
import { type Routes } from './index';
type RouterLinkProps = Parameters<typeof RouterLink>[0];
export declare function Link({ to, params, search, hash, ...restOfProps }: Omit<RouterLinkProps, "to"> & {
    search?: Record<string, string>;
    hash?: string;
} & Routes): import("react").JSX.Element;
export {};
