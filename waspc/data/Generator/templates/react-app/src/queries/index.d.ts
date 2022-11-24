import { UseQueryResult } from "@tanstack/react-query";
import { OutputHTMLAttributes } from "react";
import { Query } from "./core";

export function useQuery<Input, Output>(
    queryFn: Query<Input, Output>,
    queryFnArgs?: Input, options?: any
): UseQueryResult<Output, any>
