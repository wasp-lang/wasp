export declare const styled: <Type extends keyof JSX.IntrinsicElements | React.ComponentType<any> | import("@stitches/react/types/util").Function, Composers extends (string | React.ComponentType<any> | import("@stitches/react/types/util").Function | {
    [name: string]: unknown;
})[], CSS = import("@stitches/react/types/css-util").CSS<{}, {
    colors: {
        waspYellow: "#ffcc00";
        gray900: "#1a202c";
        gray700: "#a1a5ab";
        gray600: "#d1d5db";
        gray500: "gainsboro";
        gray400: "#f0f0f0";
        red: "#FED7D7";
        darkRed: "#fa3838";
        green: "#C6F6D5";
        brand: "$waspYellow";
        brandAccent: "#ffdb46";
        errorBackground: "$red";
        errorText: "#2D3748";
        successBackground: "$green";
        successText: "#2D3748";
        submitButtonText: "black";
        formErrorText: "$darkRed";
    };
    fontSizes: {
        sm: "0.875rem";
    };
}, import("@stitches/react/types/config").DefaultThemeMap, {}>>(type: Type, ...composers: { [K in keyof Composers]: string extends Composers[K] ? Composers[K] : Composers[K] extends string | import("@stitches/react/types/util").Function | import("react").ComponentType<any> ? Composers[K] : import("@stitches/react/types/stitches").RemoveIndex<CSS> & {
    variants?: {
        [x: string]: {
            [x: string]: CSS;
            [x: number]: CSS;
        };
    } | undefined;
    compoundVariants?: (("variants" extends keyof Composers[K] ? { [Name in keyof Composers[K][keyof Composers[K] & "variants"]]?: import("@stitches/react/types/util").String | import("@stitches/react/types/util").Widen<keyof Composers[K][keyof Composers[K] & "variants"][Name]> | undefined; } : import("@stitches/react/types/util").WideObject) & {
        css: CSS;
    })[] | undefined;
    defaultVariants?: ("variants" extends keyof Composers[K] ? { [Name_1 in keyof Composers[K][keyof Composers[K] & "variants"]]?: import("@stitches/react/types/util").String | import("@stitches/react/types/util").Widen<keyof Composers[K][keyof Composers[K] & "variants"][Name_1]> | undefined; } : import("@stitches/react/types/util").WideObject) | undefined;
} & CSS & { [K2 in keyof Composers[K]]: K2 extends "compoundVariants" | "defaultVariants" | "variants" ? unknown : K2 extends keyof CSS ? CSS[K2] : unknown; }; }) => import("@stitches/react/types/styled-component").StyledComponent<Type, import("@stitches/react/types/styled-component").StyledComponentProps<Composers>, {}, import("@stitches/react/types/css-util").CSS<{}, {
    colors: {
        waspYellow: "#ffcc00";
        gray900: "#1a202c";
        gray700: "#a1a5ab";
        gray600: "#d1d5db";
        gray500: "gainsboro";
        gray400: "#f0f0f0";
        red: "#FED7D7";
        darkRed: "#fa3838";
        green: "#C6F6D5";
        brand: "$waspYellow";
        brandAccent: "#ffdb46";
        errorBackground: "$red";
        errorText: "#2D3748";
        successBackground: "$green";
        successText: "#2D3748";
        submitButtonText: "black";
        formErrorText: "$darkRed";
    };
    fontSizes: {
        sm: "0.875rem";
    };
}, import("@stitches/react/types/config").DefaultThemeMap, {}>>, css: <Composers extends (string | React.ExoticComponent<any> | React.JSXElementConstructor<any> | import("@stitches/react/types/util").Function | {
    [name: string]: unknown;
})[], CSS = import("@stitches/react/types/css-util").CSS<{}, {
    colors: {
        waspYellow: "#ffcc00";
        gray900: "#1a202c";
        gray700: "#a1a5ab";
        gray600: "#d1d5db";
        gray500: "gainsboro";
        gray400: "#f0f0f0";
        red: "#FED7D7";
        darkRed: "#fa3838";
        green: "#C6F6D5";
        brand: "$waspYellow";
        brandAccent: "#ffdb46";
        errorBackground: "$red";
        errorText: "#2D3748";
        successBackground: "$green";
        successText: "#2D3748";
        submitButtonText: "black";
        formErrorText: "$darkRed";
    };
    fontSizes: {
        sm: "0.875rem";
    };
}, import("@stitches/react/types/config").DefaultThemeMap, {}>>(...composers: { [K in keyof Composers]: string extends Composers[K] ? Composers[K] : Composers[K] extends string | import("@stitches/react/types/util").Function | import("react").ExoticComponent<any> | import("react").JSXElementConstructor<any> ? Composers[K] : import("@stitches/react/types/stitches").RemoveIndex<CSS> & {
    variants?: {
        [x: string]: {
            [x: string]: CSS;
            [x: number]: CSS;
        };
    } | undefined;
    compoundVariants?: (("variants" extends keyof Composers[K] ? { [Name in keyof Composers[K][keyof Composers[K] & "variants"]]?: import("@stitches/react/types/util").String | import("@stitches/react/types/util").Widen<keyof Composers[K][keyof Composers[K] & "variants"][Name]> | undefined; } : import("@stitches/react/types/util").WideObject) & {
        css: CSS;
    })[] | undefined;
    defaultVariants?: ("variants" extends keyof Composers[K] ? { [Name_1 in keyof Composers[K][keyof Composers[K] & "variants"]]?: import("@stitches/react/types/util").String | import("@stitches/react/types/util").Widen<keyof Composers[K][keyof Composers[K] & "variants"][Name_1]> | undefined; } : import("@stitches/react/types/util").WideObject) | undefined;
} & CSS & { [K2 in keyof Composers[K]]: K2 extends "compoundVariants" | "defaultVariants" | "variants" ? unknown : K2 extends keyof CSS ? CSS[K2] : unknown; }; }) => import("@stitches/react/types/styled-component").CssComponent<import("@stitches/react/types/styled-component").StyledComponentType<Composers>, import("@stitches/react/types/styled-component").StyledComponentProps<Composers>, {}, CSS>, keyframes: (style: {
    [offset: string]: import("@stitches/react/types/css-util").CSS<{}, {
        colors: {
            waspYellow: "#ffcc00";
            gray900: "#1a202c";
            gray700: "#a1a5ab";
            gray600: "#d1d5db";
            gray500: "gainsboro";
            gray400: "#f0f0f0";
            red: "#FED7D7";
            darkRed: "#fa3838";
            green: "#C6F6D5";
            brand: "$waspYellow";
            brandAccent: "#ffdb46";
            errorBackground: "$red";
            errorText: "#2D3748";
            successBackground: "$green";
            successText: "#2D3748";
            submitButtonText: "black";
            formErrorText: "$darkRed";
        };
        fontSizes: {
            sm: "0.875rem";
        };
    }, import("@stitches/react/types/config").DefaultThemeMap, {}>;
}) => {
    (): string;
    name: string;
};
//# sourceMappingURL=stitches.config.d.ts.map