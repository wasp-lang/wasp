/// <reference types="react" />
export declare const styled: <Type extends import("@stitches/react/types/util").Function | keyof JSX.IntrinsicElements | import("react").ComponentType<any>, Composers extends (string | import("@stitches/react/types/util").Function | import("react").ComponentType<any> | {
    [name: string]: unknown;
})[], CSS = import("@stitches/react/types/css-util").CSS<{}, {
    colors: {
        waspYellow: string;
        gray900: string;
        gray700: string;
        gray600: string;
        gray500: string;
        gray400: string;
        red: string;
        darkRed: string;
        green: string;
        brand: string;
        brandAccent: string;
        errorBackground: string;
        errorText: string;
        successBackground: string;
        successText: string;
        submitButtonText: string;
        formErrorText: string;
    };
    fontSizes: {
        sm: string;
    };
}, import("@stitches/react/types/config").DefaultThemeMap, {}>>(type: Type, ...composers: { [K in keyof Composers]: string extends Composers[K] ? Composers[K] : Composers[K] extends string | import("@stitches/react/types/util").Function | import("react").ComponentType<any> ? Composers[K] : import("@stitches/react/types/stitches").RemoveIndex<CSS> & {
    variants?: {
        [x: string]: {
            [x: string]: CSS;
            [x: number]: CSS;
        };
    };
    compoundVariants?: (("variants" extends keyof Composers[K] ? { [Name in keyof Composers[K][keyof Composers[K] & "variants"]]?: import("@stitches/react/types/util").String | import("@stitches/react/types/util").Widen<keyof Composers[K][keyof Composers[K] & "variants"][Name]>; } : import("@stitches/react/types/util").WideObject) & {
        css: CSS;
    })[];
    defaultVariants?: "variants" extends keyof Composers[K] ? { [Name_1 in keyof Composers[K][keyof Composers[K] & "variants"]]?: import("@stitches/react/types/util").String | import("@stitches/react/types/util").Widen<keyof Composers[K][keyof Composers[K] & "variants"][Name_1]>; } : import("@stitches/react/types/util").WideObject;
} & CSS & { [K2 in keyof Composers[K]]: K2 extends "compoundVariants" | "defaultVariants" | "variants" ? unknown : K2 extends keyof CSS ? CSS[K2] : unknown; }; }) => import("@stitches/react/types/styled-component").StyledComponent<Type, import("@stitches/react/types/styled-component").StyledComponentProps<Composers>, {}, import("@stitches/react/types/css-util").CSS<{}, {
    colors: {
        waspYellow: string;
        gray900: string;
        gray700: string;
        gray600: string;
        gray500: string;
        gray400: string;
        red: string;
        darkRed: string;
        green: string;
        brand: string;
        brandAccent: string;
        errorBackground: string;
        errorText: string;
        successBackground: string;
        successText: string;
        submitButtonText: string;
        formErrorText: string;
    };
    fontSizes: {
        sm: string;
    };
}, import("@stitches/react/types/config").DefaultThemeMap, {}>>, css: <Composers extends (string | import("@stitches/react/types/util").Function | import("react").ExoticComponent<any> | import("react").JSXElementConstructor<any> | {
    [name: string]: unknown;
})[], CSS = import("@stitches/react/types/css-util").CSS<{}, {
    colors: {
        waspYellow: string;
        gray900: string;
        gray700: string;
        gray600: string;
        gray500: string;
        gray400: string;
        red: string;
        darkRed: string;
        green: string;
        brand: string;
        brandAccent: string;
        errorBackground: string;
        errorText: string;
        successBackground: string;
        successText: string;
        submitButtonText: string;
        formErrorText: string;
    };
    fontSizes: {
        sm: string;
    };
}, import("@stitches/react/types/config").DefaultThemeMap, {}>>(...composers: { [K in keyof Composers]: string extends Composers[K] ? Composers[K] : Composers[K] extends string | import("@stitches/react/types/util").Function | import("react").ExoticComponent<any> | import("react").JSXElementConstructor<any> ? Composers[K] : import("@stitches/react/types/stitches").RemoveIndex<CSS> & {
    variants?: {
        [x: string]: {
            [x: string]: CSS;
            [x: number]: CSS;
        };
    };
    compoundVariants?: (("variants" extends keyof Composers[K] ? { [Name in keyof Composers[K][keyof Composers[K] & "variants"]]?: import("@stitches/react/types/util").String | import("@stitches/react/types/util").Widen<keyof Composers[K][keyof Composers[K] & "variants"][Name]>; } : import("@stitches/react/types/util").WideObject) & {
        css: CSS;
    })[];
    defaultVariants?: "variants" extends keyof Composers[K] ? { [Name_1 in keyof Composers[K][keyof Composers[K] & "variants"]]?: import("@stitches/react/types/util").String | import("@stitches/react/types/util").Widen<keyof Composers[K][keyof Composers[K] & "variants"][Name_1]>; } : import("@stitches/react/types/util").WideObject;
} & CSS & { [K2 in keyof Composers[K]]: K2 extends "compoundVariants" | "defaultVariants" | "variants" ? unknown : K2 extends keyof CSS ? CSS[K2] : unknown; }; }) => import("@stitches/react/types/styled-component").CssComponent<import("@stitches/react/types/styled-component").StyledComponentType<Composers>, import("@stitches/react/types/styled-component").StyledComponentProps<Composers>, {}, CSS>, keyframes: (style: {
    [offset: string]: import("@stitches/react/types/css-util").CSS<{}, {
        colors: {
            waspYellow: string;
            gray900: string;
            gray700: string;
            gray600: string;
            gray500: string;
            gray400: string;
            red: string;
            darkRed: string;
            green: string;
            brand: string;
            brandAccent: string;
            errorBackground: string;
            errorText: string;
            successBackground: string;
            successText: string;
            submitButtonText: string;
            formErrorText: string;
        };
        fontSizes: {
            sm: string;
        };
    }, import("@stitches/react/types/config").DefaultThemeMap, {}>;
}) => {
    (): string;
    name: string;
};
