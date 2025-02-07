export function sayHi() {
    console.log("This is coming from shared function.")
}


export type NonEmptyArray<T> = [T, ...T[]]
