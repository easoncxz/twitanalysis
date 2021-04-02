/**
 * Returns any type, instead of returning `never`,
 * in order to not create "unreachable" code.
 */
export function typecheckNever<T>(n: never): T {
  return n;
}

export function pretty(o: unknown): string {
  return JSON.stringify(o, undefined, 4);
}

// https://github.com/microsoft/TypeScript/issues/4753#issuecomment-694557208
export function* stringEnumValues<T extends string>(enumObj: {
  [key: string]: T;
}): IterableIterator<T> {
  for (const property in enumObj) {
    yield enumObj[property];
  }
}
