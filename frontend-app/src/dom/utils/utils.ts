/**
 * Returns any type, instead of returning `never`,
 * in order to not create "unreachable" code.
 */
export function typecheckNever<T>(n: never): T {
  return n;
}

export type MaybeDefined<T> = T | undefined;

export const mapMaybe = <A, B>(
  f: (_: A) => B,
  m: MaybeDefined<A>,
): MaybeDefined<B> => (m === undefined ? undefined : f(m));

export function pretty(o: unknown): string {
  return JSON.stringify(o, undefined, 4);
}

// https://github.com/microsoft/TypeScript/issues/4753#issuecomment-694557208
export function* stringEnumValues<T extends string>(enumObj: {
  [key: string]: T;
}): IterableIterator<T> {
  for (const property in enumObj) {
    yield enumObj[property] as T;
  }
}

function guardOk(r: Response): Response {
  if (!(200 <= r.status && r.status < 300)) {
    throw new Error(`Request not ok: ${r.status}`);
  } else {
    return r;
  }
}

/**
 * No parsing. That can be added as an optional param later.
 */
export async function fetchJson<T>(
  url: string,
  init?: RequestInit,
  parser?: (_: unknown) => Promise<T>,
): Promise<T> {
  return fetch(url, init)
    .then(guardOk)
    .then((r) => r.json())
    .then((j) => (parser ? parser(j) : j));
}

export function queryParams(kvs: [string, string][]): string {
  return kvs
    .reduce((params, [k, v]) => {
      params.append(k, v);
      return params;
    }, new URLSearchParams())
    .toString();
}
