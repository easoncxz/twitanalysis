export type User = {
  id_str: string;
  name: string;
  screen_name: string;
};

async function parseObject(j: unknown): Promise<object> {
  if (typeof j !== 'object') {
    throw new Error(`User JSON needs to be an object: ${j}`);
  }
  if (j === null) {
    throw new Error(`User JSON should not be null`);
  }
  return j;
}

export async function parseUser(j: unknown): Promise<User> {
  const o = await parseObject(j);
  for (const k of ['id_str', 'name', 'screen_name']) {
    if (!(k in o)) {
      throw new Error(`Key ${k} missing from User object: ${o}`);
    }
  }
  return o as User;
}

export type Status = {
  id_str: string;
  text: string;
  created_at: string;
};

export async function parseStatus(j: unknown): Promise<Status> {
  const o = await parseObject(j);
  for (const k of ['id_str', 'text', 'created_at']) {
    if (!(k in o)) {
      throw new Error(`Key ${k} missing from Status object: ${o}`);
    }
  }
  return o as Status;
}

export const parseArray = <T>(elem: (_: unknown) => Promise<T>) => async (
  j: unknown,
): Promise<T[]> => {
  if (!(j instanceof Array)) {
    throw new Error(`Expecting an array, but got: ${j}`);
  }
  const out = [];
  for (const ej of j) {
    const e = await elem(ej);
    out.push(e);
  }
  return out;
};

/**
 * See docs for GET lists/list:
 *
 *   - https://developer.twitter.com/en/docs/twitter-api/v1/accounts-and-users/create-manage-lists/api-reference/get-lists-list
 */
export type List = {
  slug: string;
  name: string;
  created_at: string; // "Sat Feb 27 21:39:24 +0000 2010"
  uri: string; // "/twitterapi/meetup-20100301"
  subscriber_count: number;
  member_count: number;
  id: number;
  id_str: string;
  mode: 'public';
  full_name: string; // "@twitterapi/meetup-20100301"
  description: string; // long description
  user: User;
  following: boolean;
};

export async function parseList(j: unknown): Promise<List> {
  const o = await parseObject(j);
  for (const k of ['slug', 'subscriber_count', 'member_count', 'description']) {
    if (!(k in o)) {
      throw new Error(`Key ${k} missing from List object: ${o}`);
    }
  }
  return o as List;
}

export type ListMembersResponse = {
  next_cursor: number; // 0
  next_cursor_str: string; // "0"
  previous_cursor: number; // 0
  previous_cursor_str: string; // "0"
  //total_count: null
  users: User[];
};

export async function parseListMembersResponse(
  x: unknown,
): Promise<ListMembersResponse> {
  // TypeScript's type-narrowing sucks on object key access.
  // We're writing a parser here anyway, so some level of unsafeness
  // can be tolerated.
  //
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  const j = x as any;
  if (!(j instanceof Object)) {
    throw new TypeError(
      `ListMembersResponse should at least be an object: ${j}`,
    );
  } else if (
    !('next_cursor_str' in j && typeof j.next_cursor_str === 'string')
  ) {
    throw new TypeError(`ListMembersResponse missing field: next_cursor_str`);
  } else if (
    !('previous_cursor_str' in j && typeof j.previous_cursor_str === 'string')
  ) {
    throw new TypeError(
      `ListMembersResponse missing field: previous_cursor_str`,
    );
  } else if (!('users' in j)) {
    throw new TypeError(`ListMembersResponse missing field: users`);
  } else if (!(j.users instanceof Array)) {
    throw new TypeError(
      `ListMembersResponse.users should be an Array: ${j.users}`,
    );
  } else {
    const r = j as ListMembersResponse;
    // Force the parser on the nested field.
    await Promise.all(r.users.map(parseUser));
    return r;
  }
}

/**
 * Shorthand to connect to the OAuth pass-thru endpoint on the Haskell side
 */
export function t(rel: string): string {
  return `/to-twitter/${rel}.json`;
}

export type ListMembership = {
  listIdStr: string;
  userIdStr: string;
};
