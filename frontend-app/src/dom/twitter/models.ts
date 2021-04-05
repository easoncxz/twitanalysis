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

/**
 * Shorthand to connect to the OAuth pass-thru endpoint on the Haskell side
 */
export function t(rel: string): string {
  return `/to-twitter/${rel}.json`;
}
