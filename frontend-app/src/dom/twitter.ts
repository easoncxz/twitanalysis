export type User = {
  id_str: string;
  name: string;
  screen_name: string;
};

export type Status = {
  id_str: string;
  text: string;
  created_at: string;
};

/**
 * Shorthand to connect to the OAuth pass-thru endpoint on the Haskell side
 */
export function t(rel: string): string {
  return '/to-twitter/' + rel + '.json';
}
