
import fs from 'fs';
import path from 'path';
import fetch from 'cross-fetch';

import { mkSign } from '../oauth-client';

const srcPath = __dirname;

function throwErr<T>(msg?: string): T {
  throw new Error(msg);
}

export function envConsumer(): OAuth.Consumer {
  return {
    key: process.env['TWITTER_CONSUMER_KEY'] ?? throwErr('TWITTER_CONSUMER_KEY missing'),
    secret: process.env['TWITTER_CONSUMER_SECRET'] ?? throwErr('TWITTER_CONSUMER_SECRET missing'),
  };
}

export function fileAccessToken(): OAuth.Token {
  return JSON.parse(
    fs.readFileSync(
      path.join(srcPath, '../access-token.json')
    ).toString('utf-8'));
}


export const defaultSign = mkSign(envConsumer())(fileAccessToken());

export function formatFetchArgs(): [RequestInfo, RequestInit] {
  const [url, requestInit] = defaultSign(
    {
      url: 'https://api.twitter.com/1.1/statuses/update.json',
      method: 'POST',
      data: {
        status: "Testing using URLSearchParams with fetch..." //eslint-disable-line quotes
      },
    }
  );
  console.log(url, requestInit);
  return [url, requestInit];
}

export function tryOAuthClient(): Promise<JSON> {
  const args = formatFetchArgs();
  return fetch(...args)
    .then(
      (r: Response) => {
        console.log(r);
        const j = r.json();
        console.log(j);
        return j;
      },
      (e) => {
        console.log(e);
      });
}
