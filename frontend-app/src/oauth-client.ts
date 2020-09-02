import util from 'util';
import fs from 'fs';
import path from 'path';

import crypto from 'crypto';
import OAuth from 'oauth-1.0a';
import fetch from 'cross-fetch';

import { srcPath } from './nodejs-env';

void util, fetch;

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

export function newOAuth(consumer: OAuth.Consumer): OAuth {
  return new OAuth({
    consumer,
    signature_method: 'HMAC-SHA1',
    hash_function(bs, key) {
      return crypto
        .createHmac('sha1', key)
        .update(bs)
        .digest('base64');
    },
  });
}

export const envOAuth = (): OAuth => newOAuth(envConsumer());

function isProbablyHeaders(h: HeadersInit): h is Headers {
  if (!(h && h instanceof Object)) {
    return false;
  } else {
    for (const k of ['append', 'delete', 'has', 'forEach']) {
      if (!(k in h)) {
        return false;
      }
    }
    return true;
  }
}

export function headersToPairs(h: HeadersInit): [string, string][] {
  const result = [];
  if (h) {
    if (isProbablyHeaders(h)) {
      for (const [k, v] of h.entries()) {
        result.push([k, v] as [string, string]);
      }
    } else if (h instanceof Array) {
      for (const [k, v] of h) {
        result.push([k, v] as [string, string]);
      }
    } else if (h instanceof Object) {
      for (const k in h) {
        result.push([k, h[k]] as [string, string]);
      }
    }
  }
  return result;
}

/**
 * Prepare arguments for a call to `fetch`
 */
export const mkPrepFetch = (o: OAuth) => (a: OAuth.Token) => (opts: OAuth.RequestOptions): [RequestInfo, RequestInit | undefined] => {
  const authorisation = o.toHeader(o.authorize(opts, a)).Authorization;
  const params = new URLSearchParams();
  if (opts.data instanceof Object) {
    for (const k of Object.keys(opts.data).sort()) {
      params.append(k, opts.data[k]);
    }
  }
  const signedInit: RequestInit = {
    headers: {
      //'Content-Type': 'application/json',
      //'Content-Type': 'application/x-www-form-urlencoded',
      'Authorization': authorisation,
    },
    method: opts.method,
    body: params,
  };
  return [opts.url, signedInit];
};

export const prep = mkPrepFetch(envOAuth())(fileAccessToken());

export function formatCurlCommand(req: RequestInfo, init?: RequestInit): string {
  const headerKvs: [string, string][] = headersToPairs(init?.headers ?? []);
  return [
    'curl -i',
  ]
    .concat(headerKvs.map(([k, v]) =>
      '-H ' + util.inspect( `${k}: ${v}`))
    ).concat([`-X ${init?.method ?? 'GET'}`])
    .concat([`-d ${util.inspect(init?.body?.toString())}`])
    .concat([util.inspect(req.toString())])
    .join(' \\\n    ');
}
