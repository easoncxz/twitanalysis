
import http from 'http';
import OAuth from 'oauth';

import { envConsumer, fileAccessToken } from './oauth-client';

export function mkDefaultOAuth(): OAuth.OAuth {
  const consumer = envConsumer();
  return new OAuth.OAuth(
    'https://api.twitter.com/oauth/request_token',
    'https://api.twitter.com/oauth/access_token',
    consumer.key,
    consumer.secret,
    '1.0A',
    null,
    'HMAC-SHA1',
  );
}

const receiveRespose =
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
  (resolve: any, reject: any) =>
  // eslint-disable-next-line @typescript-eslint/no-explicit-any
    (e: { statusCode: number; data?: any; }, data: string | Buffer | undefined, res?: http.IncomingMessage) => {
      if (e) {
        reject(e);
      }
      console.log('IncomingMessage:', res);
      if (Buffer.isBuffer(data)) {
        resolve(data.toString('utf-8'));
      } else if (data) {
        resolve(data);
      } else {
        resolve('');
      }
    };

export async function sendTweetWwwFormUrlencoded(oauth: OAuth.OAuth, status: string): Promise<string> {
  const token = fileAccessToken();
  return new Promise((resolve, reject) => {
    oauth.post(
      'https://api.twitter.com/1.1/statuses/update.json',
      token.key,
      token.secret,
      { status },
      'application/x-www-form-urlencoded',
      receiveRespose(resolve, reject),
    );
  });
}

export async function sendTweetApplicationJson(oauth: OAuth.OAuth, status: string): Promise<string> {
  const token = fileAccessToken();
  return new Promise((resolve, reject) => {
    oauth.post(
      'https://api.twitter.com/1.1/statuses/update.json',
      token.key,
      token.secret,
      { status: JSON.stringify(status) },
      'application/json',
      receiveRespose(resolve, reject),
    );
  });
}
