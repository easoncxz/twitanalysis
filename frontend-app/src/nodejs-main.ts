
export const srcPath = __dirname;
export const isMain = (): boolean => require.main === module;

import util from 'util';
import fetch from 'cross-fetch';

import * as OAuthClient from './oauth-client';
import * as OtherOAuthClient from './oauth-client-again';

function tryOAuthClient() {
  const args = OAuthClient.prep(
    {
      url: 'https://api.twitter.com/1.1/statuses/update.json',
      method: 'POST',
      data: {
        status: "Let's see if I can tweet using the new `fetch` API..." //eslint-disable-line quotes
      },
    }
  );
  console.log(args);
  fetch(...args)
    .then(
      (r: Response) => {
        console.log(r);
        return r.json();
      },
      (e) => {
        console.log(e);
      });
}
void tryOAuthClient;

function tryOtherOAuthClient() {
  const o = OtherOAuthClient.mkDefaultOAuth();
  //OtherOAuthClient.sendTweetWwwFormUrlencoded(
  OtherOAuthClient.sendTweetApplicationJson(
    o,
    "What about application/json? Does Twitter accept this Content-Type in POST request bodies?", // eslint-disable-line quotes
  ).then(
    resp => {
      console.log('Tweet result received');
      console.log(resp);
      console.log(util.inspect(JSON.parse(resp)));
    },
    err => {
      console.log('Request errored');
      console.log(err);
    }
  );
}
void tryOtherOAuthClient;

if (isMain()) {
  tryOAuthClient();
}
