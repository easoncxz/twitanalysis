const util = require('util');
const crypto = require('crypto');
const OAuth = require('oauth-1.0a');
const fetch = require('node-fetch');

const accessCred = require('../access-token.json');

/**
 * @type <T>(msg: string) => T
 */
const throwErr = (msg) => {
  throw new Error(msg);
};

const consumerKey = process.env.TWITTER_CONSUMER_KEY ?? throwErr('TWITTER_CONSUMER_KEY missing');
const consumerSec = process.env.TWITTER_CONSUMER_SECRET ?? throwErr('TWITTER_CONSUMER_SECRET missing');

const oauth = OAuth({
  consumer: {
    key: consumerKey,
    secret: consumerSec,
  },
  signature_method: 'HMAC-SHA1',
  hash_function(bs, key) {
    return crypto
      .createHmac('sha1', key)
      .update(bs)
      .digest('base64');
  },
});
module.exports.oauth = oauth;

function prepareRequest() {
  const url = 'https://api.twitter.com/1.1/statuses/home_timeline.json';
  const method = 'GET';
  const data = {
    //count: 200,
    //include_entities: true,
  };
  const request = { url, method, data: void data };
  const header = oauth.toHeader(
    oauth.authorize(request, accessCred)
  );
  return [url, {
    headers: {
      'Authorization': header['Authorization']
    }
  }];
}
module.exports.prepareRequest = prepareRequest;

function sendRequest(url, opts) {
  return new Promise((resolve, reject) => {
    fetch(url, opts).then(resp => {
      if (resp.ok) {
        resolve(resp.json());
      } else {
        reject(util.inspect(resp));
      }
    }, e => reject(e));
  });
}
module.exports.sendRequest = sendRequest;

function main() {
  const req = prepareRequest();
  sendRequest(...req).then(j => {
    console.log(util.inspect(j));
  }).catch(e => {
    console.log(util.inspect(e));
  });
}
module.exports.main = main;

// Run like this:
//    $ node src/oauth-client.js access-token access-token-secret
if (require && require instanceof Object && require.main === module) {
  main();
}
