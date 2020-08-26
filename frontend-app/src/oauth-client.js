const util = require('util');
const crypto = require('crypto');
const OAuth = require('oauth-1.0a');

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

function main() {
  const [_node, _script, accessToken, accessSecret] = process.argv.slice();
  const token = {
    key: accessToken,
    secret: accessSecret,
  };
  const request = {
    url: 'https://google.com/',
    method: 'GET',
    data: {
      q: 'foobar'
    }
  };
  const header = oauth.toHeader(
    oauth.authorize(request, token)
  );
  console.log(util.inspect(
    header,
    {
      colors: true,
    }
  ));
}

module.exports.oauth = oauth;
module.exports.main = main;

// Run like this:
//    $ node src/oauth-client.js access-token access-token-secret
main();
