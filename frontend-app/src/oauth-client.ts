
import crypto from 'crypto';
import OAuth from 'oauth-1.0a';

function newOAuth(consumer: OAuth.Consumer): OAuth {
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

/**
 * Prepare arguments for a call to `fetch`
 *
 *    const sign = mkSign(consumerCred)(accessCred);
 *    const [url, requestInit] = sign({
 *      url: '
 *
 * Not pure; involves random-number generation as part of OAuth 1.0a signing process.
 */
export const mkSign = (c: OAuth.Consumer) => (a: OAuth.Token) => (opts: OAuth.RequestOptions): [string, RequestInit] => {
  const o = newOAuth(c);
  const authorisation = o.toHeader(o.authorize(opts, a)).Authorization;
  const params = new URLSearchParams();
  if (opts.data instanceof Object) {
    for (const k of Object.keys(opts.data).sort()) {
      params.append(k, opts.data[k]);
    }
  }
  const signedInit: RequestInit = {
    headers: { 'Authorization': authorisation },
    method: opts.method,
    body: params,
  };
  return [opts.url, signedInit];
};
