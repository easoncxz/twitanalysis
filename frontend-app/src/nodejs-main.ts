
//import { dirname } from 'path';
//import { fileURLToPath } from 'url';
//export const srcPath = dirname(fileURLToPath(import.meta.url));
//export const isMain = () => process.argv[1] === fileURLToPath(import.meta.url);

export const srcPath = __dirname;
export const isMain = (): boolean => require.main === module;

import * as OAuthClient from './oauth-client';

if (isMain()) {
  const args = OAuthClient.prep(
    {
      url: 'https://api.twitter.com/1.1/statuses/update.json',
      method: 'POST',
      data: {
        status: 'Tweeting from ts-node REPL.'
      },
    }
  );
  console.log(args);
  //fetch(...args)
  //  .then(r => {
  //    console.log(r);
  //    return r.json();
  //  })
  //  .then(j => {
  //    console.log(j);
  //  });
}
