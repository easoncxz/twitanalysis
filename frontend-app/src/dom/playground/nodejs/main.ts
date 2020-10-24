
export const isMain = (): boolean => require.main === module;

import { tryOAuthClient } from './oauth-client';

if (isMain()) {
  tryOAuthClient();
}
