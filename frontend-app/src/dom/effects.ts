import type { Dispatch } from 'redux';

import * as core from './core';
import { User, Status, t } from './twitter';

export type Effects = {
  noop(): core.Msg;
  fetchMe(): core.Msg;
  fetchFaves(nik: string, count: number): core.Msg;
  sendTweet(t: string): core.Msg;
  registerServiceWorker(): core.Msg;
  unregisterAllServiceWorkers(): core.Msg;
};

export const effectsOf = (dispatch: Dispatch<core.Msg>): Effects => ({
  noop() {
    return {
      type: 'noop',
    };
  },
  fetchMe() {
    fetch(t('account/verify_credentials'))
      .then((r) => r.json())
      .then((user: User) => {
        dispatch({ type: 'receive_fetch_me', user });
      });
    return {
      type: 'start_fetch_me',
    };
  },
  fetchFaves(nik: string, count = 200) {
    const searchParams = new URLSearchParams();
    searchParams.append('screen_name', nik);
    searchParams.append('count', count.toString());
    fetch(t('favorites/list') + '?' + searchParams.toString())
      .then((r) => r.json())
      .then((statuses: Status[]) => {
        dispatch({ type: 'receive_fetch_faves', statuses });
      });
    return {
      type: 'start_fetch_faves',
    };
  },
  sendTweet(text: string) {
    const body = new URLSearchParams();
    body.set('status', text);
    fetch(t('statuses/update'), {
      method: 'POST',
      headers: {
        'Content-Type': 'application/x-www-form-urlencoded',
      },
      body,
    })
      .then((r) => r.json())
      .then((status: Status) => {
        dispatch({ type: 'receive_send_tweet', status });
      });
    return { type: 'start_send_tweet' };
  },
  registerServiceWorker() {
    (async () => {
      if ('serviceWorker' in navigator) {
        try {
          const reg = await navigator.serviceWorker.register('/sw.js');
          console.log(
            'From main.js: ServiceWorker registration complete:',
            reg,
            reg.scope,
          );
        } catch (e) {
          console.log('From main.js: ServiceWorker registration failed:', e);
        }
      } else {
        console.log("Main.ts: ServiceWorker doesn't appear to be supported.");
      }
    })();
    return this.noop();
  },
  unregisterAllServiceWorkers() {
    (async () => {
      if ('serviceWorker' in navigator) {
        try {
          const regs = await navigator.serviceWorker.getRegistrations();
          for (const r of regs) {
            const ok = await r.unregister();
            if (!ok) {
              throw new Error('unregister not ok');
            }
          }
        } catch (e) {
          console.warn(`Main.ts: ServiceWorker unregisteration failed: ${e}`);
        }
      } else {
        console.log("Main.ts: ServiceWorker doesn't appear to be supported.");
      }
    })();
    return this.noop();
  },
});
