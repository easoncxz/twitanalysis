import type { Dispatch } from 'redux';

import * as core from './core';
import { User, Status, t, parseUser, parseStatus, parseList } from './twitter';

export type Effects = {
  noop(): core.Msg;
  fetchMe(): core.Msg;
  fetchFaves(nik: string, count: number): core.Msg;
  sendTweet(t: string): core.Msg;
  registerServiceWorker(): core.Msg;
  unregisterAllServiceWorkers(): core.Msg;
};

function guardOk(r: Response): Response {
  if (!(200 <= r.status && r.status < 300)) {
    throw new Error(`Request not ok: ${r.status}`);
  } else {
    return r;
  }
}

async function fetchJson(url: string, init?: RequestInit): Promise<unknown> {
  return fetch(url, init)
    .then(guardOk)
    .then((r) => r.json());
}

export const effectsOf = (dispatch: Dispatch<core.Msg>): Effects => {
  return {
    noop() {
      return {
        type: 'noop',
      };
    },
    fetchMe() {
      fetchJson(t('account/verify_credentials'))
        .then(parseUser)
        .then(
          (user: User) => {
            dispatch({ type: 'receive_fetch_me', user });
          },
          (e) => dispatch({ type: 'error_fetch_me', error: e }),
        );
      return {
        type: 'start_fetch_me',
      };
    },
    fetchFaves(nik: string, count = 200) {
      const searchParams = new URLSearchParams();
      searchParams.append('screen_name', nik);
      searchParams.append('count', count.toString());
      fetchJson(t('favorites/list') + '?' + searchParams.toString())
        .then(parseList(parseStatus))
        .then(
          (statuses: Status[]) => {
            dispatch({ type: 'receive_fetch_faves', statuses });
          },
          (e) => dispatch({ type: 'error_fetch_faves', error: e }),
        );
      return {
        type: 'start_fetch_faves',
      };
    },
    sendTweet(text: string) {
      const body = new URLSearchParams();
      body.set('status', text);
      fetchJson(t('statuses/update'), {
        method: 'POST',
        headers: {
          'Content-Type': 'application/x-www-form-urlencoded',
        },
        body,
      })
        .then(parseStatus)
        .then(
          (status: Status) => {
            dispatch({ type: 'receive_send_tweet', status });
          },
          (e) => dispatch({ type: 'error_send_tweet', error: e }),
        );
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
            dispatch({ type: 'add_error', error: e });
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
            dispatch({ type: 'add_error', error: e });
          }
        } else {
          console.log("Main.ts: ServiceWorker doesn't appear to be supported.");
        }
      })();
      return this.noop();
    },
  };
};
