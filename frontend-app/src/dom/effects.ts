import type { Dispatch } from 'redux';

import * as core from './core';
import { Status, t, parseStatus } from './twitter/models';
import { fetchJson } from './utils/utils';

export class Effects {
  constructor(private readonly dispatch: Dispatch<core.Msg>) {}

  noop(): core.Msg {
    return {
      type: 'noop',
    };
  }

  sendTweet(text: string): core.Msg {
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
          this.dispatch({ type: 'receive_send_tweet', status });
        },
        (e) => this.dispatch({ type: 'error_send_tweet', error: e }),
      );
    return { type: 'start_send_tweet' };
  }

  registerServiceWorker(): core.Msg {
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
          this.dispatch({ type: 'add_error', error: e });
        }
      } else {
        console.log("Main.ts: ServiceWorker doesn't appear to be supported.");
      }
    })();
    return this.noop();
  }

  unregisterAllServiceWorkers(): core.Msg {
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
          this.dispatch({ type: 'add_error', error: e });
        }
      } else {
        console.log("Main.ts: ServiceWorker doesn't appear to be supported.");
      }
    })();
    return this.noop();
  }
}
