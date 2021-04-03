import type { Dispatch } from 'redux';

import * as core from './core';
import * as tdb from './twitter/storage';
import { User, Status, t, parseStatus, parseArray } from './twitter/models';
import { fetchJson } from './utils/utils';

export class Effects {
  constructor(private readonly dispatch: Dispatch<core.Msg>) {}

  noop(): core.Msg {
    return {
      type: 'noop',
    };
  }

  fetchFaves(
    nik: string,
    {
      count = 200,
      since_id,
      max_id,
    }: {
      count?: number;
      max_id?: string;
      since_id?: string;
    },
  ): core.Msg {
    const searchParams = new URLSearchParams();
    searchParams.append('screen_name', nik);
    searchParams.append('count', count.toString());
    if (since_id !== undefined) {
      searchParams.append('since_id', since_id);
    }
    if (max_id !== undefined) {
      searchParams.append('max_id', max_id);
    }
    fetchJson(t('favorites/list') + '?' + searchParams.toString())
      .then(parseArray(parseStatus))
      .then(
        (statuses: Status[]) => {
          this.dispatch({ type: 'receive_fetch_faves', statuses });
        },
        (e) => this.dispatch({ type: 'error_fetch_faves', error: e }),
      );
    return {
      type: 'start_fetch_faves',
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

  createTwitDb(): core.Msg {
    tdb.openMyDB().then(() => this.dispatch({ type: 'idb_created_db' }));
    return { type: 'idb_creating_db' };
  }

  deleteTwitDb(): core.Msg {
    tdb.deleteMyDb();
    return core.noop();
  }

  saveUser(user: User): core.Msg {
    tdb
      .withDB(async (db) => {
        const tx = db.transaction('users', 'readwrite');
        const users = tx.objectStore('users');
        await users.add(user);
        return tx.done;
      })
      .then(() => {
        this.dispatch({ type: 'idb_saved_user', user });
      });
    return { type: 'idb_saving_user', user };
  }

  readUser(id_str: string): core.Msg {
    tdb
      .withDB(async (db) => {
        const tx = db.transaction('users');
        const users = tx.store;
        return users.get(id_str);
      })
      .then((user) => {
        this.dispatch({ type: 'idb_read_user', id_str, user });
      });
    return { type: 'idb_reading_user', id_str };
  }

  putTweet(s: Status): core.Msg {
    tdb
      .withDB(async (db) => {
        const tx = db.transaction('tweets', 'readwrite');
        const tweets = tx.objectStore('tweets');
        return tweets.put(s);
      })
      .then(() => this.dispatch({ type: 'idb_saved_tweet', status: s }));
    return { type: 'idb_saving_tweet', status: s };
  }

  putTweets(ss: Status[]): core.Msg {
    tdb
      .withDB(async (db) => {
        const tx = db.transaction('tweets', 'readwrite');
        const tweets = () => tx.objectStore('tweets');
        const result = await Promise.all(ss.map((s) => tweets().put(s)));
        await tx.done;
        return result;
      })
      .then(() => this.dispatch({ type: 'idb_saved_tweets', statuses: ss }));
    return { type: 'idb_saving_tweets', statuses: ss };
  }

  readTweet(id_str: string): core.Msg {
    tdb
      .withDB(async (db) => {
        const tx = db.transaction('tweets');
        const tweets = tx.objectStore('tweets');
        return tweets.get(id_str);
      })
      .then((status) =>
        this.dispatch({ type: 'idb_read_tweet', id_str, status }),
      );
    return { type: 'idb_reading_tweet', id_str };
  }

  readAllTweets(): core.Msg {
    tdb
      .withDB(async (db) => {
        const tx = db.transaction('tweets');
        const tweets = tx.objectStore('tweets');
        const res = [];
        let cursor = await tweets.openCursor(undefined, 'prev');
        while (cursor) {
          res.push(cursor.value);
          cursor = await cursor.continue();
        }
        await tx.done;
        return res;
      })
      .then((statuses) =>
        this.dispatch({ type: 'idb_read_all_tweets', statuses }),
      );
    return { type: 'idb_reading_all_tweets' };
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
