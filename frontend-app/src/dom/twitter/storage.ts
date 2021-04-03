import { openDB, deleteDB } from 'idb';
import type { IDBPDatabase, IDBPTransaction, DBSchema } from 'idb';

import { User, Status } from './models';

interface TwitDb extends DBSchema {
  tweets: {
    key: string;
    value: Status;
  };
  users: {
    key: string;
    value: User;
  };
}

const dbName = 'twitanalysis-idb';
const dbVersion = 1;

export async function openMyDB(): Promise<IDBPDatabase<TwitDb>> {
  const db: IDBPDatabase<TwitDb> = await openDB<TwitDb>(dbName, dbVersion, {
    blocked() {
      console.log('openDB Blocked');
    },
    terminated() {
      console.log("We're sad to see you go.");
    },
    blocking() {
      console.log("Sorry, just a moment and we'll head off");
    },
    upgrade(
      db: IDBPDatabase<TwitDb>,
      _oldVersion: number,
      _newVersion: number,
      tx: IDBPTransaction<TwitDb>,
    ) {
      console.log('Inside openDB.upgrade');
      db.createObjectStore('tweets', {
        keyPath: 'id_str',
      });
      db.createObjectStore('users', {
        keyPath: 'id_str',
      });
      return tx.done;
    },
  });
  return db;
}

export async function deleteMyDb(): Promise<void> {
  await deleteDB(dbName, {
    blocked() {
      console.log(`deleteDB blocked`);
    },
  });
}

export async function withDB<T>(
  action: (_: IDBPDatabase<TwitDb>) => Promise<T>,
): Promise<T> {
  return openMyDB().then(action);
}

// ------
// a bunch of single-transaction actions follows
// ------

export function saveUser(user: User): Promise<void> {
  return withDB(async (db) => {
    const tx = db.transaction('users', 'readwrite');
    const users = tx.objectStore('users');
    await users.add(user);
    return tx.done;
  });
}

export function readUser(id_str: string): Promise<User | undefined> {
  return withDB(async (db) => {
    const tx = db.transaction('users');
    const users = tx.store;
    return users.get(id_str);
  });
}

/**
 * Seems to return stored Status ID
 */
export function putTweet(s: Status): Promise<string> {
  return withDB(async (db) => {
    const tx = db.transaction('tweets', 'readwrite');
    const tweets = tx.objectStore('tweets');
    return tweets.put(s);
  });
}

export function putTweets(ss: Status[]): Promise<string[]> {
  return withDB(async (db) => {
    const tx = db.transaction('tweets', 'readwrite');
    const tweets = () => tx.objectStore('tweets');
    const result = await Promise.all(ss.map((s) => tweets().put(s)));
    await tx.done;
    return result;
  });
}

export function readTweet(id_str: string): Promise<Status | undefined> {
  return withDB(async (db) => {
    const tx = db.transaction('tweets');
    const tweets = tx.objectStore('tweets');
    return tweets.get(id_str);
  });
}

export function readAllTweets(): Promise<Status[]> {
  return withDB(async (db) => {
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
  });
}
