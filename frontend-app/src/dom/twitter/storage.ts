import { openDB, deleteDB } from 'idb';
import type { IDBPDatabase, IDBPTransaction, DBSchema } from 'idb';

import * as twitter from './models';

interface TwitDb extends DBSchema {
  tweets: {
    key: string;
    value: twitter.Status;
  };
  users: {
    key: string;
    value: twitter.User;
  };
  lists: {
    key: string;
    value: twitter.List;
  };
  listMemberships: {
    key: number; // auto-increment
    value: twitter.ListMembership;
    indexes: {
      'by-list': string;
    };
  };
}

const dbName = 'twitanalysis-idb';
const currentVersion = 3;

export async function openMyDB(): Promise<IDBPDatabase<TwitDb>> {
  const db: IDBPDatabase<TwitDb> = await openDB<TwitDb>(
    dbName,
    currentVersion,
    {
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
        oldVersion: number,
        _newVersion: number,
        tx: IDBPTransaction<TwitDb>,
      ) {
        if (oldVersion < 1) {
          console.log('Inside openDB.upgrade');
          db.createObjectStore('tweets', {
            keyPath: 'id_str',
          });
          db.createObjectStore('users', {
            keyPath: 'id_str',
          });
        }
        if (oldVersion < 2) {
          db.createObjectStore('lists', {
            keyPath: 'id_str',
          });
        }
        if (oldVersion < 3) {
          const listMemberships = db.createObjectStore('listMemberships', {
            autoIncrement: true,
          });
          // There was some confusion about what the keyPath should be;
          // see #24 for discussion.
          listMemberships.createIndex('by-list', 'listIdStr', {
            unique: false,
          });
        }
        return tx.done;
      },
    },
  );
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

export function saveUser(user: twitter.User): Promise<void> {
  return withDB(async (db) => {
    const tx = db.transaction('users', 'readwrite');
    const users = tx.objectStore('users');
    await users.add(user);
    return tx.done;
  });
}

export function readUser(id_str: string): Promise<twitter.User | undefined> {
  return withDB(async (db) => {
    const tx = db.transaction('users');
    const users = tx.store;
    return users.get(id_str);
  });
}

/**
 * Seems to return stored Status ID
 */
export function putTweet(s: twitter.Status): Promise<string> {
  return withDB(async (db) => {
    const tx = db.transaction('tweets', 'readwrite');
    const tweets = tx.objectStore('tweets');
    return tweets.put(s);
  });
}

export function putTweets(ss: twitter.Status[]): Promise<string[]> {
  return withDB(async (db) => {
    const tx = db.transaction('tweets', 'readwrite');
    const tweets = () => tx.objectStore('tweets');
    const result = await Promise.all(ss.map((s) => tweets().put(s)));
    await tx.done;
    return result;
  });
}

export function readTweet(id_str: string): Promise<twitter.Status | undefined> {
  return withDB(async (db) => {
    const tx = db.transaction('tweets');
    const tweets = tx.objectStore('tweets');
    return tweets.get(id_str);
  });
}

export function readAllTweets(): Promise<twitter.Status[]> {
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

export function storeLists(ls: twitter.List[]): Promise<string[]> {
  return withDB(async (db) => {
    const tx = db.transaction('lists', 'readwrite');
    const lists = tx.store;
    const result = await Promise.all(ls.map((l) => lists.put(l)));
    await tx.done;
    return result;
  });
}

export function readLists(): Promise<twitter.List[]> {
  return withDB(async (db) => {
    const tx = db.transaction('lists');
    const lists = tx.store;
    return lists.getAll();
  });
}

/**
 * @returns ids - autoIncrement
 */
export function addListMemberships(
  ps: twitter.ListMembership[],
): Promise<number[]> {
  return withDB(async (db) => {
    const tx = db.transaction('listMemberships', 'readwrite');
    const os = tx.objectStore('listMemberships');
    return Promise.all(
      ps.map((p: twitter.ListMembership): Promise<number> => os.put(p)),
    );
  });
}

export function loadListMembers(listIdStr: string): Promise<twitter.User[]> {
  console.log(`Looking for users in the list: ${listIdStr}`);
  const maybeToList = <T>(x: T | undefined): T[] =>
    x === undefined ? [] : [x];
  return withDB(async (db) => {
    const tx = db.transaction(['listMemberships', 'users']);
    const osMemberships = tx.objectStore('listMemberships');
    const osUsers = tx.objectStore('users');
    const idx = osMemberships.index('by-list');
    const listMemberships = await idx.getAll(listIdStr);
    console.log(
      `We know there should be ${listMemberships.length} users in the list ${listIdStr}`,
    );
    const result = await Promise.all(
      listMemberships.map((pair) =>
        osUsers.get(pair.userIdStr).then(maybeToList),
      ),
    ).then((nested) => nested.flat());
    console.log(`Found ${result.length} users in list ${listIdStr}`);
    return result;
  });
}
