import { openDB } from 'idb';
import type {
  IDBPDatabase,
  IDBPObjectStore,
  IDBPTransaction,
  DBSchema,
} from 'idb';

type Thing = {
  id: string;
  foo: string;
  bar: number;
};

interface MyDB extends DBSchema {
  'table-one': {
    key: string;
    value: Thing;
    indexes: {
      'idx-bar': number;
    };
  };
}

export const things: Thing[] = [
  { id: 'thing-one', foo: 'foo', bar: 3 },
  { id: 'other-thing', foo: 'nothing', bar: 3000 },
];

async function defineSchema(
  db: IDBPDatabase<MyDB>,
  _oldVersion: number,
  _newVersion: number,
  tx: IDBPTransaction<MyDB>,
): Promise<void> {
  console.log('Inside openDB.upgrade');
  const tableOne: IDBPObjectStore<
    MyDB,
    'table-one'[],
    'table-one'
  > = db.createObjectStore('table-one', {
    keyPath: 'id',
  });
  tableOne.createIndex('idx-bar', 'bar');
  return tx.done;
}

export async function openMyDB(): Promise<IDBPDatabase<MyDB>> {
  const db: IDBPDatabase<MyDB> = await openDB<MyDB>('first-db', 1, {
    blocked() {
      console.log('openDB Blocked');
    },
    terminated() {
      console.log("We're sad to see you go.");
    },
    blocking() {
      console.log("Sorry, just a moment and we'll head off");
    },
    upgrade: defineSchema,
  });
  console.log('Opened IDBPDatabase:', db);
  return db;
}

export async function insertSomeData(
  db: IDBPDatabase<MyDB>,
  ts: Thing[],
): Promise<void> {
  const tx = db.transaction(['table-one'], 'readwrite');
  const os = tx.objectStore('table-one');
  console.log('Using transaction:', tx);
  console.log('Using objectStore:', os);
  for (const t of ts) {
    await os.put(t);
  }
  console.log('About to commit transaction');
  return tx.done;
}

export async function clearTable(db: IDBPDatabase<MyDB>): Promise<void> {
  const tx = db.transaction(['table-one'], 'readwrite');
  const os = tx.objectStore('table-one');
  console.log("About to clear everything from 'table-one'...");
  await os.clear();
  return tx.done;
}
