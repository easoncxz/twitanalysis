---
title: "IndexedDB composite indices and sorted queries"
---

I'm trying to show tweets of a given user, in a sensible way, via queries from
IndexedDB. The query semantics I want is something like this:

```sql
SELECT *
FROM tweets t
WHERE t.user_id = :user_id
ORDER BY t.timestamp DESC
```

That is, I want to show tweets of the given user in reverse-chronological order
-- i.e. latest tweets first.

Since my idea of storing tweets is to use one `tweets` object-store and to use
many application-specific indices on it, the question now becomes how to define
an index that provides the functionality I want:

- Filter by user ID, and
- Order by some kind of timestamp.

I'm hoping that Twitter's [Snowflake][snowflake] algorithm would lead to the
`id_str` field of tweets (`Status` objects) be roughly chronological, which is
good enough. (See also [newer Twitter
docs](https://developer.twitter.com/en/docs/twitter-ids).)

[snowflake]: https://blog.twitter.com/engineering/en_us/a/2010/announcing-snowflake.html

## Composite keys in IndexedDB indices

The MDN page on how to use an index is here:

- Section: [Structuring the database](https://developer.mozilla.org/en-US/docs/Web/API/IndexedDB_API/Using_IndexedDB#structuring_the_database)

In particular this snippet:

```javascript
// Create an index to search customers by name. We may have duplicates
// so we can't use a unique index.
objectStore.createIndex("name", "name", { unique: false });
```

It's not at all clear that it even is a possibility that passing a non-string
value to the `keyPath` parameter exists, but ok, the TypeScript `.d.ts` files
from the npm `idb` package told me that `keyPath: string | string[]`:

```typescript
// node_modules/idb/build/esm/entry.d.ts

/**
 * Creates a new index in store.
 *
 * Throws an "InvalidStateError" DOMException if not called within an upgrade transaction.
 */
createIndex<IndexName extends IndexNames<DBTypes, StoreName>>(
  name: IndexName,
  keyPath: string | string[],
  options?: IDBIndexParameters,
): IDBPIndex<DBTypes, TxStores, StoreName, IndexName>;
```

But what is the semantics of the `string[]` case?

## Lexicographic ordering

I took a look around StackOverflow, and found these relevant pages:

- [querying a compound index](https://stackoverflow.com/questions/26203075/querying-an-indexeddb-compound-index-with-a-shorter-array)
- [making a compound query](https://stackoverflow.com/questions/12084177/in-indexeddb-is-there-a-way-to-make-a-sorted-compound-query/15625231#15625231)

So it looks like stuff like this is a possibility:

```javascript
objectStore.createIndex("name", ["name", "age"], { unique: false });
```

with the semantics that the index would be sorted "name"-major, and then "age"
subsequently.

With this knowledge, we have just about enough building blocks to build what we
want.
