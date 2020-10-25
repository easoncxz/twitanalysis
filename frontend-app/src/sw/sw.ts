declare let self: ServiceWorkerGlobalScope;
export {};

const CACHE_NAME = 'v1';

self.addEventListener('install', (event: ExtendableEvent) =>
  event.waitUntil(
    (async () => {
      const c = await caches.open(CACHE_NAME);
      await c.addAll(['/main.js', '/app.html', '/favicon.ico']);
      console.log('service-worker.js added some files to cache.');
    })(),
  ),
);

self.addEventListener('activate', (event: ExtendableEvent) =>
  event.waitUntil(
    (async () => {
      const ks = await caches.keys();
      const outdated = ks.filter((k) => k !== CACHE_NAME);
      console.log(`sw.js activate: deleting old caches - ${outdated}`);
      await Promise.all(outdated.map((k) => caches.delete(k)));
    })(),
  ),
);

self.addEventListener('fetch', async (event: FetchEvent) => {
  event.respondWith(
    (async () => {
      const hit = await caches.match(event.request);
      if (hit) {
        (async () => {
          const resp = await fetch(event.request);
          const c = await caches.open(CACHE_NAME);
          await c.put(event.request, resp);
          console.log(`sw.js fetch: cache updated - ${event.request.url}`);
        })(); // not awaiting this
        console.log(`sw.js fetch: cache hit - ${event.request.url}`);
        return hit;
      } else {
        return fetch(event.request);
      }
    })(),
  );
});
