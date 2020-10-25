declare let self: ServiceWorkerGlobalScope;
export {};

const CACHE_NAME = 'v1';

console.log('service-worker.js about to add listener for `install` event...');
self.addEventListener('install', (event: ExtendableEvent) => {
  console.log('service-worker.js handling `install` event...');
  return event.waitUntil(
    (async () => {
      const c = await caches.open(CACHE_NAME);
      console.log('service-worker.js adding files to cache...');
      return c.addAll(['/main.js', '/app.html']);
    })(),
  );
});
console.log('service-worker.js added listener for `install` event.');

self.addEventListener('activate', async (_event: ExtendableEvent) => {
  // Always throw away all caches
  const ks = await caches.keys();
  await Promise.all(
    ks.map((k) => {
      //if (k !== CACHE_NAME) {
      return caches.delete(k);
      //}
    }),
  );
});

self.addEventListener('fetch', (event: FetchEvent) => {
  // Read-cache and update
  // https://serviceworke.rs/strategy-cache-and-update_service-worker_doc.html
  event.respondWith(
    (async () => {
      const hit = await caches.match(event.request);
      if (hit) {
        return hit;
      } else {
        event.waitUntil(
          (async () => {
            const resp = await fetch(event.request);
            const c = await caches.open(CACHE_NAME);
            await c.put(event.request, resp);
          })(),
        );
        throw new Error('Cache miss');
      }
    })(),
  );
});