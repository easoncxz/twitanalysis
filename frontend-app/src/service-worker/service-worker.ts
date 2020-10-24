declare let self: ServiceWorkerGlobalScope;
export {};

self.addEventListener('install', (event: ExtendableEvent) => {
  event.waitUntil(
    new Promise((resolve, _reject) => {
      setTimeout(() => {
        console.log('Install finished.');
        resolve();
      }, 3000);
      console.log('Installing...');
    }),
  );
});
