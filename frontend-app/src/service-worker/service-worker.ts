declare let self: ServiceWorkerGlobalScope;
export {};

console.log('service-worker.js about to add listener for `install` event...');
self.addEventListener('install', (event: ExtendableEvent) => {
  console.log('service-worker.js handling `install` event...');
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
console.log('service-worker.js added listener for `install` event.');
