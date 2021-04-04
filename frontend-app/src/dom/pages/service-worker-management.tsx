import React from 'react';

class Effects {
  async registerServiceWorker(): Promise<
    ServiceWorkerRegistration | undefined
  > {
    if ('serviceWorker' in navigator) {
      try {
        const reg = await navigator.serviceWorker.register('/sw.js');
        console.log(
          'From main.js: ServiceWorker registration complete:',
          reg,
          reg.scope,
        );
        return reg;
      } catch (e) {
        console.log('From main.js: ServiceWorker registration failed:', e);
      }
    } else {
      console.log("Main.ts: ServiceWorker doesn't appear to be supported.");
    }
  }

  async unregisterAllServiceWorkers(): Promise<
    ServiceWorkerRegistration[] | undefined
  > {
    if ('serviceWorker' in navigator) {
      try {
        const regs: readonly ServiceWorkerRegistration[] = await navigator.serviceWorker.getRegistrations();
        const cancelled: ServiceWorkerRegistration[] = [];
        for (const r of regs) {
          const ok: boolean = await r.unregister();
          if (!ok) {
            throw new Error('unregister not ok');
          } else {
            cancelled.push(r);
          }
        }
        return cancelled;
      } catch (e) {
        console.warn(`Main.ts: ServiceWorker unregisteration failed: ${e}`);
      }
    } else {
      console.log("Main.ts: ServiceWorker doesn't appear to be supported.");
    }
  }
}

export const view = (): React.ReactFragment => {
  const effects = new Effects();
  return (
    <>
      <button onClick={() => effects.registerServiceWorker()}>
        register ServiceWorker
      </button>
      <button onClick={() => effects.unregisterAllServiceWorkers()}>
        unregister all ServiceWorkers
      </button>
    </>
  );
};
