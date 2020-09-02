import util from 'util';

export const isMain = (): boolean => require.main === module;

function isProbablyHeaders(h: HeadersInit): h is Headers {
  if (!(h && h instanceof Object)) {
    return false;
  } else {
    for (const k of ['append', 'delete', 'has', 'forEach']) {
      if (!(k in h)) {
        return false;
      }
    }
    return true;
  }
}

export function headersToPairs(h: HeadersInit): [string, string][] {
  const result = [];
  if (h) {
    if (isProbablyHeaders(h)) {
      for (const [k, v] of h.entries()) {
        result.push([k, v] as [string, string]);
      }
    } else if (h instanceof Array) {
      for (const [k, v] of h) {
        result.push([k, v] as [string, string]);
      }
    } else if (h instanceof Object) {
      for (const k in h) {
        result.push([k, h[k]] as [string, string]);
      }
    }
  }
  return result;
}


export function formatCurlCommand(req: RequestInfo, init: RequestInit): string {
  const headerKvs: [string, string][] = headersToPairs(init.headers ?? []);
  return [
    'curl -i',
  ]
    .concat(headerKvs.map(([k, v]) =>
      '-H ' + util.inspect( `${k}: ${v}`))
    ).concat([`-X ${init?.method ?? 'GET'}`])
    .concat([`-d ${util.inspect(init?.body?.toString())}`])
    .concat([util.inspect(req.toString())])
    .join(' \\\n    ');
}

if (isMain()) {
  console.log(formatCurlCommand(
    'http://google.com',
    { body: '' }
  ));
}
