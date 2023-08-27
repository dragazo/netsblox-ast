#!/usr/bin/env python

import aiohttp
import asyncio
import certifi
import ssl

BASE_URL = 'https://editor.netsblox.org'

SSL_CONTEXT = ssl.create_default_context(cafile = certifi.where())

def escape(s: str) -> str:
    return s.replace('"', '\\"')

async def main():
    async with aiohttp.ClientSession() as session:
        async with session.get(f'{BASE_URL}/services', ssl = SSL_CONTEXT) as res:
            services_meta = await res.json(content_type = None) # ignore content type in case response mime type is wrong
            services = [x['name'] for x in services_meta]
            services.sort()

            async def get_rpcs(service: str):
                async with session.get(f'{BASE_URL}/services/{service}', ssl = SSL_CONTEXT) as res:
                    raw = await res.json(content_type = None) # ignore content type in case response mime type is wrong
                    rpcs = [(k, [x['name'] for x in v['args']]) for k, v in raw['rpcs'].items()]
                    rpcs.sort(key = lambda v: v[0])
                    return rpcs
            rpcs_meta = await asyncio.gather(*[asyncio.ensure_future(get_rpcs(x)) for x in services])

    with open('src/rpcs.rs', 'w') as f:
        f.write("pub(crate) const SERVICE_INFO: &'static [(&'static str, &'static [(&'static str, &'static [&'static str])])] = [\n")
        for i in range(len(services)):
            service = services[i]
            rpcs = rpcs_meta[i]

            f.write(f'    ("{escape(service)}", [\n')
            for rpc, args in rpcs:
                trans_args = [f'"{escape(x)}"' for x in args]
                f.write(f'        ("{escape(rpc)}", [{", ".join(trans_args)}].as_slice()),\n')
            f.write('    ].as_slice()),\n')
        f.write('].as_slice();')

def main_sync():
    loop = asyncio.new_event_loop()
    loop.run_until_complete(main())
    loop.run_until_complete(asyncio.sleep(1)) # workaround needed on windows - for some reason they close the proactor event loop early otherwise
    loop.close()

if __name__ == '__main__':
    main_sync()
