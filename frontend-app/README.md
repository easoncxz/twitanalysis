twitanalysis-frontend
=====================

This directory is an ordinary frontend npm project, as you can see from the 
`package.json` file.

There are some experimental NodeJS stuff sprinkled here and there, but those
are mostly just demos.

## Motivation

### Learn stuff, get comfortable with UI development

Learn some frontend web UI app things, like React/Redux, and just basic 
HTML/DOM/CSS. It's kind of overdue for me to learn these.

### Rapid feature changes

Keep the "ideation-implementation" loop as small as possible, by involving the 
backend as little as possible.

This is because I'm still not sure at all what exact features I wanted to build.

### High inner-loop performance

Allow for the best performance by keeping the data where the computation is.

While it's ok to wait one more network-roundtrip while data is being loaded from 
Twitter's API via my backend server, it's not acceptable to need to wait long on 
every keypress once the data is loaded.

This project expects to have _very heavy_, perhaps borderline unreasonable, use 
of [localStorage][localStorage].

[localStorage]: https://developer.mozilla.org/en-US/docs/Web/API/Window/localStorage

## Scope of project

No OAuth!

Since Twitter's v1.1 REST API requires OAuth 1.0a, and OAuth 1.0a does not 
appear to be securely feasible in a browser frontend setting (a single-page app 
does not have the ability to keep secrets), in this frontend project there 
should be no OAuth logic involed, no "request signing", no log-in, no 
authentication or even user-identification, no cross-origin requests. Simple.

By the time control even reaches the frontend-app, authentication should already 
have been taken care of by the backend-app via ordinary secure cookie-sessions.
The backend will ask the browser to pass relevant cookies when this app makes 
XMLHttpRequests to the backend server via e.g. `fetch`.

## Build toolchain

I don't know how to do JavaScript, so I kept things simple at the cost of a
lack of fancy niceities like hot-reloading or a fast build.

- I use TypeScript because I am unable to write software without a type-checker.
  If my needs become stronger, I will consider switching to PureScript.
- I use Rollup so that I can use npm packages as part of my in-browser 
    JavaScript app implementation.
  - Also, development tooling like JavaScript/TypeTypeScript Language Server 
    Protol servers seem to work better when there are things in a 
    `node_modules/` directory.  I found out about this after noticing that 
    [jspm](https://jspm.org/) makes my lose all my in-editor auto-completions.
- I use Yarn because it is fast.

The build steps are defined as 
[`npm-run-script`](https://docs.npmjs.com/cli/run-script) targets and can be 
invoked via `yarn` or `npm run`:

    $ yarn build    # Perform a one-off build of everything
    $ yarn dev      # Start a long-running process to watch for changes

The build is step-by-step like a waterfall:

- TypeScript `tsc` reads from `src/` and writes to `ts-out/`.
- Rollup reads from `ts-out/` and writes to `static/`.

Then the backend server will use some `../something` path to reach over into 
this `static/` directory to find the build output to serve.

As such, during development of this app, it makes sense to not launch the 
Haskell backend server at all, and just use any HTTP static server, like the
one in the Python built-in libraries. Indeed, my `package.json` 
`npm-run-scripts` do use the `python3` command.

If and when it makes sense, I may:

- swap out parts of the pipeline (e.g. replace TypeScript `tsc` with PureScript `spago`), or
- add stages to the pipeline (e.g. some minification step).

Only if I get very comfortable with JavaScript tooling would I consider changing 
the "separate stages" build pipeline into an "integrated plugins" one, e.g. by 
using something like `@rollup/plugin-typescript` --- anything that has a 
double-barreled name.
