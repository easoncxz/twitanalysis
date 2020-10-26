TwitAnalysis
============

![Daily Workflow](https://github.com/easoncxz/twitanalysis/workflows/Daily%20Workflow/badge.svg?branch=master)

Live instance deployed at: <https://twitanalysis.easoncxz.com/>

An experiment-lab for building Twitter tools using the Twitter v1.1 REST API.

- `backend-app/` is a Haskell Stack project, defining the backend server.
- `frontend-app/` is a JavaScript/TypeScript project, building a browser-UI app.
- `docs/` is the Jekyll site that serves the [Github Pages for this repo][gh-pages]

[gh-pages]: https://easoncxz.github.io/twitanalysis

# Dev workflow

## Static files

Currently not well defined. The critical link is a symlink file:

    $ readlink backend-app/static
    ../frontend-app/static

which the frontend build-chain writes to, and the backend server serves up. 

## SSL during development

This is so that I can use `Secure` cookies while not changing code between
development and production.

The key link is defined in a reverse-proxy SSL-termination in a Git submodule:

- `frontend-app/secure/`

Run the Haskell server, which listens to HTTP:

    $ cd backend-app/
    $ stack run     # or equivalent command

Then concurrently run this other process to set up an HTTPS server to forward
to the Haskell one:

    $ cd frontend-app/
    $ yarn ssl

## Tooling

Lots of tools. Probably need to be manually installed:

- `rbenv`
- `nodenv`
- Golang

Probably taken care of by the Rakefile:

- Haskell `stack`, which takes care of compilers and libraries;
- `yarn` and all JavaScript/TypeScript things.

Once you have `rbenv` and `nodenv` installed, you can try the following Rake
tasks.

## Build commands

There is a build system piggy-backing on the Jekyll Gem bundle (`Gemfile`) using 
Ruby Rake:

```
$ ./activate.sh
(.gems) $ gem install bundler   # Ruby build tooling
(.gems) $ bundle                # Ruby dependencies
(.gems) $ bundle exec rake  # build everything
```

I also wrote up a task to run a development server:


    (.gems) $ bundle exec rake dev

With that running, visit <https://localhost:5050> to see the app running. If you
don't have any valid SSL certificates entrusted into the OS yet, you can also
try <http://localhost:5000>.

I may add deploy commands as `rake` targets; why not, it's simple to read, 
write, and run.

## Environments: build-time and run-time

The environment variables needed for the backend-app server during runtime is listed in `backend-app/README.md`.

The following environment variables are needed for the build/test/package/publish process (mostly running Rake commands):

### GITHUB_OAUTH_TOKEN

This is for publishing to my Github account; only needed for the `rake publish[...]` task.

I obtained this token as per these docs:

- Old doc: https://developer.github.com/apps/building-oauth-apps/authorizing-oauth-apps/
- New doc: https://docs.github.com/en/free-pro-team@latest/developers/apps/authorizing-oauth-apps


## Publish

Package everything up into a tarball containing static HTML/JavaScript and
a binary-executable for running the backend-app server on port 5000.

```
(.gems) $ bundle exec rake 'publish[some-git-tag]'
```

The Git tag doesn't have to exist; it will be created if it doesn't exist.

This Rake task is idempotent; it's safe to run it again on the same args.

# License

All rights reserved, easoncxz 2020.

This is until I make up my mind about what license to choose for this one.
