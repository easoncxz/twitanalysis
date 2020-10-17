TwitAnalysis
============

An experiment-lab for building Twitter tools using the Twitter v1.1 REST API.

- `backend-app/` is a Haskell Stack project, defining the backend server.
- `frontend-app/` is a JavaScript/TypeScript project, building a browser-UI app.
- `docs/` is the Jekyll site that serves the [Github Pages for this repo][gh-pages]

[gh-pages]: https://easoncxz.github.io/twitanalysis

# Dev workflow and tooling

Currently not well defined. The critical link is a symlink file:

    $ ls -l backend-app/static
    lrwxr-xr-x  1 eason  staff  22 12 Sep 15:10 backend-app/static -> ../frontend-app/static

which the frontend build-chain writes to, and the backend server serves up. 

Lots of tools. Probably need to be manually installed:

- `rbenv`
- Golang

Probably taken care of by the Rakefile:

- `nodenv`
- Haskell `stack`
- All JavaScript/TypeScript things, now that `nodenv` is available

## Build commands

There is a build system piggy-backing on the Jekyll Gem bundle (`Gemfile`) using 
Ruby Rake:

```
$ ./activate.sh
(.gems) $ gem install bundler   # Ruby build tooling
(.gems) $ bundle                # Ruby dependencies
(.gems) $ bundle exec rake  # build everything
```

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
