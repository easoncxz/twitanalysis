---
title: "A very good title indeed."
---

# Getting the server set up

Nothing major, just using Scotty:

- <https://www.stackage.org/haddock/lts-16.10/scotty-0.11.6/Web-Scotty.html>

Perhaps the slightly more major thing, and indeed was the first thing I tried, before I even tried Scotty, was a library of Twitter types. I found this library, [`twitter-types` (on Hackage)](https://hackage.haskell.org/package/twitter-types), which seems a bit old, but nonetheless seem to work.

- Github page for the library: <https://github.com/himura/twitter-types>
- A fork contributing ~4 commits (but fell behind ~100 commits): <https://github.com/glasserc/twitter-types/tree/add-status>

I also have some basic tests set up, and indeed, de-serialisation works.

# Gotcha

The Twitter reference docs are showing a syntactically-invalid code-block as their example value for their JSON objects. In this case, it was the example for the Status object (i.e. a tweet) on the reference page for [`GET statuses/user_timeline`](https://developer.twitter.com/en/docs/twitter-api/v1/tweets/timelines/api-reference/get-statuses-user_timeline):

![image](https://user-images.githubusercontent.com/1837369/90394458-cf1b6380-e0e6-11ea-9a86-e4340b4e1500.png)

![image](https://user-images.githubusercontent.com/1837369/90394336-9d0a0180-e0e6-11ea-9f15-b990ea96c94f.png)

Look at how the value part of the JSON expression didn't escape the double-quotes. It caused me a lot of digging around before I found out what was wrong! I thought it was the `twitter-types` library that was out-of-date. Apparently no evidence for that yet!
