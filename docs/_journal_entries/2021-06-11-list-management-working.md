---
title: "Main List Management features now working"
---

# APIs now working??

I don't know why the `POST lists/members/create` API that was returning
unexpected 500 responses last time just worked today without me having changed anything.

With this API working, it didn't take me long to piece together its reverse
API, `POST lists/members/destroy`, and wire up the API calls to the buttons in the UI.

# React compromises

I also had enough time to make some little UI enhancements to load from
IndexedDB on component mount, so that I don't have to keep clicking those
"load" buttons when I don't strictly need to.

This is a compromise -- I'm here using React Hooks. The `useEffect` hook, to be
specific.  This isn't something I enjoy doing -- ideally, I should be able to
perform an effect and then dispatch an action in response to a route-change;
however, it still remains rather tricky how to get this set up.
