# purescript-logoot-core

This package defines the core Logoot algorithm as described in "Logoot: A
Scalable Optimistic Replication Algorithm for Collaborative Editing on P2P
Networks" by Stéphane Weiss, Pascal Urso and Pascal Molli.

Wherever possible, it tries to be polymorphic in its types.

The core identifier generation algorithm is not a direct port of the imperative
pseudocode found in the paper; it has been modified to fit a more functional
style of programming.

# Usage

This package is only useful for identifier generation. In an actual collaborative
editor, the following will need to be implemented:

1. Sending messages to other peers (you can use `encodeJSON` on identifiers for this)
2. Receiving messages from other peers (similarly for `decodeJSON`)
3. Assigning identifiers to atomic elements in the document.
4. Retrieving elements based on identifiers.
5. Retrieving document history for newcomers.

Note that since identifiers generated are unique and totally ordered,
it makes sense to use a fast (i.e. O(log n)) search algorithm, like
a simple binary search, to retrieve identifiers.

Also, if server CPU cyles are not an issue, it may be worthwhile to keep a "current document state" that holds a list of active identifiers and document elements, then broadcast that to a new user upon connecting. The alternative is to transmit the whole document history and let the user recompute the latest document state.