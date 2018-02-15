# purescript-logoot-core

This package defines the core Logoot algorithm as described in "Logoot: A
Scalable Optimistic Replication Algorithm for Collaborative Editing on P2P
Networks" by Stéphane Weiss, Pascal Urso and Pascal Molli.

Wherever possible, it tries to be polymorphic in its types.

The core identifier generation algorithm is not a direct port of the imperative
pseudocode found in the paper; it has been modified to fit a more functional
style of programming.