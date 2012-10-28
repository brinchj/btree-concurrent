btree-concurrent
================


A backend agnostic, concurrent BTree with relaxed balance[1] written in Haskell using STM.


Although the code does work, it is neither production-ready nor complete. See
the TODO.org file for more details on missing parts.


Features include:

* Caching: While nodes are periodically saved on persistent storage (e.g. disk) they are cached in-memory during operations.

* Live flushing: It is possible to save the current version of the tree to disk and run modifying operations at the same time. The nodes are updated buttom-up to ensure a valid tree in the case of a crash.

* Backend agnosticism: A simple API is used as an abstraction for the persistent storage.

* Verification: The test-suite uses QuickCheck to compare the trees behaviour to that of Data.Map.


Deficients include:

* Too much memory usage. Nodes are not stored in a compact fashion and are constantly being serialized/deserialized.

* findMin and findMax are incomplete and may fail (see TODO for details).

* The implementation does not parallelize well.



[1] B-trees with relaxed balance, K. S. Larsen & R. Fagerberg, Parallel Processing Symposium, 1995. Proceedings., 9th International