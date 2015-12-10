# Performance

According to [the benchmarks](https://github.com/nikita-volkov/bytestring-builders-benchmark) this builder implementation beats all the alternatives. It is especially well-suited for generating strict bytestrings, beating the standard builder by at least the factor of 4.

# Motivation

This library is the result of the following Reddit discussions:

* https://www.reddit.com/r/haskell/comments/3qj53a/an_alternative_bytestring_builder/

* https://www.reddit.com/r/haskell/comments/3ql9m7/a_community_benchmark_project_comparing_different/
