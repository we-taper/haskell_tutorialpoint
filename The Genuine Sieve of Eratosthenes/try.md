# Haskell Problem

Some algorithms, when written in Haskell might not be as efficient as it is designed:

1. [This post][1] discusses the issue with the quick sort at length.
2. [This paper][2] discusses the issue of the sieves algorithm in Haskell at length.

[1]: https://stackoverflow.com/questions/7717691/why-is-the-minimalist-example-haskell-quicksort-not-a-true-quicksort
[2]: https://www.cs.hmc.edu/~oneill/papers/Sieve-JFP.pdf

I have implemented the sieves algorithm in both Haskell([try.hs](try.hs)) and Julia ([try.jl](try.jl) to test their speed differences. But currently I cannot measure the execution time in Haskell and therefore the test is inconclusive. It should be noted that I do notice that Haskell's implementation is slower than Julia's, though not significantly.