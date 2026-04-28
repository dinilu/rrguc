# Find the first best column-name match

Searches a vector for the first value beginning with one of the supplied
patterns, ignoring case.

## Usage

``` r
find_best_match(patterns, vector)
```

## Arguments

- patterns:

  Character vector of candidate patterns.

- vector:

  Character vector to search.

## Value

A character string with the first match, or `None` if no match exists.
