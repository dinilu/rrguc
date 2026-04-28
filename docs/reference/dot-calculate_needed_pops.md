# Calculate needed populations

Internal helper that calculates the cumulative probability of retaining
rare alleles as the number of populations increases.

## Usage

``` r
.calculate_needed_pops(Fst, nmax = 20)
```

## Arguments

- Fst:

  FST value.

- nmax:

  Maximum number of populations to evaluate.

## Value

A data frame with population number and retention probability.
