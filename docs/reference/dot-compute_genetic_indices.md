# Compute genetic indices for selected alleles

Internal helper that calculates allele frequencies, population counts,
Lo, Le, log-transformed indices, R coefficient, and the associated plot.

## Usage

``` r
.compute_genetic_indices(mat, pops, alleles)
```

## Arguments

- mat:

  Numeric matrix or data frame of genetic data.

- pops:

  Population identifier for each row of `mat`.

- alleles:

  Character vector of allele column names to analyze.

## Value

A list with `table`, `R`, and `plot`.
