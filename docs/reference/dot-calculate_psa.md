# Calculate private shared allele percentages

Internal helper that computes sample-level and population-level PSA
summaries for each group.

## Usage

``` r
.calculate_psa(mat, pops, groups, alleles)
```

## Arguments

- mat:

  Numeric matrix or data frame of genetic data.

- pops:

  Population identifier for each row of `mat`.

- groups:

  Group identifier for each row of `mat`.

- alleles:

  Character vector of allele column names to analyze.

## Value

A data frame with PSA values and the group of maximum PSA per allele.
