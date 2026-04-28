# Build the PSA and R summary table

Combines private/shared allele summaries by group with R coefficients
and computes conservation priority scores.

## Usage

``` r
make_psa_r_table(group_summary, rtable)
```

## Arguments

- group_summary:

  A data frame with PSA results by allele and group.

- rtable:

  A data frame with R values by group.

## Value

A data frame with PSA, R, relative R, and priority scores.
