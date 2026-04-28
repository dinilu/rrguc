# Run the RGUC genetic conservation analysis

Performs the global and, when groups are supplied, group-level genetic
conservation analysis used by the application.

## Usage

``` r
RGUC(mat, pops, allele_perc = 0.1, pop_perc = 0.1, Fst = 0.099, groups = NULL)
```

## Arguments

- mat:

  Numeric matrix or data frame of genetic data.

- pops:

  Population identifier for each row of `mat`.

- allele_perc:

  Maximum overall allelic frequency.

- pop_perc:

  Maximum frequency across populations.

- Fst:

  FST value used to calculate the probability of retaining rare alleles.

- groups:

  Optional group identifier for each row of `mat`.

## Value

A list with global results, group results, PSA results, and needed
populations.
