# Select low-frequency alleles

Selects alleles according to maximum individual allele frequency and
maximum population frequency thresholds.

## Usage

``` r
select_alleles(mat, pops, allele_perc = 0.1, pop_perc = 0.1)
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

## Value

A character vector with selected allele names.
