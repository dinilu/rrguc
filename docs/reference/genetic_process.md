# Process a complete genetic analysis

Main wrapper used by the Shiny application to run the RGUC workflow and
format the objects required by the outputs.

## Usage

``` r
genetic_process(
  matriz,
  pop_info,
  allele_perc = 0.1,
  pop_perc = 0.1,
  Fst = 0.099
)
```

## Arguments

- matriz:

  Numeric matrix or data frame of genetic data.

- pop_info:

  Data frame containing at least a `pop` column and optionally a `group`
  column.

- allele_perc:

  Maximum overall allelic frequency.

- pop_perc:

  Maximum frequency across populations.

- Fst:

  FST value used to calculate the probability of retaining rare alleles.

## Value

A list with selected alleles, plots, R table, PSA summary, and needed
populations.
