# Check whether a data frame contains valid geographic coordinates

Tests whether both `lat` and `lon` columns are present and whether at
least one row contains numeric, non-missing values for both coordinates.

## Usage

``` r
has_valid_coordinates(dta)
```

## Arguments

- dta:

  A data frame containing uploaded or standardized data.

## Value

A logical value.
