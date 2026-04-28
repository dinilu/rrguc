# Standardize uploaded data columns

Renames selected metadata columns to the standard names used internally
by the application and keeps numeric genetic columns unchanged.

## Usage

``` r
standardize_uploaded_data(dta, col_map)
```

## Arguments

- dta:

  A data frame with uploaded data.

- col_map:

  A data frame produced by
  [`make_column_map()`](https://dnietolugilde.com/rrguc/reference/make_column_map.md)
  with columns `col_names` and `standard_col_names`.

## Value

A standardized data frame.
