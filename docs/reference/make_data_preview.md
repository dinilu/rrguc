# Create a limited data preview

Returns the first rows and columns of a data frame for display in the
Shiny application.

## Usage

``` r
make_data_preview(dta, max_rows = 50, max_cols = 100)
```

## Arguments

- dta:

  A data frame.

- max_rows:

  Maximum number of rows to include.

- max_cols:

  Maximum number of columns to include.

## Value

A data frame subset.
