# Create a column mapping table

Creates the mapping between user-selected column names and the
standardized internal names used by the application.

## Usage

``` r
make_column_map(
  pop_col,
  group_col,
  sample_col = NULL,
  lon_col = NULL,
  lat_col = NULL
)
```

## Arguments

- pop_col:

  Character string. Population column name.

- group_col:

  Character string. Group column name.

- sample_col:

  Optional character string. Sample column name, or `None`.

- lon_col:

  Optional character string. Longitude column name, or `None`.

- lat_col:

  Optional character string. Latitude column name, or `None`.

## Value

A data frame with original and standardized column names.
