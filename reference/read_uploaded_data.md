# Read uploaded data

Reads an uploaded CSV, XLS, or XLSX file using the same parsing rules as
the original Shiny application. CSV files are read with semicolon
separators.

## Usage

``` r
read_uploaded_data(path, filename)
```

## Arguments

- path:

  Character string. Path to the temporary uploaded file.

- filename:

  Character string. Original uploaded file name.

## Value

A data frame.
