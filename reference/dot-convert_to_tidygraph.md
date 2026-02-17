# Convert Transitions Data to tidygraph Object

Internal helper function to convert various input formats to tidygraph
objects for network analysis and visualization.

## Usage

``` r
.convert_to_tidygraph(data, format, directed, min_weight)
```

## Arguments

- data:

  Input data (data.table or matrix)

- format:

  Format type

- directed:

  Whether graph should be directed

- min_weight:

  Minimum edge weight threshold

## Value

A tidygraph tbl_graph object
