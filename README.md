# harvester: command-line folding tool

`harvester [expr]`

reads space-separated lists of numbers from the standard input, and prints the result.

## Examples

```
harvester len 'sum $1'
```

Returns the length and the sum of the first column.

```
harvester 'dist (bucket 0.1 0.0 $1) (sum ($2 * $3) / sum $2)'
```

Calculates an average of the third column weighted by the second column, for each interval.
