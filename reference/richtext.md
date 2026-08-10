# Construct a richtext value

A richtext value is either a length-1 character (plain) or a list of
styled runs created with \[rt_run()\]. It is the renderer-neutral
styling layer.

## Usage

``` r
richtext(x)
```

## Arguments

- x:

  a length-1 character, or a list of runs.

## Value

an object of class \`apa_richtext\`.
