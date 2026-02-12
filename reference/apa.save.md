# Save previously constructed APA table objects in a single .doc file

Save previously constructed APA table objects in a single .doc file

## Usage

``` r
apa.save(filename, ..., paper = "us")
```

## Arguments

- filename:

  Filename (e.g., my.tables.doc)

- ...:

  apaTable objects to be saved

- paper:

  Use "us" or "a4". Default is "us".

## Value

Save status

## Examples

``` r
library(apaTables)

table1 <- apa.1way.table(iv = dose, dv = libido,
               data = viagra, table.number = 1)

table2 <- apa.2way.table(iv1 = gender, iv2 = alcohol,
                         dv = attractiveness,
                         data = goggles, table.number = 1)

apa.save(filename = file.path(tempdir(), "my.tables.doc"), table1, table2)

# delete demo file
unlink(file.path(tempdir(), "my.tables.doc"))
```
