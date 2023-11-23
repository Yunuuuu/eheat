
<!-- README.md is generated from README.Rmd. Please edit that file -->

# eheat

<!-- badges: start -->

<!-- badges: end -->

This package serves as a bridge between the ggplot2 and ComplexHeatmap
packages. Essentially, all ggplot2 geometries and operations can be
utilized in ComplexHeatmap through the `eheat` package, with the
exception of facet operations (and you shouldn’t do it in `eheat`
package). Fortunately, ComplexHeatmap is capable of handling these
operations independently, rendering them unnecessary.

## Installation

You can install the development version of `eheat` from
[GitHub](https://github.com/) with:

``` r
if (!requireNamespace("pak")) {
  install.packages("pak",
    repos = sprintf(
      "https://r-lib.github.io/p/pak/devel/%s/%s/%s",
      .Platform$pkgType, R.Version()$os, R.Version()$arch
    )
  )
}
pak::pkg_install("Yunuuuu/eheat")
```

``` r
library(eheat)
library(ggplot2)
library(ComplexHeatmap)
#> Loading required package: grid
#> ========================================
#> ComplexHeatmap version 2.15.4
#> Bioconductor page: http://bioconductor.org/packages/ComplexHeatmap/
#> Github page: https://github.com/jokergoo/ComplexHeatmap
#> Documentation: http://jokergoo.github.io/ComplexHeatmap-reference
#> 
#> If you use it in published research, please cite either one:
#> - Gu, Z. Complex Heatmap Visualization. iMeta 2022.
#> - Gu, Z. Complex heatmaps reveal patterns and correlations in multidimensional 
#>     genomic data. Bioinformatics 2016.
#> 
#> 
#> The new InteractiveComplexHeatmap package can directly export static 
#> complex heatmaps into an interactive Shiny app with zero effort. Have a try!
#> 
#> This message can be suppressed by:
#>   suppressPackageStartupMessages(library(ComplexHeatmap))
#> ========================================
```

Let’s begin by creating some example data, following code was copied
from ComplexHeatmap book directly

``` r
set.seed(123)
nr1 <- 4
nr2 <- 8
nr3 <- 6
nr <- nr1 + nr2 + nr3
nc1 <- 6
nc2 <- 8
nc3 <- 10
nc <- nc1 + nc2 + nc3
mat <- cbind(
  rbind(
    matrix(rnorm(nr1 * nc1, mean = 1, sd = 0.5), nr = nr1),
    matrix(rnorm(nr2 * nc1, mean = 0, sd = 0.5), nr = nr2),
    matrix(rnorm(nr3 * nc1, mean = 0, sd = 0.5), nr = nr3)
  ),
  rbind(
    matrix(rnorm(nr1 * nc2, mean = 0, sd = 0.5), nr = nr1),
    matrix(rnorm(nr2 * nc2, mean = 1, sd = 0.5), nr = nr2),
    matrix(rnorm(nr3 * nc2, mean = 0, sd = 0.5), nr = nr3)
  ),
  rbind(
    matrix(rnorm(nr1 * nc3, mean = 0.5, sd = 0.5), nr = nr1),
    matrix(rnorm(nr2 * nc3, mean = 0.5, sd = 0.5), nr = nr2),
    matrix(rnorm(nr3 * nc3, mean = 1, sd = 0.5), nr = nr3)
  )
)
#> Warning in matrix(rnorm(nr1 * nc1, mean = 1, sd = 0.5), nr = nr1): partial
#> argument match of 'nr' to 'nrow'
#> Warning in matrix(rnorm(nr2 * nc1, mean = 0, sd = 0.5), nr = nr2): partial
#> argument match of 'nr' to 'nrow'
#> Warning in matrix(rnorm(nr3 * nc1, mean = 0, sd = 0.5), nr = nr3): partial
#> argument match of 'nr' to 'nrow'
#> Warning in matrix(rnorm(nr1 * nc2, mean = 0, sd = 0.5), nr = nr1): partial
#> argument match of 'nr' to 'nrow'
#> Warning in matrix(rnorm(nr2 * nc2, mean = 1, sd = 0.5), nr = nr2): partial
#> argument match of 'nr' to 'nrow'
#> Warning in matrix(rnorm(nr3 * nc2, mean = 0, sd = 0.5), nr = nr3): partial
#> argument match of 'nr' to 'nrow'
#> Warning in matrix(rnorm(nr1 * nc3, mean = 0.5, sd = 0.5), nr = nr1): partial
#> argument match of 'nr' to 'nrow'
#> Warning in matrix(rnorm(nr2 * nc3, mean = 0.5, sd = 0.5), nr = nr2): partial
#> argument match of 'nr' to 'nrow'
#> Warning in matrix(rnorm(nr3 * nc3, mean = 1, sd = 0.5), nr = nr3): partial
#> argument match of 'nr' to 'nrow'
mat <- mat[sample(nr, nr), sample(nc, nc)] # random shuffle rows and columns
rownames(mat) <- paste0("row", seq_len(nr))
colnames(mat) <- paste0("column", seq_len(nc))
small_mat <- mat[1:9, 1:9]
```

The central functions of the `eheat` package are `ggheat` and `gganno`.
These two functions encompass all the necessary functionalities.
`ggheat` serves as a substitute for the `ComplexHeatmap::Heatmap`
function, while `gganno` replaces all the `anno_*` functions within the
ComplexHeatmap package, offering a comprehensive solution for our
requirements.

## ggheat

Using `ggheat`, it is effortless to create a simple Heatmap.

``` r
draw(ggheat(small_mat))
```

<img src="man/figures/README-unnamed-chunk-6-1.png" width="100%" />

You do not need to explicitly specify the color mapping as you can
utilize the `scale_*` function directly from ggplot2. All guide legends
will directly extracted from `ggplot2`. The essential parameter of
`ggheat` is `ggfn`, which accepts a ggplot2 object equipped with a
default data and mappings established via `ggplot(data,
aes(.data$.column, .data$.row))`. By default, the data includes six
columns, each prefixed with a dot for caution.

  - `.slice_row`: the slice row number

  - `.slice_column`: the slice column number

  - `.row` and `.column`: the row and column coordinates

  - `.row_index` and `.column_index`: the row and column index of the
    original matrix.

<!-- end list -->

``` r
draw(ggheat(small_mat, function(p) {
  p + scale_fill_viridis_c()
}))
```

<img src="man/figures/README-unnamed-chunk-7-1.png" width="100%" />

You can add more geoms.

``` r
draw(
  ggheat(small_mat, function(p) {
    p + geom_text(aes(label = sprintf("%d * %d", .row_index, .column_index)))
  })
)
```

<img src="man/figures/README-unnamed-chunk-8-1.png" width="100%" />

All the functionalities of the `ComplexHeatmap::Heatmap` function can be
used as is.

``` r
draw(ggheat(small_mat, function(p) {
  p + scale_fill_viridis_c()
}, column_km = 2L))
```

<img src="man/figures/README-unnamed-chunk-9-1.png" width="100%" />

``` r
draw(ggheat(small_mat, function(p) {
  p + scale_fill_viridis_c()
}, column_km = 2L, row_km = 3))
```

<img src="man/figures/README-unnamed-chunk-10-1.png" width="100%" />

The row names and column names should be controlled by the
`ComplexHeatmap::Heatmap` function, while the legends should be
controlled by `ggplot2`. However, the default legend name is taken from
`ComplexHeatmap::Heatmap` in order to maintain consistency.
Nevertheless, you can directly override it in `ggfn`.

``` r
draw(ggheat(small_mat, function(p) {
  p + scale_fill_viridis_c()
}, column_km = 2L, row_km = 3, row_names_gp = gpar(col = "red")))
```

<img src="man/figures/README-unnamed-chunk-11-1.png" width="100%" />

``` r
draw(
  ggheat(small_mat, function(p) {
    p + scale_fill_viridis_c()
  },
  column_km = 2L, row_km = 3, row_names_gp = gpar(col = "red"),
  name = "ComplexHeatmap"
  )
)
```

<img src="man/figures/README-unnamed-chunk-12-1.png" width="100%" />

``` r
draw(
  ggheat(small_mat, function(p) {
    p + scale_fill_viridis_c(name = "ggplot2")
  },
  column_km = 2L, row_km = 3, row_names_gp = gpar(col = "red"),
  name = "ComplexHeatmap"
  )
)
```

<img src="man/figures/README-unnamed-chunk-13-1.png" width="100%" />

## gganno

Both `gganno` and `gganno2` perform identical functions, but `gganno` is
not compatible with direct integration with `ComplexHeatmap::Heatmap`.
In such cases, only an empty annotation region can be added. On the
other hand, `gganno2` can be seamlessly combined with both
`ComplexHeatmap::Heatmap` and `ggheat`, although legends will not be
extracted.

The same with `ggheat`, the essential parameter of `gganno` is also
`ggfn`, which accepts a ggplot2 object equipped with a default data and
mappings established by `ggplot(data, aes(.data$x))`. The original
matrix will be converted into a data.frame with another 3 columns added:

  - `.slice`: the slice row (which = “row”) or column (which = “column”)
    number.

  - `.x`/`.y`: indicating the x-axis (or y-axis) coordinates. Don’t use
    `ggplot2::coord_flip` to flip coordinates as it may disrupt internal
    operations.

  - `.index`: denoting the row index of the original matrix, where rows
    are uniformly considered as observations and columns as variables.

In general, we should just use `ggheat` and `gganno`.

``` r
anno_data <- sample(1:10, nrow(small_mat))
draw(ggheat(small_mat,
  top_annotation = HeatmapAnnotation(
    foo = gganno(
      # Note: vector will be converted one-column data.frame
      # with a column names `V1`
      matrix = anno_data,
      function(p) {
        p + geom_point(aes(.x, V1))
      },
      which = "column"
    ), which = "column"
  )
))
#> ℹ convert simple vector `matrix` to one-column matrix
```

<img src="man/figures/README-unnamed-chunk-14-1.png" width="100%" />

Legends will also be extracted, in the similar manner like passing them
into `annotation_legend_list`.

``` r
draw(ggheat(small_mat,
  top_annotation = HeatmapAnnotation(
    foo = gganno(
      matrix = anno_data,
      function(p) {
        p + geom_bar(aes(y = V1, fill = factor(.index)), stat = "identity")
      },
      which = "column", height = unit(5, "cm")
    ), which = "column"
  )
), merge_legends = TRUE)
#> ℹ convert simple vector `matrix` to one-column matrix
```

<img src="man/figures/README-unnamed-chunk-15-1.png" width="100%" />

``` r
draw(ggheat(small_mat,
  top_annotation = HeatmapAnnotation(
    foo = gganno(
      matrix = anno_data,
      function(p) {
        p + aes(y = V1) + geom_text(aes(label = .index))
      },
      which = "column", height = unit(2, "cm")
    ), which = "column"
  ),
  bottom_annotation = HeatmapAnnotation(
    foo = gganno(
      function(p) {
        p + aes(y = V1) +
          geom_text(aes(label = .index))
      },
      matrix = anno_data,
      which = "column", height = unit(2, "cm")
    ),
    which = "column"
  ),
  right_annotation = HeatmapAnnotation(
    foo = gganno(
      function(p) {
        p + aes(x = V1) +
          geom_text(aes(label = .index))
      },
      matrix = anno_data,
      which = "row", width = unit(3, "cm")
    ),
    which = "row"
  ),
  left_annotation = HeatmapAnnotation(
    foo = gganno(
      function(p) {
        p + aes(x = V1) +
          geom_text(aes(label = .index))
      },
      matrix = anno_data,
      which = "row", width = unit(3, "cm")
    ),
    which = "row"
  ),
  row_km = 2L, column_km = 2L,
), merge_legends = TRUE)
#> ℹ convert simple vector `matrix` to one-column matrix
#> ℹ convert simple vector `matrix` to one-column matrix
#> ℹ convert simple vector `matrix` to one-column matrix
#> ℹ convert simple vector `matrix` to one-column matrix
```

<img src="man/figures/README-unnamed-chunk-16-1.png" width="100%" />

Finally, let’s see the difference between `gganno2` and `gganno`.

`gganno2` will not extract the legend.

``` r
anno_data <- sample(1:10, nrow(small_mat))
draw(ggheat(small_mat,
  top_annotation = HeatmapAnnotation(
    foo = gganno2(
      matrix = anno_data,
      function(p) {
        p + geom_bar(aes(y = V1, fill = factor(.index)), stat = "identity")
      },
      which = "column"
    ), which = "column"
  )
), merge_legends = TRUE)
#> ℹ convert simple vector `matrix` to one-column matrix
```

<img src="man/figures/README-unnamed-chunk-17-1.png" width="100%" />

But `gganno2` can work with `Heatmap` function.

``` r
anno_data <- sample(1:10, nrow(small_mat))
draw(Heatmap(small_mat,
  top_annotation = HeatmapAnnotation(
    foo = gganno2(
      matrix = anno_data,
      function(p) {
        p + geom_point(aes(.x, V1))
      },
      which = "column"
    ), which = "column"
  )
), merge_legends = TRUE)
#> ℹ convert simple vector `matrix` to one-column matrix
```

<img src="man/figures/README-unnamed-chunk-18-1.png" width="100%" />

`gganno` will just add a blank region in `Heatmap` function.

``` r
draw(Heatmap(small_mat,
  top_annotation = HeatmapAnnotation(
    foo = gganno(
      matrix = anno_data,
      function(p) {
        p + geom_bar(aes(y = V1, fill = factor(.index)), stat = "identity")
      },
      which = "column"
    ), which = "column"
  )
), merge_legends = TRUE)
#> ℹ convert simple vector `matrix` to one-column matrix
```

<img src="man/figures/README-unnamed-chunk-19-1.png" width="100%" />
