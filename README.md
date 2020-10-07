
<!-- README.md is generated from README.Rmd. Please edit that file -->

# statgl

<!-- badges: start -->

<!-- badges: end -->

Internal R package of Statistics Greenland. Consists of extensions for
ggplot2 themes, with plans to include miscellaneous tools for table
output, tools for data wrangling, and API functions for the
[Statbank](https://bank.stat.gl)

This is a development version; expect things to change.

## Installation

The development version of `statgl` is available on
[GitHub](https://github.com/). To install, run:

``` r
# install.packages("devtools")
devtools::install_github("StatisticsGreenland/statgl")
```

At the moment, this package only has extension functions for `ggplot2`,
providing themes and color palettes that follow the company design
manual.

The color functions are made by following this great blog post by [Simon
Jackson](https://drsimonj.svbtle.com/creating-corporate-colour-palettes-for-ggplot2).

## Example

Add `+ theme_statgl()` to a ggplot object, to provide a custom theme:

``` r
library(ggplot2)
library(statgl)
library(dplyr)

ex_plot <- 
  mpg %>% 
  ggplot(aes(x = displ, y = hwy, color = class)) + 
  geom_point()

ex_plot + theme_statgl()
```

<img src="man/figures/README-theme_example-1.png" width="100%" />

Add `+ scale_color_statgl()` ot a ggplot object to provide custom color
scale:

``` r
ex_plot + theme_statgl() + scale_color_statgl()
```

<img src="man/figures/README-color_example-1.png" width="100%" />

To provide a fill palette, use `+ scale_fill_statgl()` instead:

``` r
ex_plot <- mpg %>% ggplot(aes(x = class, fill = class)) + geom_bar()

ex_plot + theme_statgl() + scale_fill_statgl()
```

<img src="man/figures/README-fill_example-1.png" width="100%" />
