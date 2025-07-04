% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/statgl_plot.R
\name{statgl_plot}
\alias{statgl_plot}
\title{Create a highcharter plot with smart defaults}
\usage{
statgl_plot(
  df,
  x,
  y = value,
  type = "line",
  name = NULL,
  group = NULL,
  title = NULL,
  subtitle = NULL,
  caption = NULL,
  show_last_value = TRUE,
  xlab = "",
  ylab = "",
  tooltip = NULL,
  locale = "en"
)
}
\arguments{
\item{df}{A data frame.}

\item{x, }{y Bare column names for x and y aesthetics. \code{y} defaults to \code{value}.}

\item{type}{A string specifying the chart type (e.g., \code{"line"}, \code{"column"}, \code{"bar"}, \code{"scatter"}).}

\item{name}{An optional name for the series.}

\item{group}{A bare column name to split data into series (for grouped plots).}

\item{title, }{subtitle, caption Optional text elements to add to the chart.}

\item{show_last_value}{Logical; if \code{TRUE}, adds data labels to the last value of each line, or all bars in bar/column charts.}

\item{xlab, }{ylab Axis labels.}

\item{tooltip}{Optional JavaScript string for a custom tooltip formatter.}

\item{locale}{Locale code used to control number formatting (default is \code{"en"}; use \code{"da"} or \code{"kl"} for Danish/Greenlandic-style formatting).}
}
\value{
A \code{highchart} object.
}
\description{
A wrapper around \code{highcharter::hchart()} that makes it easy to create line, bar, and column plots with automatic titles, axis labels, tooltips, and localized number formatting.
If \code{show_last_value = TRUE}, it shows value labels on:
\itemize{
\item only the last point in each line (\code{type = "line"} or \code{"spline"})
\item all bars for \code{type = "bar"} or \code{"column"}
}
}
\examples{
statgl_plot(mtcars, x = wt, y = mpg, type = "line")
statgl_plot(mtcars, x = cyl, y = mpg, type = "column", show_last_value = TRUE)
statgl_plot(mpg, x = displ, y = hwy, group = class, type = "line", locale = "da")
}
