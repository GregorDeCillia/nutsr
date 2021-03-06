% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nuts_drilldown.R
\name{nuts_drilldown}
\alias{nuts_drilldown}
\title{Create a drilldown plots from iso5 data}
\usage{
nuts_drilldown(x, title = "Chart for ${name}", value_label = NULL)
}
\arguments{
\item{x}{a data frame. The first column should contain valid iso5 ids
and the second column values.}

\item{title}{a chart title}

\item{value_label}{a label for the value. Used in the tooltip. If missing,
it will be extracted from the column name}
}
\value{
a htmlwidget object from the highcharter package
}
\description{
Create a drilldown plots from iso5 data
}
\examples{
nuts_drilldown(iso5_pop())
}
