% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-style.R
\name{font_style}
\alias{font_style}
\title{Construct a vector of styles related to 'font'}
\usage{
font_style(state, geom)
}
\arguments{
\item{state}{list including 'gc' (graphics context)}

\item{geom}{which geometry has asked for a style}
}
\value{
character vector of multiple "{attr_name}: {value}" strings for
        'font-size', 'font-family', 'fill'
}
\description{
TODO: add font choice and style
}
