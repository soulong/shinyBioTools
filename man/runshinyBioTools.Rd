% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/runshinyBioTools.R
\name{runshinyBioTools}
\alias{runshinyBioTools}
\title{start shinyBioTools}
\usage{
runshinyBioTools(port = 5001)
}
\arguments{
\item{port}{run app on localhost, define a port, default port is 5001}
}
\value{
a shiny web app
}
\description{
start shinyBioTools by initialize a shiny app
}
\examples{
libray("shinyBioTools")
runshinyBioTools(port=5001)
}
