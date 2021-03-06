% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rtPCR.R
\name{rtPCR}
\alias{rtPCR}
\title{rtPCR result analysis}
\usage{
rtPCR(data, choose_samples, choose_targets, na.do, real_file, skip_row)
}
\arguments{
\item{data}{read expreesion CT values which is exported from Quantistudio}

\item{choose_samples}{choose which samples need to be analysed}

\item{choose_targets}{choose which genes need to be analysed}

\item{na.do}{how to deal with Underteriminated genes}

\item{real_file}{excel file path, used to extract merlting curve info}

\item{skip_row}{skip lines when reading \emph{real_file}}
}
\value{
a list contain multiple info
}
\description{
calculate real-time generated .xls file, both delta-delta-CT and merting curve
}
