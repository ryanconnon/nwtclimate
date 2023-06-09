% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/merge.R
\name{merge}
\alias{merge}
\title{Merge climate datasets together}
\usage{
merge(filepath, stitch_input_file, orig_input_file, output_file)
}
\arguments{
\item{filepath}{Character. The filepath to find input files and save output files}

\item{stitch_input_file}{Character. The file containing the stitched input file}

\item{orig_input_file}{Character. The file containing the original input file}

\item{output_file}{Character. The name of the final output file}
}
\value{
A dataframe of a merged dataset
}
\description{
Produce a map of all station data
}
