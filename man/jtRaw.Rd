% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/jtRaw.R
\name{jtRaw}
\alias{jtRaw}
\title{Sales data collection}
\usage{
jtRaw(file = input$raw_file)
}
\arguments{
\item{file}{list with stores datapath element containing the path to the file}
}
\value{
Returns table with original data structure
}
\description{
Function to read the data from the file / only shiny
}
\examples{
jtRaw(input$raw_file)

To get the data in the local session use `data/przyklad1.csv`
from `joint-hubs/sales`. Find more details in `joint-hubs/sales/getappdata.r`

table_jtRaw <- jtRaw(input$raw_file)

table_readcsv <- read.csv("../data/przyklad1.csv",
                  stringsAsFactors = FALSE) \%>\%
                  janitor::clean_names()

identical(table_jtRaw, table_readcsv)
[1] TRUE

}
\keyword{collection}
\keyword{data}
