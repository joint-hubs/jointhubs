#' Sales data collection
#'
#' Function to read the data from the file / only shiny
#' @param file list with stores datapath element containing the path to the file
#' @keywords data collection
#' @return Returns table with original data structure
#'
#' @examples jtRaw(input$raw_file)
#'
#' To get the data in the local session use `data/przyklad1.csv`
#' from `joint-hubs/sales`. Find more details in `joint-hubs/sales/getappdata.r`
#'
#' table_jtRaw <- jtRaw(input$raw_file)
#'
#' table_readcsv <- read.csv("../data/przyklad1.csv",
#'                   stringsAsFactors = FALSE) %>%
#'                   janitor::clean_names()
#'
#' identical(table_jtRaw, table_readcsv)
#' [1] TRUE
#'
#' @export
#'

jtRaw <- function(file = input$raw_file){

    ext <- tools::file_ext(file$datapath)
  req(file)
  validate(need(ext == "csv", "Please upload a csv file"))

  table <- read.csv(file$datapath,
                    stringsAsFactors = FALSE) %>%
    janitor::clean_names()

  return(table)
}
