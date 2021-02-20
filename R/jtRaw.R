#' Sales data collection
#'
#' Function to read the data from the file / only shiny
#' @param file list with stores datapath element containing the path to the file
#' @keywords data collection
#' @return Returns table with original data structure
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
