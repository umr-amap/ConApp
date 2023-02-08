
#' Data Validation Module
#'
#' @param id Module's ID.
#'
#' @export
#'
#' @return
#'  * UI: HTML tags that can be included in the UI part of the application.
#'  * Server: a [shiny::reactive()] function returning a `data.frame`.

#'
#' @name module-data-validation
#'
#' @importFrom shiny NS
#' @importFrom htmltools tagList
data_count_taxa_ui <- function(id) {
  ns <- NS(id)
  tagList(
    # datamods::validation_ui(ns("result"), display = "inline"),
    uiOutput(outputId = ns("alert_result"))
  )
}

#' @param data_r A `reactive` function returning a `data.frame`.
#'
#' @export
#'
#' @rdname module-data-validation
#'
#' @importFrom shiny moduleServer observeEvent reactive req actionLink downloadLink
data_count_taxa_server <- function(id, data_r = reactive(NULL)) {
  moduleServer(
    id = id,
    module = function(input, output, session) {
      
      to_validate_r <- reactive({
        req(data_r())
        
        data_r <- data_r()
        
        count_tax <- 
          data_r %>% 
          dplyr::group_by(.__taxa) %>% 
          dplyr::count() %>% 
          filter(n <= 30)
          
        data_r_filter <- data_r %>% 
          dplyr::filter(.__taxa %in% count_tax$.__taxa)
        
        data_r_filter

      })
      
      
      output$alert_result <- renderUI({
        
        data_r <- data_r()
        to_validate <- to_validate_r()
        nbe_rows_excluded <- nrow(data_r) - nrow(to_validate)
        nbe_tax_excluded <- length(unique(data_r$.__taxa)) - length(unique(to_validate$.__taxa))
        nbe_tax_left <- length(unique(to_validate$.__taxa))
        nbe_tax_init <- length(unique(data_r$.__taxa))
        
        if (nrow(to_validate) < 1) {
          shinyWidgets::alert(
            status = "danger",
            ph("warning"), "There are not enough rows to proceed after filtering out taxa with many records"
          )
        } else {
          shinyWidgets::alert(
            status = "info",
            ph("warning"),
            paste(nbe_tax_init, "taxa in the whole dataset"),
            ph("trash"),
            paste(nbe_tax_excluded, "taxa have been excluded because there are represented by too many records"),
            tags$br(),
            ph("trash"),
            paste(nbe_rows_excluded, "row(s) have been excluded"),
            ph("arrow-right"),
            paste(nbe_tax_left, "taxa left to be assessed")
          )
        }
      })
      
      return(to_validate_r)
    }
  )
}
