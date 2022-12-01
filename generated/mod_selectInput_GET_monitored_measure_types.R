#' selectInput_GET_monitored_measure_types UI Function
#'
#' @description A shiny::selectInput wrapper around the deepomics' API GET monitored_measure_types call.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @export
#' @param id Environment id of the module
#' @importFrom shiny NS tagList uiOutput
mod_selectInput_GET_monitored_measure_types_ui <- function(id){
  ns <- NS(id)
  tagList(
    shiny::uiOutput(NS(id, "choix"))
  )
}
    
#' selectInput_GET_monitored_measure_types Server Functions
#'
#' @export
#' @param id Internal parameter for {shiny}.
#' @param authentification_module Authentification module from this package
#' @param api_function_options List of options to pass to GET /monitored_measure_types as query parameters
#' @param widget_options List of options to pass to \code{\link[shiny]{selectInput\}\}
#' @return A named list with various reactive values
#' \describe{
#'  \item{input}{The module input, to be used for binding events}
#'  \item{options}{The options used to call GET /monitored_measure_types
#'  \item{selected}{The enitire row selection from result_df of the shiny::selectInput widget }
#'  \item{choices}{A named list containing the selection choices} 
#'  \item{result_df}{A dataframe containing the results of GET /monitored_measure_types call}
#' }
#' @importFrom shiny reactive renderUI
#' @importFrom stats setNames
mod_selectInput_GET_monitored_measure_types_server <- function(id,
                                       authentification_module,
                                       api_function_options = list(),
                                       widget_options = list()) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Function to call when wanted actual values, in a reactive context
    function_options_reactive <- compute_reactive_in_list(api_function_options)
    widget_options_reactive <- compute_reactive_in_list(widget_options)

    result_df <- reactiveVal(data.frame())
    itemList <- reactiveVal(list())
    final_options <- reactiveVal()

    output$choix <- shiny::renderUI({
      # Compute reactive
      host <- authentification_module$host()
      token <- authentification_module$token()

      final_options(function_options_reactive())

      # Custom code
      # This is where the code should come from a R deepomics package
      call1 <-
        paste0(
          host,
          "/",
          "monitored_measure_types",
          parse_query_parameters(
            js_spread(
              list(
                pageSize = 10000
              ),
              final_options())
          )
        )

      get_result <- parse_status(
        # This request is a GET request
        httr::GET(call1, httr::add_headers(Authorization = token))
      )
      get_result_text <- httr::content(get_result, "text")

      label <- 'Choose an monitored measure type:'
      final_widget_options <- widget_options_reactive()
      final_widget_options[['inputId']] = ns("choix")
      get_result_json <- jsonlite::fromJSON(get_result_text, flatten = TRUE)
      result_df_static <- get_result_json[['hydra:member']]
      result_df(result_df_static)

      itemListStatic <- setNames(result_df_static[["@id"]], result_df_static$slug)
      # To fix:
      # Whole logic should be put outside or render function
      # So we can hide the UI is required
      if (is.null(itemListStatic)) {
        itemListStatic <- list()
      }
      itemList(itemListStatic)
      # If the user specifies thoses values, we keep item
      # This allow easy use of the module by hiding its UI
      if (!('label' %in% names(final_widget_options))) {
        final_widget_options[['label']] = label
      }
      if (!('choices' %in% names(final_widget_options))) {
        final_widget_options[['choices']] = itemListStatic
      }
      if (isTruthy(final_widget_options$multiple)) {
        label <- 'Choose one or more monitored measure type(s):'
      }
      do.call(shiny::selectInput, final_widget_options)
    })

    selected <- shiny::reactive({
          widget_options_reactive <- compute_reactive_in_list(widget_options)
          final_widget_options <- widget_options_reactive()
          isMultiple <- FALSE
          if (isTruthy(final_widget_options$multiple)) {
            isMultiple <- final_widget_options$multiple
          }
          out <- NA
          if (!isMultiple) {
            choice <- which(input$choix == itemList())
            if (!is.null(input$choix)
                && input$choix %in% itemList()
                && length(itemList()) > 0
                && choice != 0) {
              out <- result_df()[choice, ]
            }
          } else {
            choices <- which(input$choix %in% itemList())
            if (!is.null(input$choix)
                && all(input$choix %in% itemList())
                && length(itemList()) > 0
            ) {
              out <- result_df()[choices, ]
            }
          }
          return(out)
        })

    return(
      list(
        input = input,
        options = final_options,
        selected = selected,
        choices = itemList,
        result_df = result_df
      )
    )
  })
}