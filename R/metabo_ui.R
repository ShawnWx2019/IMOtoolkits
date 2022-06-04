#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom bslib bs_theme
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyjqui jqui_resizable
#' @importFrom colourpicker colourInput
#' @importFrom thematic thematic_shiny
#' @importFrom DT dataTableOutput
#' @noRd


# bslib theme -------------------------------------------------------------







#' # add external resources ---------------------------------------------------------------------
#'
#'
#' #' Add external Resources to the Application
#' #'
#' #' This function is internally used to add external
#' #' resources inside the Shiny application.
#' #'
#' #' @import shiny
#' #' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' #' @noRd
#' golem_add_external_resources <- function() {
#'   add_resource_path(
#'     "www",
#'     app_sys("app/www")
#'   )
#'
#'   tags$head(
#'     favicon(),
#'     bundle_resources(
#'       path = app_sys("app/www"),
#'       app_title = "IMOtoolkits"
#'     )
#'     # Add here other external resources
#'     # for example, you can add shinyalert::useShinyalert()
#'   )
#' }
