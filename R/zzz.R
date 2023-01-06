#' fndat : Data research and organization for First Nations in Canada
#'
#' @docType package
#' @name fndat
#'
# importFrom

NULL

# ------------------------------------------------------------------------------
# Timestamp
timestamp <- function() format(Sys.time(), format = "%Y-%m-%d")

# ------------------------------------------------------------------------------
# Message
msgInfo <- function(..., appendLF = TRUE) {
  txt <- paste(cli::symbol$info, ...)
  message(crayon::green(txt), appendLF = appendLF)
  invisible(txt)
}

# ------------------------------------------------------------------------------
# Params 
global_param <- function() {
  assign(x = "param",
         value = yaml::read_yaml("./data/data-config/global_parameters.yml"),
         envir = globalenv())
}
 
# ------------------------------------------------------------------------------
#' Check if folder exists and create if not
chk_create <- function(path) {
  if (!file.exists(path)) dir.create(path, recursive = TRUE)
}