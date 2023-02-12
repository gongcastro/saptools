# run the targets workflow
# make <- function() {
#   job(
#     {
#       tar_make()
#       export("none") # return nothing
#     },
#     import = NULL,
#     title = "Trajectories"
#   )
# }

#' Remove targets products
#'
#' @param keep_fits Should RDS object associated with model fits be kept?
#' @export
unmake <- function(keep_fits = TRUE) {
  path <- "results/fit.rds"
  tar_destroy(ask = FALSE)

  if (!keep_fits) {
    filenames <-
      list.files("results",
        pattern = "fit",
        full.names = TRUE
      )
    if (length(filenames > 0)) {
      lapply(filenames, file.remove)
    }
  }

  cli_alert_success("Removed project outputs")
}
