#' Resolve NAMESPACE conflicts
#'
#' @export
resolve_conflicts <- function() {
  suppressMessages({
    conflict_prefer("last_warnings", "rlang")
    conflict_prefer("filter", "dplyr")
    conflict_prefer("between", "dplyr")
    conflict_prefer("timestamp", "utils")
    conflict_prefer("ar", "brms")
    conflict_prefer("chisq.test", "stats")
    conflict_prefer("discard", "scales")
    conflict_prefer("duration", "lubridate")
    conflict_prefer("fisher.test", "stats")
    conflict_prefer("lag", "dplyr")
  })
  cli_alert_success("Resolved conflicts")
}
