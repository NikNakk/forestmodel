`%||%` <- function(x, y) if (is.null(x)) y else x

#' @import rlang
default_cols <- function(.data, ...) {
  dots <- list2(...)
  if (is.null(names(dots)) || any(names(dots) == "")) {
    stop("All parameters (except `.data`) must be named")
  }
  for (cn in names(dots)) {
    if (!has_name(.data, cn)) {
      .data[[cn]] <- eval_tidy(dots[[cn]], .data)
    }
  }
  .data
}
