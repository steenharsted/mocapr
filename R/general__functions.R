#' Swap axis suffixes in column names
#'
#' Swaps column name suffixes, typically used for motion capture axes (e.g., X <-> Z).
#'
#' @param df A data frame or tibble with column names ending in axis suffixes.
#' @param axis1 A string. First axis suffix to swap (e.g., "X").
#' @param axis2 A string. Second axis suffix to swap (e.g., "Z").
#'
#' @return A tibble with renamed columns.
#' @export
#'
#' @examples
#' # df <- tibble("LHX" = 1, "LHZ" = 2)
#' # swap_axis(df, "X", "Z")
swap_axis <- function(df, axis1, axis2) {
  stopifnot(is.data.frame(df))

  # Temporary tags to avoid overwrite
  temp_tag <- paste0("___TEMP_", axis1, "___")

  names(df) <- names(df) |>
    stringr::str_replace(paste0(axis1, "$"), temp_tag) |>
    stringr::str_replace(paste0(axis2, "$"), axis1) |>
    stringr::str_replace(temp_tag, axis2)

  return(df)
}
