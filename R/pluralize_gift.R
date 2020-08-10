#' Takes a noun and makes it plural
#'
#' @param gift A string or vector of strings
#'
#' @return A string or vector of strings with the pluralized words
#'
#' @import stringr
#' @import dplyr
#' @import glue
#' @import purrr
#'
#' @export
pluralize_gift <- function(gift) {
  special1 <- gift %>% str_detect("oo")
  special2 <- gift %>% str_detect("y$")
  len <- 2:length(gift)

  gift[len] <- paste0(gift[len], "s")
  gift[special1] <- gift[special1] %>%
    str_replace("oo", "ee") %>%
    str_replace("s$", "")
  gift[special2] <- gift[special2] %>%
    str_replace("s$", "") %>%
    str_replace("y$", "ies")

  return(gift)
}
