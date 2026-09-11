#' Calculate Enneagram Wing
#'
#' Determines the wing of an Enneagram type based on the counts of its neighbors.
#' The wing is the neighbor with the higher count. In case of a tie, the second
#' neighbor is chosen.
#'
#' @param main_number The primary Enneagram type (1-9).
#' @param praxis A data frame containing at least `number` and `count` columns.
#' @return The number of the wing type (1-9).
calculate_wing <- function(main_number, praxis) {
  if (main_number == 9) {
    wing1 <- 8
    wing2 <- 1
  } else if (main_number == 1) {
    wing1 <- 9
    wing2 <- 2
  } else {
    wing1 <- main_number - 1
    wing2 <- main_number + 1
  }

  count1 <- praxis$count[praxis$number == wing1]
  count2 <- praxis$count[praxis$number == wing2]

  if (count1 > count2) {
    wing <- wing1
  } else {
    wing <- wing2
  }

  return(wing)
}
