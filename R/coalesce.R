#' Coalesce two R objects
#'
#' I first saw this function in
#' \href{https://youtu.be/7oyiPBjLAWY}{a talk by Jenny Bryan at UseR! 2018}.
#' I hear it referred to as "coalesce", most likely named after the SQL
#' function. I later found it out it was actually included in `rlang`, but I
#' tend to keep my own implementation of the function (and its unit tests)
#' rather than introduce a dependency.
#'
#' @param x Any R object, possibly `NULL`
#' @param y Any R object, possibly `NULL`
#
#' @export
#'
#' @examples
#' 3 %||% NULL
#' NULL %||% "red panda"
#' NULL %||% NULL %||% 42
#'
#' @name coalesce
#' @rdname coalesce
`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}
