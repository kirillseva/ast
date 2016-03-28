deparse_tree <- function(x) {
  if (is.atomic(x) || is.name(x)) {
    x
  } else if (is.call(x) || is.pairlist(x)) {
    `class<-`(lapply(x, deparse_tree), 'ast_node')
  } else {
    stop("Don't know how to handle type ", typeof(x),
      call. = FALSE)
  }
}
