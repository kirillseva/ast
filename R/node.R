print.ast_node <- function(x, ...) {
  cat('AST node of type', crayon::yellow(as.character(ast$root(x))), 'and length', length(x), '\n')
}

root <- function(x) {
  if (length(x) > 0) { x[[1]] } else { x }
}

children <- function(x) {
  if (length(x) > 0) { x[-1] } else { NULL }
}

is_terminal <- function(x) {
  tryCatch(length(x[-1]) == 0, error = function(e) TRUE)
}
