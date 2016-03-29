print.ast_node <- function(x, ..., offset = 0) {
  for (i in seq(offset)) cat(' ')
  cat('AST node of type', crayon::yellow(as.character(ast$root(x))), 'and length', length(x), '\n')
  lapply(children(x), function(x) {
    if (is.ast_node(x)) { cat('  '); print(x, offset = offset + 2) }
    else { for (i in seq(offset)) cat('  '); print(x) }
  })
}

root <- function(x) {
  if (length(x) > 0) { x[[1]] } else { x }
}

children <- function(x) {
  if (length(x) > 0) { x[-1] } else { NULL }
}

has_children <- function(x) {
  length(children(x)) > 0
}

is_terminal <- function(x) {
  Negate(any)(sapply(children(x), is.ast_node))
}

set_root <- function(x, new_root) {
  stopifnot(class(root(x)) == class(new_root))
  if (length(x) > 0) { x[[1]] <- new_root; x } else { new_root }
}

compile <- function(x) {
  if (is.ast_node(x)) {
    as.call(append(
      list(x[[1]]),
      lapply(children(x), compile)
    ))
  } else { x }
}
