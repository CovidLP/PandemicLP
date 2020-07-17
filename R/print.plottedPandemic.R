#' @export
print.plottedPandemic = function(object){
  print(object$long)
  print(object$short)

  invisible(object)
}
