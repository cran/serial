#' Print method for serialConnection
#'
#' Outputs major information of the serial connection.
#' 
#' @param x \code{serialConnection} Object
#' @param ... not used
#'
#' @return nothing to return
#' 
#' @export
print.serialConnection <- function(x, ...)
{
  cat("Port",x$port,"(",x$mode,") is",ifelse(isOpen(x),"open","closed"))
  cat(".\n")
  cat(format("Buffer usage (in/out):",width = 18)
      ,ifelse(isOpen(x), paste(nBytesInQueue(x),collapse = "/"),"NA/NA"),"\n")
  warning("Try summary() for more information!")
}