#' Reads out the number of characters / bytes pending in the input and output buffer
#' 
#' @param con serial connection
#' 
#' @return named vector with number of bytes
#' @seealso \code{\link{serial}}
#' @examples
#'  # See the top package documentation
#' @export
nBytesInQueue <- function(con)
{
  if(!isOpen(con))
    stop(simpleError(paste(con$port,'is not open!')))
  
  tryCatch({
    tmp <- tclvalue(.Tcl( paste( 'fconfigure ${sdev_', con$var,'} -queue', sep = '' ) ) )
  } 
  ,error = function(e) stop(simpleError(e$message))
  )
  
  res <- as.numeric(strsplit(tmp," ")[[1]])
  names(res) <- c("n_in","n_out")
  return(res)
}