#' Flushes the connection.
#' 
#' Some times (and depending on buffering mode) the connection buffer needs 
#' to be flushed manually. This command empties the buffer by sending all 
#' remaining bytes.
#' 
#' @param con serial connection
#' 
#' @method flush serialConnection
#' 
#' @return Nothing is returned
#' @seealso \code{\link{serial}}
#' @examples
#'  # See the top package documentation
#' @export
flush.serialConnection <- function(con)
{
  if(!isOpen(con))
    stop(simpleError(paste(con$port,'is not open!')))
  
  tryCatch({
    .Tcl( paste( 'flush ${sdev_', con$var,'}', sep = '' ) )
  } 
  ,error = function(e) stop(simpleError(e$message))
  )
  
  invisible('DONE')
}
