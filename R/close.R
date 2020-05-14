#' Function to close an serial interface.
#' 
#' This function closes the corresponding connection.
#' 
#' @method close serialConnection
#' 
#' @param con serial connection
#' @param ... is ignored
#' 
#' @seealso \code{\link{serialConnection}}
#' @import tcltk
#' @export
close.serialConnection<-function(con, ...)
{
  if(isOpen(con))
  {
    tryCatch({
      .Tcl( paste( 'flush ${sdev_', con$var,'}', sep = '' ) )
      .Tcl( paste( 'close ${sdev_', con$var,'}', sep = '' ) )
    } 
    ,error = function(e) stop(simpleError(e$message))
    )
    .Tcl( paste( 'unset sdev_', con$var, sep = '' ) )
  }
  invisible('DONE')
}
