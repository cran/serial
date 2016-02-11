#' Wirtes data to serial interface. 
#' 
#' @param con serial Connection
#' @param dat data string to write on the serial interface. At the moment this 
#'            must be a string \code{"..."}. See examle section in \code{\link{serial}}.
#' 
#' @usage write.serialConnection(con,dat)
#' @seealso \code{\link{serial}}
#' @examples
#'  # See the top package documentation
#'  
#'  \dontrun{write.serialConnection(con, "Hello World!")}
#' @export
write.serialConnection<-function(con,dat)
{
  nl <- "-nonewline "
  if(con$newline) nl <- ""
  
  try( .Tcl( paste("puts ",nl,"$sdev_",con$port, " \"", dat,"\"", sep="")) )
  
  #   ..," \"", dat,"\"",.. -> quotes dat in TCL String
  #   with out quoting space and control characters this will fail 
  
  invisible("DONE")
}

#' Reads from the serial interface.
#' 
#' This function reads from the serial interface as long as the buffer is not
#' empty. The read takes place perbyte.
#' 
#' 
#' @param con serial connection
#' 
#' @usage read.serialConnection(con)
#' 
#' @return The result is a string, which can be converted to raw as necessary

#' @seealso \code{\link{serial}}
#' @examples
#'  # See the top package documentation
#' @export
read.serialConnection<-function(con)
{
  res<-""
  while(1)
  {
    tmp <- try(tclvalue( .Tcl( paste("gets $sdev_",con$port, sep=""))))
    if(tmp=="") break
    res<-paste(res,tmp,sep="")
  }
  return(res)
}