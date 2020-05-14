#' Function to initialize an serial interface.
#' 
#' This function initializes the serial interface and opens it for later usage. 
#' 
#' @method open serialConnection
#' 
#' @param con serial connection
#' @param ... is ignored
#' @seealso \code{\link{serialConnection}}
#' @import tcltk
#' @export
open.serialConnection<-function(con, ...)
{
  if(isOpen(con))
  {
    warning(paste(con$port,'is already open and will be reconfigured!'))
    invisible(return('Reconfigure!'))
  }
  ## set platform depended path
  os_path <- switch(.Platform$OS.type
                    ,windows = '//./'
                    ,unix = '/dev/'
  )
  buffer_string <- switch(.Platform$OS.type
                          ,windows = ' -sysbuffer'
                          ,unix = ' -buffersize'
  )
  buffer_string <- paste(buffer_string,con$buffersize)
  
  ## open connection and variables
  if(!isOpen(con))
  { #set serial [open /dev/cu.usbserial r+]
    tryCatch( .Tcl( paste('set ::sdev_',con$var
                          ,' [open {',os_path,con$port,'} r+]'
                          ,sep='')
    )
    ,error = function(e) stop(simpleError(e$message))
    )
  }
  ## setup configuration, or reconfigure
  eof <- paste(" -eofchar \\x", charToRaw(con$eof),sep='')
  if(con$eof == '') eof <- ''
  
  .Tcl( paste('fconfigure ${sdev_',con$var,'}'
              ,' -mode ', con$mode
              ,' -buffering ',con$buffering
              ,' -blocking 0'
              ,' -translation ',con$translation
              ,' -handshake ',con$handshake
              ,buffer_string
              ,eof
              ,sep=''
  )
  )
  invisible('DONE')
  ## 'buffering none' is recommended
}
