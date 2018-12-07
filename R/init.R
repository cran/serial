#' Sets up the interface parameters.
#' 
#' This is the constructor of the serial interface connection.
#' 
#' Linux and Windows behave a little bit different, when utilizing serial com 
#' ports. Still, by providing the name (like 'COM1' or 'ttyS1') and the 
#' appropriate settings, the serial interface can be used. Even virtual com 
#' ports, like the FTDI usb uart chips will work, as long they map to a standard
#' serial interface in the system.
#' 
#' Since the \code{serial} package relies on R's built in Tcl/Tk engine the 
#' configuration of the serial port takes place in the Tcl framework. This 
#' becomes important when different buffer sizes are set. For Windows the Tcl 
#' "-sysbuffer" parameter is invoked, whereas on unix-like systems "-buffersize" 
#' does the job.
#' 
#' @section Binary Data: 
#' 
#' Reading binary data is possible by setting \code{transaltion = 'binary'}. Pay 
#' attention that input and output are strings with a number range of 0...0xFF
#' which might require certain conversations e. g. \code{charToRaw()} or
#' \code{rawToChar()} functions. If \code{eof}-character is defined, this symbol
#' terminates the input data stream. Every byte in the buffer after that
#' symbol is deleted/ignored. The next transmission is valid again up to that symbol.
#' If the connection is closed \code{eof} is send to terminate the output data
#' stream.
#' 
#' @section ASCII Data:
#' 
#' In non binary mode, ASCII-communication is assumed. This means, that each
#' string, which is send or received, carries valid 8bit ASCII characters 
#' (0x01 -- 0xFF). Some of these characters appear as escaped sequences, if they
#' are not printable.
#' A string is terminated by the end-of-line character (e. g. \\n). The 
#' transmission ends and so becomes valid if a symbol is detected according to
#' the \code{translation} setting. Sending terminated strings invokes the
#' substitution of the end-of-line character according to the \code{translation}
#' setting.
#' 
#' 
#' @param name  optional name for the connection
#' @param port  comport name; also virtual com's are 
#'              supported; maybe USB schould work too
#' @param mode  communication mode '\code{<BAUD>, <PARITY>, <DATABITS>, <STOPBITS>}'
#' \describe{
#'    \item{\code{BAUD}}{sets the baud rate (bits per second)}
#'    \item{\code{PARITY}}{\emph{n, o, e, m, s} stands for 'none', 'odd', 'even', 'mark' and 'space'}
#'    \item{\code{DATABITS}}{integer number of data bits. The value can range from 5 to 8}
#'    \item{\code{STOPBITS}}{integer number of stop bits. This can be '1' or '2'}
#'        }       
#'              
#' @param buffering '\code{none}', best for RS232 serial interface. Connection buffer is flushed (send) when ever a write operation took place. '\code{line}', buffer is send after newline character (\\n or 0x0A) is recognized. '\code{full}' write operations will be bufferd until a \code{flush(con)} is invoked.
#' @param newline \code{<BOOL>}, whether a transmission ends with a newline or not.
#'                \describe{
#'                  \item{\code{TRUE} or 1}{send newline-char according to \code{<translation>} befor transmitting}
#'                  \item{\code{FALSE} or 0}{no newline}
#'                          }
#' @param handshake determines the type of handshaking the communication
#'                  \describe{
#'                    \item{'\code{none}'}{no handshake is done}
#'                    \item{'\code{rtscts}'}{hardware handshake is enabled}
#'                    \item{'\code{xonxoff}'}{software handshake via extra characters is enabled}
#'                    }
#' 
#' @param eof \code{<CHAR>}, termination char of the datastream. It only makes sense
#'        if \code{<translation>} is 'binary' and the stream is a file. Must be in the
#'        range of 0x01 -- 0x7f. When the conection is closed \code{eof} is send as 
#'        last and final character
#' @param translation  Determines the end-of-line (eol) character and mode of 
#'                     operation. This could be 'lf', 'cr', 'crlf', 'binary',
#'                     'auto' (default). A transmission is complete if eol 
#'                     symbol is received in non binary mode.
#' @param buffersize defines the system buffersize. The default value is 4096 
#'                   bytes (4kB).
#' @return An object of the class '\code{serialConnection}' is returned
#' @export
serialConnection<-function(name, port='com1', mode='115200,n,8,1', buffering='none', newline=0,eof='',translation='auto',handshake='none',buffersize = 4096)
{
  obj<-list()
  obj$name <- name
  obj$port <- port
  obj$mode <- mode
  obj$buffering <- buffering
  obj$newline <- newline
  obj$eof <- eof
  obj$translation <- translation
  obj$handshake <- handshake
  obj$buffersize <- buffersize
  class(obj) <- 'serialConnection'
  return(obj)
}

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
  {
    tryCatch( .Tcl( paste('set ::sdev_',con$port
                          ,' [open {',os_path,con$port,'} r+]'
                          ,sep='')
                    )
              ,error = function(e) stop(simpleError(e$message))
    )
  }
  ## setup configuration, or reconfigure
  eof <- paste(" -eofchar \\x", charToRaw(con$eof),sep='')
  if(con$eof == '') eof <- ''
  
  .Tcl( paste('fconfigure ${sdev_',con$port,'}'
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
  ## 'buffering none' is recommended, other setings doesn't work to send 
}

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
              .Tcl( paste( 'flush ${sdev_', con$port,'}', sep = '' ) )
              .Tcl( paste( 'close ${sdev_', con$port,'}', sep = '' ) )
             } 
              ,error = function(e) stop(simpleError(e$message))
    )
    .Tcl( paste( 'unset sdev_', con$port, sep = '' ) )
  }
  invisible('DONE')
}

#' Generic function for isOpen
#' 
#' @param con connection Object
#' @param ... not used
#' 
#' @export
isOpen <- function(con, ...) UseMethod('isOpen')

#' Default function from base-package
#' 
#' @param con connection object
#' @param rw defines the mode of operation
#' 
#' @seealso \code{\link[base]{isOpen}}
isOpen.default <- function(con, rw='')
{
  base::isOpen(con,rw)
}

#' Tests whether the connection is open or not
#' 
#' @param con connection of the class \code{serialConnection}
#' @param ... not used
#' 
#' @method isOpen serialConnection
#' 
#' @return returns \code{{F, T}} for 'not open' and 'is open'
#' @export
isOpen.serialConnection <- function(con,...)
{
  e <- 0 # assume that the connection is closed
  
  # read out variable list
  s <- c( tclvalue(.Tcl('info globals')),
          tclvalue(.Tcl('info locals'))
  )
  vars <- unlist(strsplit(s,' '))
  # only if the corresponding variable exists, there is a chance to test
  # if(tclvalue( .Tcl( paste('info exists sdev_',con$port, sep=''))) == 1) 
  if(paste('sdev_',con$port,sep='') %in% vars)
  {
    # get the names of all open channels (connections)
    chan_names <- tclvalue( .Tcl('file channels'))
    
    # get the tcl internal name of the connection
    con_name <- tclvalue(paste('sdev_',con$port, sep='')) 
    
    # test if con_name is in the list of channels
    e <- con_name %in% strsplit(chan_names,' ')[[1]]
  }
  
  return(ifelse(e == 1,T,F))
}
