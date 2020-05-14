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
  if(paste('sdev_',con$var,sep='') %in% vars)
  {
    # get the names of all open channels (connections)
    chan_names <- tclvalue( .Tcl('file channels'))
    
    # get the tcl internal name of the connection
    con_name <- tclvalue(paste('sdev_',con$var, sep='')) 
    
    # test if con_name is in the list of channels
    e <- con_name %in% strsplit(chan_names,' ')[[1]]
  }
  
  return(ifelse(e == 1,T,F))
}
