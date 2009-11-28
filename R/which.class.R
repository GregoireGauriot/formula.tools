# -----------------------------------------------------------------------------
# which.class
#   Make assesments about classes for objects.
#
#  TODO: 
#    alias & for intersect of numerical types. 
# -----------------------------------------------------------------------------
  
## IS ##
  
is.cat  <- function(x, classes = c( 'character', 'factor', 'logical' ) ) 
  class( x ) %in% classes       

is.cont <- function(x, classes = c( 'numeric', 'integer', 'Date' ) )
  class( x ) %in% classes 



## WHICH ## 

which.cat  <- function(x, ..., names = FALSE ) 
{ 
  ret <- which( unlist( lapply( x, is.cat, ... ) ) )
  return( if( names ) names(ret) else ret  )
}

# which.cat(iris)
# which.cat(iris,names=T)
    

which.cont  <- function(x, ..., names = FALSE ) 
{ 
  ret <- which( unlist( lapply( x, is.cont, ... ) ) )
  return( if( names ) names(ret) else ret  )
}    

# which.cont(iris)
# which.cont(iris,names=T)
                           


