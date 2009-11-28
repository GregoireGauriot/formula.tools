# -----------------------------------------------------------------------------
# rhs
#   extract and manipulate the right-hand side of R objects
# -----------------------------------------------------------------------------

setGeneric( 'rhs', function(x, ...) standardGeneric( 'rhs' ) )
setMethod( 'rhs', 'call', 
  # NB. Before getting the rhs of a call we have to make sure that the 
  #     First argument of the call has a right-hand side notion. So far,
  #     this is relational operators
  function(x) 
  {
    if( class(x[[1]]) == 'name' && 
        deparse(x[[1]]) %in% relational.operators 
    ) {
      x[[3]]
    } else {
      warning( "There is no relational operator defined for ", deparse(x)  )
    }
  }
)

setMethod( 'rhs', 'formula', function(x) x[[3]] )
setMethod( 'rhs', 'expression', function(x,...) lapply( x, rhs, ... ) )       
setMethod( 'rhs', 'list', function(x,...) lapply( x, rhs, ... ) )
 


# -----------------------------------------------------------------------------
# REPLACEMENT METHOD 
# -----------------------------------------------------------------------------
setGeneric( 'rhs<-', function(this,value) standardGeneric('rhs<-') )

# -------------------------------------
# SINGLE: call, formula
# -------------------------------------
.replace.rhs.single <-  function(this,value) {
    this[[3]] <- value 
    this 
}                                                    

setReplaceMethod( 'rhs', 'call' , .replace.rhs.single )
setReplaceMethod( 'rhs', 'formula' , .replace.rhs.single )


# -------------------------------------
# LIST AND VECTORS: expression, list
# -------------------------------------
.replace.rhs.many <- function( this, value ) {

    if( length(value) == 1 ) {
      for( i in length(this) ) rhs( this[[i]] ) <- value 
    } else {  

      if( length(this) != length(value) ) 
        stop( "Cannot change the rhs  Arguments have different lengths" )

      for( i in length(this) ) rhs( this[[i]] ) <- value[[i]]

    }

    this
}        


setReplaceMethod( 'rhs', 'expression' , .replace.rhs.many )
setReplaceMethod( 'rhs', 'list' , .replace.rhs.many )


