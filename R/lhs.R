# -----------------------------------------------------------------------------
# lhs
#   extract and manipulate the left-hand side of R objects.
# -----------------------------------------------------------------------------

setGeneric( 'lhs', function(x, ...) standardGeneric( 'lhs' ) )
setMethod(  'lhs', 'call', 
  function(x) 
  { 
    if( class(x[[1]]) == 'name' && 
        deparse(x[[1]]) %in% relational.operators 
    ) {
      x[[2]]
    } else {
      warning( "There is no relational operator defined for ", deparse(x)  )
    }
  }
)


setMethod(  'lhs', 'formula', 
  function(x,...) {
    
  # rh only, e.g. ~ a 
    if ( length(x) == 2 ) return( NULL )

    if ( length(x) == 3 ) return( x[[2]] ) 

  }
)


setMethod(  'lhs', 'expression', function(x,...) lapply( x, lhs, ... ) )
setMethod(  'lhs', 'list', function(x,...) lapply( x, lhs, ... ) )



# -----------------------------------------------------------------------------
# REPLACEMENT : lhs<-
# -----------------------------------------------------------------------------
setGeneric( 'lhs<-', function(this,value) standardGeneric('lhs<-') )


# -------------------------------------
# SINGLE: call, formula
# -------------------------------------
.replace.lhs.single <-  function(this,value) {
    this[[2]] <- value 
    this 
}

setReplaceMethod( 'lhs', 'call' , .replace.lhs.single )
setReplaceMethod( 'lhs', 'formula' , .replace.lhs.single )


# -------------------------------------
# LIST AND VECTORS: expression, list
# -------------------------------------
.replace.lhs.many <- function( this, value ) {

    if( length(value) == 1 ) {
      for( i in length(this) ) lhs( this[[i]] ) <- value 
    } else {  
      if( length(this) != length(value) ) 
        stop( "Cannot change the lhs.  Arguments have different lengths" )

      for( i in length(this) ) lhs(this[[i]] ) <- value[[i]]
    }

    this
}        


setReplaceMethod( 'lhs', 'expression' , .replace.lhs.many )
setReplaceMethod( 'lhs', 'list' , .replace.lhs.many )


