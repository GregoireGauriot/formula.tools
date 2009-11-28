# -----------------------------------------------------------------------------
# op
#   extract and manipulate the operator of a call, expression or rule
# 
# -----------------------------------------------------------------------------

setGeneric( 'op', function(x, ...) standardGeneric( 'op' ) )

setMethod(  'op', 'call' , function (x) as.character(x[[1]]) )
setMethod(  'op', 'expression', function(x,...) lapply( x, op, ... ) )
setMethod(  'op', 'formula', function(x) as.character( x[[1]] ) )
setMethod(  'op', 'list', function(x, ...) lapply( x, op, ... ) )
                                                                     

# -----------------------------------------------------------------------------
# REPLACEMENT : OP<-
# -----------------------------------------------------------------------------
setGeneric( 'op<-', function(this,value) standardGeneric('op<-') )

# -------------------------------------
# SINGLE: call, formula
# -------------------------------------   
.replace.op.single <- function( this, value ) {
    this[[1]] <- value
    this
}



setReplaceMethod( 'op', 'call',    .replace.op.single )
setReplaceMethod( 'op', 'formula', .replace.op.single )


# -------------------------------------
# LIST AND VECTORS: expression, list
# -------------------------------------
.replace.op.many <- function( this, value ) {

    if( length(value) == 1 ) {
      for( i in length(this) ) op( this[[i]] ) <- value 
    } else {  
      if( length(this) != length(value) ) 
        stop( "Cannot change the 'op'.  Arguments have different lengths" )

      for( i in length(this) ) op( this[[i]] ) <- value[[i]]
    }

    this
}


setReplaceMethod( 'op', 'expression' , .replace.op.many )
setReplaceMethod( 'op', 'list' , .replace.op.many ) 


