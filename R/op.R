# -----------------------------------------------------------------------------
# op
#   extract and manipulate the operator of a call, expression or rule
# 
# -----------------------------------------------------------------------------

setGeneric( 'op', function(x, ...) standardGeneric( 'op' ) )

setMethod( 'op', 'call' , function (x) x[[1]] ) 
setMethod( 'op', 'formula', function(x) x[[1]] )
setMethod( 'op', 'name', function(x, ...) NULL )
setMethod( 'op', 'expression', function(x,...) lapply( x, op, ... ) )
setMethod( 'op', 'list', function(x, ...) lapply( x, op, ... ) )
                                                                     

# -----------------------------------------------------------------------------
# REPLACEMENT : OP<-
# -----------------------------------------------------------------------------
setGeneric( 'op<-', function(this,value) standardGeneric('op<-') )

# -------------------------------------
# SINGLE: call, formula
# -------------------------------------   
.replace.op.singular <- function( this, value ) {
    this[[1]] <- value
    this
}



setReplaceMethod( 'op', 'call',    .replace.op.singular )
setReplaceMethod( 'op', 'formula', .replace.op.singular )


# -------------------------------------
# LIST AND VECTORS: expression, list
# -------------------------------------
.replace.op.plural <- function( this, value ) {

    if( length(value) == 1 ) {
      for( i in length(this) ) op( this[[i]] ) <- value 
    } else {  
      if( length(this) != length(value) ) 
        stop( "Cannot change the 'op'.  Arguments have different lengths" )

      for( i in length(this) ) op( this[[i]] ) <- value[[i]]
    }

    this
}


setReplaceMethod( 'op', 'expression' , .replace.op.plural )
setReplaceMethod( 'op', 'list' , .replace.op.plural ) 


