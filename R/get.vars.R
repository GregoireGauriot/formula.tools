# -----------------------------------------------------------------------------
# METHOD: get.vars
#
#   Retrieves the variable names from various types of R objects such as calls, 
#   expressions, names and formulas.
#   
#   This method is similar to all.vars except it will expand '.' and other
#   special characters in the for
#   
#   Returns the variables in order of appearance
# -----------------------------------------------------------------------------

setGeneric( 
  'get.vars', function(x, data=NULL, ...) standardGeneric( 'get.vars' ) 
)


# ---------------------------------------------------------------------
# SIGNATURE: formula
#   For this to work correctly we need to treat the lhs and rhs distinctly
#   and merge the results.
#
#   Some edge cases may not work.
#  
# ---------------------------------------------------------------------
setMethod( 'get.vars', c( 'formula', 'ANY' ) ,
  # get.vars.form <- 
  function(x, data=NULL, ... ) {
    
    vars.lhs <- get.vars( lhs(x), data=data, ... )

    term.rhs <- terms.formula( x, data=data, ... )
    labels   <- attr( term.rhs, 'term.labels' )
    order    <- attr( term.rhs, 'order' )
    vars.rhs <- labels[ order == 1 ]

    unique( c(vars.lhs, vars.rhs)  )
    
  }
)


# ---------------------------------------------------------------------
# SIGNATURE: call
# ---------------------------------------------------------------------
setMethod( 'get.vars', c( 'call', 'ANY' ), 
  #  get.vars.call <- function(x,data,...) {
  function( x, data=NULL, ... ) {

    term <- terms( x, data=data, ... )
    vars <- attr( term, 'variables' )
    
    nms <- as.character(vars)

    if ( length(nms)  > 0 ) {
      return( nms[-1] )
    } else {
      return( NULL )
    }

  }
)


# ---------------------------------------------------------------------
#  SIGNATURE: expression, missing
# ---------------------------------------------------------------------
setMethod( 'get.vars', c( 'expression', 'missing' ) ,
  function( x, ... ) all.vars( x, ... ) 
)


# ---------------------------------------------------------------------
# SIGNATURE: name
#   Simply returns itself
# ---------------------------------------------------------------------
setMethod( 'get.vars', c( 'name', 'ANY' ) ,
  function( x, data, ... ) as.character(x) 
)



# ---------------------------------------------------------------------
# ANY
# ---------------------------------------------------------------------
setMethod( 'get.vars', c( 'ANY', 'ANY' ), 
  function( x, data, ... ) NULL
)



# dep.vars OR rhs.vars  
# setGeneric( 'lhs.vars', function(x, ... ) standardGeneric( 'lhs.vars' ) )
#setMethod( 'lhs.vars' , 'formula', 
#  function(x, ..., data=NULL) get.vars( lhs(x), data=data)
#)



# ind.vars OR lhs.vars 
#   Returans the lhs.vars
#        x[[1]] %in% c( relational.opertators, tilde )
#setGeneric( 'rhs.vars', function(x, ... ) standardGeneric( 'rhs.vars' ) )
#setMethod( 'rhs.vars', 'formula', 
#  function(x, ..., data=NULL) {
#
#    term.rhs <- terms.formula( x, data=data, ... )
#    labels   <- attr( term.rhs, 'term.labels' )
#    order    <- attr( term.rhs, 'order' )
#    vars.rhs <- labels[ order == 1 ]      
#
#    return(vars.rhs )
#
#  }
#
# )


# setGeneric( 'all.vars', function(x,... ) standardGeneric( 'all.vars' ) )

# setMethod( 'all.vars', 'formula', function(x) { "hw" } ) 
# all.vars.formula <- function( x ) x  





