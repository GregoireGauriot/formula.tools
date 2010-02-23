
# is.one.sided, is.two.sided


# ---------------------------------------------------------------------
# is.one.sided
# ---------------------------------------------------------------------
setGeneric( 'is.one.sided', function(x, ...) standardGeneric( 'is.one.sided' ) )

setMethod( 'is.one.sided', 'formula',
  function(x) 
    (
      class(x[[1]]) == 'name' && 
      deparse(x[[1]]) == tilde.operator && 
      length(x) == 2
    )   
)


setMethod( 'is.one.sided', 'call', 
  function(x, ...) { 
    if( deparse(x[[1]]) %in% relational.operators ) return( FALSE )  # Reqs 2 args
    if( deparse(x[[1]]) == tilde.operator && length(x) == 3 ) return( FALSE )
    return(TRUE)
  }
)

  
# ----------------------
# DEFAULT METHOD
# ----------------------
setMethod( 'is.one.sided', 'ANY', 
  function(x,...) {
    warning( "'is.one.sided' is not defined for object of class ", class(x) )
    return(NA)
  }
)

# ----------------------
# PLURAL METHODS
# ----------------------
.is.one.sided.plural <- function(x,...) sapply(x, is.one.sided) 

setMethod( 'is.one.sided', 'expression', .is.one.sided.plural ) 
setMethod( 'is.one.sided', 'list', .is.one.sided.plural )

          

# ---------------------------------------------------------------------
# is.two.sided
#  nb. identical to ! is.one.sided ?
# ---------------------------------------------------------------------
setGeneric( 'is.two.sided', function(x,...) standardGeneric( 'is.two.sided' ) )
setMethod( 'is.two.sided', 'ANY', function(x,...) ! is.one.sided(x,...) )


