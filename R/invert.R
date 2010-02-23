# ------------------------------------------------------------------------------
# invert
#   inverts the operators in a formula
# 
# ------------------------------------------------------------------------------

# Before declaring a new generic function we check to see if it exists.
#  package::hash already defines an invert generic, so this is not 
#  necessary.
# if( ! isGeneric( 'invert' ) ) {
#  setGeneric( 'invert', function(x, ...) standardGeneric( 'invert' ) )
# }  


.invert.single <- 
  function(x) { 

    if ( as.character(op(x)) %in% keys(relational.inverses) ) 
    { 
      op(x) <- as.name( relational.inverses[[ as.character(op(x)) ]] )
    } else  {
      warning( "No inverse found for op:", op(x) )    
    }

    return(x)
}


setMethod( 'invert', 'call', .invert.single ) 


.invert.many <- 
  function(x) {

    for( i in 1:length(x) )
      x[[i]] <- invert( x[[i]] )
    
    return(x)

  }   

setMethod( 'invert', 'expression', .invert.many )






# invert( quote( A >  5 ) )
# invert( quote( A >= 5 ) )
# invert( quote( A <  5 ) )
# invert( quote( A <= 5 ) )
# invert( quote( A == 5 ) )
# invert( quote( A != 5 ) )
# invert( quote( A %in% lettters[1:5] ) )
# invert( quote( A %!in% letters[1:5] ) )


