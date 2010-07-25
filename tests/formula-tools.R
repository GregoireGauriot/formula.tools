
# Tests for the formula.tools package
#   Using operators: relational, tilde. arithmetic, colon(range), 
#   colons, logical and assignment
#   
#   And these objects as formula, calls, expressions, lists, etc.
library(formula.tools)

# 
c.relational <- quote( A > B )
c.colon      <- quote( 1:5 )

# EXPRESSIONS 
e.relational <- expression( A < B, A > B, A <= B, A >= B, A == B, A %in% B )
e.arith      <- expression( A+B, A-B, A*B, A/B, A^B, A %% B, A %/% B )
e.colons     <- expression( A::B, A:::B )
e.logical    <- expression( A && B, A & B, A || B, A | B )
e.complex    <- expression( A + C < B + D )

# FORMULAS
f.simple     <- A ~ B  
f.complex    <- A + C ~ B + D 
f.one.sided  <- ~ B 



# TEST ALL OPERATORS 
for( o in operators(types=c('relational','arithmetic') ) ) {
  c <- parse( text=paste( "A", o, "B" ), srcfile=NULL )[[1]]
  # cat( o, "  " )
  print( lhs(c) == "A" && rhs(c) == "B" )
  print( op(c) == o )
}
# cat( "\n" )

  

# is.one.sided, is.two.sided

is.one.sided( c.relational ) == FALSE


# lhs 





