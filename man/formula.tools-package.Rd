\name{formula.tools-package}
\Rdversion{1.1}
\alias{formula.tools-package}
\alias{formula.tools}
\docType{package}
\title{
   Tools for working with formulas, expressions, calls and other R objects
}

\description{
  This packages provides a number of useful utilities for working with 
  formulas, expressions, calls, names/symbols and other R objects.
  These utilities can be used for symbolic manupulation of these objects. 
}

\details{
\tabular{ll}{
    Package: \tab formtools\cr
    Type: \tab Package\cr
    Version: \tab 1.0.1\cr
    Date: \tab 2010-07-25\cr
    License: \tab LPGL(>=2)\cr
    LazyLoad: \tab yes\cr
  }
}

\author{
  Christopher Brown  
  Maintainer: Christopher Brown <cbrown@opendatagroup.com>
  \url{http://www.opendatagroup.com}
}

\keyword{ package }


\examples{
  e <- expression( A > 5, B == 10, C + D >= E )
  get.vars(e)

  lhs(e)
  rhs(e)
  op(e) 
  
}
