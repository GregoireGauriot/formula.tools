\name{\%!in\%}
\Rdversion{1.1}
\alias{\%!in\%}
\alias{!in}
\alias{not in}

\title{ Logical inverse on \%in\% }


\description{
  Logical NOT IN. Binary operator which returns a logical vector indicating
  if there is NO match or not for its left operand. 
}

\usage{
  %!in%(x, table)
}

\arguments{
  \item{x}{ A vector or 'NULL'  }
  \item{table}{ A vector or 'NULL'.  Values to be tested against. }
}

\details{
  Currently defined as:
    "%!in%"<-function(x,table) ! x %in% table  
}
\value{
  A logical vector of the same length as 'x'.
}

\author{
    Christopher Brown
}


\seealso{
    \code{\link{match}}
}

\examples{
  4   %!in% 1:3
  "f" %!in% letters[1:5] 
}

\keyword{ logic }
\keyword{ manip }
