\name{pin.na}
\alias{pin.na}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pinpoint NAs in a vector, matrix or data.frame}
\description{
    %%  ~~ A concise (1-5 lines) description of what the function does. ~~
    This function provides a list of locations for each NA found in the variable.
}
\usage{
    pin.na(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{x}{
        %%  ~~Describe \code{varlist} here~~
        A vector, data.frame or matrix which you want to pinpoint NAs (Mandatory)
    }
    
}
\details{
    %%  ~~ If necessary, more details than the description above ~~
    This function is a quick and easy way to locate and pinpoint NAs in a given vector, data.frame or matrix.
    %% This function is also used in \code{\link{summary.na}} function of this package.
}
\value{
    %%  ~Describe the value returned
    %%  If it is a LIST, use
    %%  \item{comp1 }{Description of 'comp1'}
    %%  \item{comp2 }{Description of 'comp2'}
    %% ...
    If a vector is given, the index of NAs will be returned in a numeric vector format. In case of a given matrix or data.frame the function will return a data.frame with two columns, one indicating the row and one indicating the column number, each row will be location of a NA.
}
%%\references{
    %% ~put references to the literature/web site here ~
%%}
\author{
%%  ~~who you are~~
	Mehrad Mahmoudian
}
%%\note{
    %%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
    %% ~~objects to See Also as \code{\link{help}}, ~~~
    \code{\link{is.na}}
}

\examples{
    ## generate some variables
    # create a vector
    var1 <- 1:30
    # add NA at random places
    var1[runif(7, 1,30)] <- NA
    # pinpoint NAs
    pin.na(var1)
    
    # create a matrix
    var2 <- matrix(runif(100, 10, 99), nrow=10)
    # add NA at random places
    var2[runif(9, 1, 100)] <- NA
    # pinpoint NAs
    pin.na(var2)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ NA }
\keyword{ locate }
\keyword{ pinpoint }% __ONLY ONE__ keyword per line