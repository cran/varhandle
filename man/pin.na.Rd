\name{pin.na}
\alias{pin.na}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Pinpoint NAs in a vector, matrix or data.frame}
\description{
    %%  ~~ A concise (1-5 lines) description of what the function does. ~~
    This function finds NAs (or defined missing values) in a vector, data.frame
    or matrix and returns a data.frame which contains two columns that includes
    the row number and column number of each NA.
}
\usage{
    pin.na(x, na.value = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{x}{
        %%  ~~Describe \code{varlist} here~~
        A vector, data.frame or matrix which you want to pinpoint its NAs.
        (Mandatory)
    }
    
    \item{na.value}{
        A vector containing the value that should be considered as missing
        value. The default is NA, but you can add to it or change it to your
        preference. See the example. (Optional)
    }

}
\details{
    %%  ~~ If necessary, more details than the description above ~~
    This function provides a quick and easy way to locate and pinpoint NAs in a given vector, data.frame or matrix.
    This function is also used in \code{\link{inspect.na}} function of this package.
}
\value{
    %%  ~Describe the value returned
    %%  If it is a LIST, use
    %%  \item{comp1 }{Description of 'comp1'}
    %%  \item{comp2 }{Description of 'comp2'}
    %% ...
    If a vector is given, the index of NAs will be returned in a numeric vector format. In case of a given matrix or data.frame the function will return a data.frame with two columns, one indicating the row number and one indicating the column number. Each row will represent a location of a NA.
    In case no NA is found, the function will return NULL which makes it easy to use in if conditions using \code{\link{is.null}}.
}
%%\references{
    %% ~put references to the literature/web site here ~
%%}
\author{
  %%~~who you are~~
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
    var1[runif(7, 1, 30)] <- NA
    # pinpoint NAs
    pin.na(var1)
    
    # create a matrix
    var2 <- matrix(runif(100, 10, 99), nrow = 10)
    # add NA at random places
    var2[runif(9, 1, 100)] <- NA
    # pinpoint NAs
    pin.na(var2)
    
    ## define your own missing values:
    var2[runif(5, 1, 100)] <- "."
    pin.na(var2, na.value = c(NA, "."))


}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ NA }
\keyword{ missing }
\keyword{ locate }
\keyword{ pinpoint }% __ONLY ONE__ keyword per line
