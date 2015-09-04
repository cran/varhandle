\name{check.numeric}
\alias{check.numeric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert factor into appropriate class}
\description{
    %%  ~~ A concise (1-5 lines) description of what the function does. ~~
    This function gets a character or factor vector and checks if all the value are numeric. meaning if it is safe to convert the vector to numeric.
}
\usage{
    check.numeric(v, rm.na=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{v}{
        %%  ~~Describe \code{varlist} here~~
        The character vector or factor vector. (Mandatory)
    }
    \item{rm.na}{
        Should the function ignore NA? Default value is TRUE. (Optional)
    }
}
\details{
    %%  ~~ If necessary, more details than the description above ~~
    This function checks if the input vector is all numeric. see example for better understanding.
}
\value{
    %%  ~Describe the value returned
    %%  If it is a LIST, use
    %%  \item{comp1 }{Description of 'comp1'}
    %%  \item{comp2 }{Description of 'comp2'}
    %% ...
    The function return TRUE if the input is all numeric and FALSE otherwise.
}
%%\references{
%%    %% ~put references to the literature/web site here ~
%%}
\author{
%%  ~~who you are~~
	Mehrad Mahmoudian
}
\note{
    %%  ~~further notes~~
    In case of an input with class of numeric or integer, it pushes a warning along with the TRUE value.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
    %% ~~objects to See Also as \code{\link{help}}, ~~~
    \code{\link{as.numeric}}
}

\examples{
    # Create a vector with NA
    a <- as.character(c(1:5, NA, seq(from=6, to=7, by=0.2)))
    # see what we created
    print(a)
    # check if the vector is all numbers (ignoring NAs)
    check.numeric(a)
    # check if the vector is all numbers ( not ignoring NAs)
    check.numeric(a, rm.na=FALSE)
    
    # create a complicated vector
    b <- c("1", "2.2", "3.", ".4", ".5.", "..6", "seven", "00008",
           "90000", "-10", "+11", "12-", "--13", "++14")
    # check items in b one by one
    unlist(lapply(b, check.numeric))
    # show in propper format
    print(data.frame(value=b, check.numeric=unlist(lapply(b, check.numeric))))
    #     value check.numeric
    # 1      1          TRUE
    # 2    2.2          TRUE
    # 3     3.         FALSE
    # 4     .4         FALSE
    # 5    .5.         FALSE
    # 6    ..6         FALSE
    # 7  seven         FALSE
    # 8  00008          TRUE
    # 9  90000          TRUE
    # 10   -10          TRUE
    # 11   +11          TRUE
    # 12   12-         FALSE
    # 13  --13         FALSE
    # 14  ++14         FALSE

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ check numeric }% __ONLY ONE__ keyword per line
