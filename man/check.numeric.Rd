\name{check.numeric}
\alias{check.numeric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Check the vector's possiblity to convert to numeric}
\description{
    %%  ~~ A concise (1-5 lines) description of what the function does. ~~
    This function gets a character or factor vector and checks if all the value can be safely converted to numeric.
}
\usage{
    check.numeric(v=NULL, na.rm=FALSE, only.integer=FALSE, exceptions=c(""),
                  ignore.whitespace=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{v}{
        %%  ~~Describe \code{varlist} here~~
        The character vector or factor vector. (Mandatory)
    }
    \item{na.rm}{
        logical. Should the function ignore NA? Default value is FLASE since NA can be converted to numeric. (Optional)
    }
    \item{only.integer}{
        logical. Only check for integers and do not accept floating point. Default value is FALSE. (Optional)
    }
    \item{exceptions}{
        A character vector containing the strings that should be considered as valid to be converted to numeric. (Optional)
    }
    \item{ignore.whitespace}{
        logical. Ignore leading and tailing whitespace characters before assessing if the vector can be converted to numeric. Default value is TRUE. (Optional)
    }
}
\details{
    %%  ~~ If necessary, more details than the description above ~~
    This function checks if it is safe to convert the vector to numeric and this conversion will not end up in producing NA. In nutshell this function tries to mak sure provided vector contains numbers but in a non-numeric class. see example for better understanding.

    This function can be configured to only accept integer numbers (by setting the argument only.integer to TRUE). It can also ignore NA values (na.rm argument) and ignore heading/tailing whitespaces (ignore.whitespace argument).

    There is also room to manually define exceptions to be concidered as numbers (exceptions argument).
}
\value{
    %%  ~Describe the value returned
    %%  If it is a LIST, use
    %%  \item{comp1 }{Description of 'comp1'}
    %%  \item{comp2 }{Description of 'comp2'}
    %% ...
    The function return a logical vector. TRUE for all the elements in the given vector if they are safe to convert into numeric in R.

    In case of a integer, numric or logical vector, the funstion simply returns aall TRUE logical vector.
}
%%\references{
%%    ~put references to the literature/web site here ~
%%}
\author{
%%  ~~who you are~~
    Mehrad Mahmoudian
}
%%\note{
%%    ~~further notes~~
%%}

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
    # check if the vector is all numbers (not ignoring NAs)
    check.numeric(a)
    # check if the vector is all numbers (ignoring NAs)
    check.numeric(a, na.rm=TRUE)
    # if all the items in vector a are safe for numeric conversion
    if(all(check.numeric(a))){
        # convert the vector to numeric
        a <- as.numeric(a)
    }

    # create a complicated vector
    b <- c("1", "2.2", "3.", ".4", ".5.", "..6", "seven", "00008",
           "90000", "-10", "+11", "12-", "--13", "++14", NA, "",
           " 7 ", "   ")
    # show in propper format
    print(data.frame(value=b, check.numeric=check.numeric(b)))
    #      value check.numeric
    # 1        1          TRUE
    # 2      2.2          TRUE
    # 3       3.          TRUE
    # 4       .4          TRUE
    # 5      .5.         FALSE
    # 6      ..6         FALSE
    # 7    seven         FALSE
    # 8    00008          TRUE
    # 9    90000          TRUE
    # 10     -10          TRUE
    # 11     +11          TRUE
    # 12     12-         FALSE
    # 13    --13         FALSE
    # 14    ++14         FALSE
    # 15    <NA>          TRUE
    # 16                  TRUE
    # 17      7           TRUE
    # 18                  TRUE
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ check numeric }% __ONLY ONE__ keyword per line
