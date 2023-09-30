\name{check.numeric}
\alias{check.numeric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Check the vector's possibility to convert to numeric}
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
        logical. Should the function ignore NA? Default value is FALSE since NA can be converted to numeric. (Optional)
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
    This function checks if it is safe to convert the vector to numeric and this conversion will not end up in producing NA. In nutshell this function tries to make sure provided vector contains numbers but in a non-numeric class. See example for better understanding.

    This function can be configured to only accept integer numbers and ignoring those with decimal point (by setting the argument `only.integer` to `TRUE`). It can also ignore NA values (`na.rm` argument) and ignore heading/tailing whitespaces (`ignore.whitespace` argument).

    There is also room to manually define exceptions to be considered as numbers (exceptions argument).
}
\value{
    %%  ~Describe the value returned
    %%  If it is a LIST, use
    %%  \item{comp1 }{Description of 'comp1'}
    %%  \item{comp2 }{Description of 'comp2'}
    %% ...
    The function return a logical vector. TRUE for all the elements in the given vector if they are safe to be converted into numeric in R (does not turned into NA).
    Remember that `""` and `"   "` can be converted to numeric safely (without getting the "NAs introduced by coercion" error), but their value would be NA.

    In case of a integer, numeric or logical vector, the function simply returns all TRUE logical vector.
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
       " 7 ", "   ", "8e2", "8.6e-10", "-8.6e+10", "e3")

    # show in proper format
    print(data.frame(value=b, check.numeric=check.numeric(b), converted=as.numeric(b)))
        #       value check.numeric converted
        # 1         1          TRUE   1.0e+00
        # 2       2.2          TRUE   2.2e+00
        # 3        3.          TRUE   3.0e+00
        # 4        .4          TRUE   4.0e-01
        # 5       .5.         FALSE        NA
        # 6       ..6         FALSE        NA
        # 7     seven         FALSE        NA
        # 8     00008          TRUE   8.0e+00
        # 9     90000          TRUE   9.0e+04
        # 10      -10          TRUE  -1.0e+01
        # 11      +11          TRUE   1.1e+01
        # 12      12-         FALSE        NA
        # 13     --13         FALSE        NA
        # 14     ++14         FALSE        NA
        # 15     <NA>          TRUE        NA
        # 16                   TRUE        NA
        # 17       7           TRUE   7.0e+00
        # 18                   TRUE        NA
        # 19      8e2          TRUE   8.0e+02
        # 20  8.6e-10          TRUE   8.6e-10
        # 21 -8.6e+10          TRUE  -8.6e+10
        # 22       e3         FALSE        NA

    # remember that "" and "   " can be converted to numeric safely, but their value would be NA.
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ check numeric }% __ONLY ONE__ keyword per line
