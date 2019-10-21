\name{unfactor}
\alias{unfactor}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert factor into appropriate class}
\description{
    %%  ~~ A concise (1-5 lines) description of what the function does. ~~
    This function gets a factor vector, data.frame or matrix (that contains factor columns), detects the real class of the values and convert factor to the real class.
}
\usage{
    unfactor(obj, auto_class_conversion = TRUE, verbose = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{obj}{
        %%  ~~Describe \code{varlist} here~~
        The factor vector, data.fram or matrix. (Mandatory)
    }
    
    \item{auto_class_conversion}{
        %%  ~~Describe \code{varlist} here~~
        Whether or not the function should automatically convert numbers to numeric. If set to FALSE, it will return all columns as class characters. Default is TRUE. (Optional)
    }
    
    \item{verbose}{
        %%  ~~Describe \code{varlist} here~~
        Whether or not the function should be verbose, meaning it should message user about the details of operation. Default is FALSE. (Optional)
    }
}
\details{
    %%  ~~ If necessary, more details than the description above ~~
    This function turns factors to their real values. when a data.frame is given, the function detects factor columns and unfactor them, so you can give the whole data.frameand the function takes care of the rest.
    The values' real class detection mechanism is in a way that if everything in that column or vector are numbers and a decimal character, it change it to numeric otherwise it will be changed to character vector.
    This functionality can be turned off by setting the `auto_class_conversion` argument to FALSE
}
\value{
    %%  ~Describe the value returned
    %%  If it is a LIST, use
    %%  \item{comp1 }{Description of 'comp1'}
    %%  \item{comp2 }{Description of 'comp2'}
    %% ...
    In case of providing a vector as an input, a character vector or numeric vector. This depends on the type of values the input variable contains. Check the details section for detailed information.
    In case of providing a data.frame, the same data.frame will be returned but with converted columns.
    In case there is nothing to get converted from factors, the function peacefully exits. You can get the details of the steps in form of message if you set the `verbose` argument to TRUE.
}
%%\references{
    %% ~put references to the literature/web site here ~
%%}
\author{
%%  ~~who you are~~
	Mehrad Mahmoudian
}
\note{
    %%  ~~further notes~~
    In case you have any issues with the function, please report to:
    https://bitbucket.org/mehrad_mahmoudian/varhandle/issues
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
    %% ~~objects to See Also as \code{\link{help}}, ~~~
    \code{\link{as.character}}, \code{\link{as.numeric}}
}

\examples{
    # load a dataframe (from base package)
    data(iris)
    
    # see the actual values of the categorical column
    class(iris$Species)
    
    # use vactor as input
    species <- unfactor(iris$Species)
    # check the class
    class(species)
    
    # use data.frame as input
    my_iris <- data.frame(Sepal.Length=factor(iris$Sepal.Length), sample_id=factor(1:nrow(iris)))
    my_iris <- unfactor(my_iris)
    # check the class
    class(my_iris)
    class(my_iris$Sepal.Length)
    class(my_iris$sample_id)
}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ unfactor }
\keyword{ defactor }% __ONLY ONE__ keyword per line
