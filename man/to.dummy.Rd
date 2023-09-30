\name{to.dummy}
\alias{to.dummy}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Convert categorical vector into dummy binary dataframe}
\description{
    %%  ~~ A concise (1-5 lines) description of what the function does. ~~
    This function gets a vector that contains some categories and convert it to dummy columns (also known as binary columns). The number of output columns is equal to the input categories.
}
\usage{
    to.dummy(v, prefix)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{v}{
        %%~~Describe \code{varlist} here~~
        A character, numeric or factor vector that contains the categories. (Mandatory)
    }
    \item{prefix}{
        %%~~Describe \code{path} here~~
        A character string to attach to the beginning of the column names to prevent confusion or conflicts. (Mandatory)
    }
}
\details{
    %%  ~~ If necessary, more details than the description above ~~
    This function simplifies the procedure of making data ready for those learning algorithms or methods that cannot handle categorical columns. It works by getting a character, numeric or factor vector and convert it to some columns that each of which represent a category from the input vector. For example a vector of eye color with different categories like Black, Brown, Blue, Green will be transformed into a dataframe with 4 columns and each column has value of 1 for samples that have that specific eye color.
}
\value{
    %%  ~Describe the value returned
    %%  If it is a LIST, use
    %%  \item{comp1 }{Description of 'comp1'}
    %%  \item{comp2 }{Description of 'comp2'}
    %% ...
    A data.frame is returned which only contains 0 and 1 as values. Number of this data.frame columns is equal to number of categories in the original input vector.
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

%%\seealso{
    %% ~~objects to See Also as \code{\link{help}}, ~~~
%%}
\examples{
    # load a dataframe (from base package)
    data(iris)
    
    # see the actual values of the categorical column
    print(iris$Species)
    
    # convert to dummy
    binary_species <- to.dummy(iris$Species, "species")
    # view the first few lines of the binary_species data.frame
    head(binary_species)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ Binary }
\keyword{ dummy }% __ONLY ONE__ keyword per line
