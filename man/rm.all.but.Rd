\name{rm.all.but}
\alias{rm.all.but}
\title{
    Remove all variables except those that you mention
}
\description{
    This function removes all existing variables (in the defined environment) except the variables given to it.
}
\usage{
    rm.all.but(keep=NULL, envir=.GlobalEnv, keep_functions=TRUE, gc_limit=100)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{keep}{
        A vector containing the name of variables which you wish to keep and not remove. (Mandatory)
    }
    \item{envir}{
        The environment that this function should be functional in, search in and act in. (Optional)
    }
    \item{keep_functions}{
        A logical vector of length 1 indicating exclusion of function variables from removal. (optional)
    }
    \item{gc_limit}{
        A numeric vector of length 1 indicating the threshold for garbage collection in Megabyte (MB) scale. (Optional)
    }
}
\details{
    While working with R it happens that users generates an accumulate many variables and at some point they just want to keep some of them and remove the rest to make the workspace clean and reduce memory usage. This is where this function comes in to keep those variables user wants and remove the rest.

    The garbage collection will run if the size of removed variables exceed the gc_limit parameter and will tell R to give back the amount of occupied memory by removed variables to the system. This comes handy since usually removed variables are created temprorily and removing them should free up the memory.
}
%%\value{

%%}
%%\references{

%%}
\author{
	Mehrad Mahmoudian
}
%%\note{

%%}


\seealso{
    \code{\link{remove}}
    \code{\link{gc}}
    \code{\link{object.size}}
}
\examples{
    # create some variable
    for(i in  names(iris)){
    	assign(i, iris[,i])
    }
    # see the list of variables
    ls()
    # remove every variable except Species and i
    rm.all.but(c("Species", "i"))
    # see which variable are left
    ls()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ remove variables }
\keyword{ keep variables }% __ONLY ONE__ keyword per line
