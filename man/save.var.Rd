\name{save.var}
\alias{save.var}
\title{Save variables separate files}
\description{This function gets a list of variables as a character vector and save each variable name in a separate file, so that they can be loaded separately.}
\usage{
    save.var(varlist = ls(envir = as.environment(.GlobalEnv)),
             path = getwd(), newdir = TRUE, newdirtag = NULL, envir = .GlobalEnv)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{varlist}{
        Character vector containing variable names. If not provided the function will use all the variables of the environment. (Optional)
    }
  \item{path}{
        Path to folder that you want to save the files in. If not provided, the current working directory will be used. (Optional)
    }
    \item{newdir}{
        Logical vector of length 1 indicating whether you want to create a subdirectory to store your variable files in. This subdirectory will have the execution time (\%Y\%m\%d-\%H\%M\%S) as folder name in combination with a text tag (newdirtag) if provided. (Optional)
    }
    \item{newdirtag}{
        Character string used in combination with newdir=TRUE to tag the time based folder name with a custom name. If not provided no tag will be used and the folder name will just have the time format. (Optional)
    }
    \item{envir}{
        Character string providing target environment in R session. The default is the Global Environment. (Optional)
    }
}
\details{
    This function is used for saving variables in batch into separate files in an organized way. This specifically comes handy when you generate many variables either dynamically or manually and you want to save them for later use and empty your memory. Saving variables in separate files help finding them as a file easier and faster and also reduces the loading time of the variable since you are also loading those that you want to use.

    The save.var function has the feature to save the variables in subfolder to help user manage different version of same variable which are related to different runs.
}
%%\value{
    %%  ~Describe the value returned
    %%  If it is a LIST, use
    %%  \item{comp1 }{Description of 'comp1'}
    %%  \item{comp2 }{Description of 'comp2'}
    %% ...
%%}
%%\references{
    %% ~put references to the literature/web site here ~
%%}
\author{
    Mehrad Mahmoudian
}
%%\note{
    %%  ~~further notes~~
%%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
    \code{\link{save.image}}, \code{\link{save}}
}
\examples{
# generate variables dynamically
lapply(letters, function(x){assign(x=x, value=rnorm(1), env=globalenv())})
# simple usage
save.var()
# specify a listof variables with tag
save.var(varlist=c("a","e","i","o","u"), newdirtag="just_vowels")

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ save variable }
\keyword{ save to file }% __ONLY ONE__ keyword per line
