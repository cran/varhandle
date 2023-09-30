\name{var.info}
\alias{var.info}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Get a detailed list of variables}
\description{
    %%  ~~ A concise (1-5 lines) description of what the function does. ~~
    This function provides a detailed information of variables in the specified environment. If no environment and list of variables provided for the function, it will consider all existing variables in global environment.
}
\usage{
    var.info(list="ALL", regex = NULL, envir=.GlobalEnv, human.readable=TRUE,
             sortby="size", decreasing=TRUE, n=Inf, beautify = FALSE,
             progressbar = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{list}{
        A list of variables which you want to get information for. If not specified, it gets all variables (Optional)
    }
    \item{regex}{
        A regular expression to be applied on the list of variables. This is very useful for example when `list = "ALL"`. (Optional)
    }
    \item{envir}{
        The environment in which you want this function to be functional. (Optional)
    }
    \item{human.readable}{
        If you want to have the variable size in human readable format (Kb, Mb, etc.). (Optional)
    }
    \item{sortby}{
        The name of a column that you wish to sort the output with. If not specified the result will be sorted by "size". Valid options are "name", "class", "size" or "detail". (Optional)
    }
    \item{decreasing}{
        A logical parameter (TRUE/FALSE) indicating that you want the sort to be done decreasingly or  increasingly. If not specified it is TRUE. (Optional)
    }
    \item{n}{
        Number of desired rows in output If you want to have top 10, n should be equal to 10. If not specified the output will include all variables. (Optional)
    }
    \item{beautify}{
        A Logical parameter indicating whether the output should beautified. At the moment it just adds a up/down triangle in the column name, showing the sort direction and based which column the table is sorted. Default value is FALSE. See Details for more information. (optional)
    }
    \item{progressbar}{
        A Logical parameter indicating whether user wants to see progressbar or not. Default value is FALSE. See Details for more information. (optional)
    }
}
\details{
    %%  ~~ If necessary, more details than the description above ~~
    This function is a quick way to have some basic information about a list of variables. By modifying and changing the default parameters, you can narrow down the variables you are investigating. The main objective of this function is providing the following information about the variables in an easy and intuitive way:
    class
    size (amount of memory the variable has occupied)
    detail (dimension for data.frame and matrices and length of vectors)

    In case the variable is a matrix or data.frame, in detail column the dimension will be provided, if it is a vector, the length will be reported, otherwise you will see NA in detail column for that specific variable.

    This function is usually very quick but in case of having many variables in the environment, it might take some time, hence a progressbar is implemented to inform user about the process.
}
\value{
    %%  ~Describe the value returned
    %%  If it is a LIST, use
    %%  \item{comp1 }{Description of 'comp1'}
    %%  \item{comp2 }{Description of 'comp2'}
    %% ...
    The output will be a sorted data.frame with 4 columns. The "name" column contains the name of each variable, column "class" contains the class of each variable, columns "size" show the amount of memory the variable is occupying (it can be configured to be in bytes or human readable format. for this use the human.readable parameter.) and "detail" column includes variable-specific information (for matrices and data.frames the dimension and for vectors their list will be reported.)
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
    \code{\link{class}}, \code{\link{object.size}}
}

\examples{
    ###### generate some variables ######
    # a data.frame
    data(iris)
    
    # some character vector
    for(i in 1:5){
        assign(letters[i], paste("some random text:",
                                 paste0(letters[runif(5, 1, 26)],
                                 collapse="")))
    }
    
    # a list
    f <- lapply(5:10, function(x){paste("some random text:",
                                  paste0(letters[runif(5, 1, 26)],
                                  collapse=""))})


    ###### demo of this function ######
    # basic usage
    var.info()
    
    # the sorting
    var.info(sortby="name", decreasing=FALSE)
    
    # select using regular expression
    var.info(regex="^i")
    
    # having the top 5 objects that use most memory in a beautified output
    var.info(n=5, sortby="size", decreasing=TRUE, beautify=TRUE)

}

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ information }
\keyword{ size }
\keyword{ memory usage }
\keyword{ variable }% __ONLY ONE__ keyword per line
