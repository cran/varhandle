## Function Description:
##     A function to assess if a vector can be interpreted as numbers
## ToDo
##   * it can get a parameter to trim white space

check.numeric <- function(v=NULL, na.rm=FALSE, only.integer=FALSE, exceptions=c(""), ignore.whitespace=TRUE){
    #----[ checking the input ]----#
    {
        # if user has not defined the vector v
        if(is.null(v)){
          # complain
            stop("The parameter \"v\" is not defined. It can be character vector, numeric vector, factor vector or logical vector.")
        # if user has defined but the class is NOT character or factor
        }else if(!class(v) %in% c("character", "factor")){
            # if the class is NOT numeric or integer either
            if(!class(v) %in% c("numeric", "integer", "logical")){
                # complain
                stop("The parameter \"v\" can only be a character vector, numeric vector, factor vector or logical vector.")
            # if the class is numeric or integer
            }else{
                # since it is already a number
                return(rep(x = TRUE, length(v)))
            }
        }

        # if the na.rm is NOT a single TRUE or FALSE
        if(!is.logical(na.rm) | length(na.rm)!=1){
            # complain
            stop("The parameter \"na.rm\" should be either TRUE or FALSE.")
        }

        # if the only.integer is NOT a single TRUE or FALSE
        if(!is.logical(only.integer) | length(only.integer)!=1){
            # complain
            stop("The parameter \"only.integer\" should be either TRUE or FALSE.")
        }

        # if the ignore.whitespace is NOT a single TRUE or FALSE
        if(!is.logical(ignore.whitespace) | length(ignore.whitespace)!=1){
            # complain
            stop("The parameter \"ignore.whitespace\" should be either TRUE or FALSE.")
        }
    }


    #----[ pre-processing ]----#
    {
        # convert to character if it is vector
        if(class(v)=="factor"){
            # convert to character
            v <- as.character(v)
        }

        # if user wants to ignore NAs
        if(na.rm){
            # if it has some NAs
            if(any(is.na(v))){
                # remove NAs
                v <- v[-pin.na(v)]
            }
        }

        # if user wants to ignore leading or tailing white space
        if(ignore.whitespace){
            # substitute whitespaces in the begining and at the ending of each item in v
            v <- gsub("^\\s+|\\s+$", "", v)
        }
    }


    #----[ processing ]----#
    {
        # if user wants to only detect integers
        if(only.integer){
            regexp_pattern <- "^(-|\\+)?\\d+$"
        # if user wants to detect all numbers
        }else{
            #regexp_pattern <- "^(-|\\+)?\\d+(\\.?\\d+)?$"
            regexp_pattern <- "^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))$"
        }

        # perform the regexp
        output <- grepl(pattern = regexp_pattern, x = v)

        # check for existance of exceptions
        exception_index <- is.element(v, exceptions)
        # if there are is exception detected
        if(any(exception_index)){
            # turn their output value to TRUE
            output[exception_index] <- TRUE
        }

        # if user wants to keep NA
        if(!na.rm){
            # NAs are marked as FALSE by grepl and we replace it with TRUE instead
            output[is.na(v)] <- TRUE
        }


        # return the result
        return(output)
    }
}
