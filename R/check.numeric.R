## Function Description:
##     A function to assess if a vector can be interpreted as numbers

check.numeric <- function(v = NULL, na.rm = FALSE, only.integer = FALSE,
                          exceptions=c(""), ignore.whitespace = TRUE){
    #----[ checking the input ]----#
    {
        # if the only.integer is NOT a single TRUE or FALSE
        if (!is.logical(only.integer) | length(only.integer) != 1) {
            # complain
            stop("The parameter \"only.integer\" should be either TRUE or FALSE.")
        }

        # if user has not defined the vector v
        if (is.null(v)) {
          # complain
            stop("The parameter \"v\" is not defined. It can be character vector, numeric vector, factor vector or logical vector.")
        # if user has defined but the class is NOT character or factor
        }else if (!inherits(v, c("character", "factor"))) {
            # if the class is NOT numeric or integer either
            if (!inherits(v, c("numeric", "integer", "logical"))) {
                # complain
                stop("The parameter \"v\" can only be a character vector, numeric vector, factor vector or logical vector.")
            # if the class is numeric or integer
            }else{
                # if user wants to specifically filter out non-integers, there
                # is a chance that the vector contains some non-integer numbers
                # so we should turn the vector to character and run the function
                if(only.integer){
                    # convert the vector to character
                    v <- as.character(v)
                }else{
                    # since it is already a number
                    return(rep(x = TRUE, length(v)))
                }
            }
        }
        
        
        #-------[ na.rm ]-------#
        {
            # if the na.rm is NOT a single TRUE or FALSE
            if (!is.logical(na.rm) | length(na.rm) != 1) {
                # complain
                stop("The parameter \"na.rm\" should be either TRUE or FALSE.")
            }
        }
        
        
        #-------[ ignore.whitespace ]-------#
        {
            # if the ignore.whitespace is NOT a single TRUE or FALSE
            if (!is.logical(ignore.whitespace) | length(ignore.whitespace) != 1) {
                # complain
                stop("The parameter \"ignore.whitespace\" should be either TRUE or FALSE.")
            }
        }
    }
    
    
    #----[ pre-processing ]----#
    {
        # convert to character if it is vector
        if (inherits(v, "factor")) {
            # convert to character
            v <- as.character(v)
        }
        
        
        # if user wants to ignore NAs
        if (na.rm) {
            v <- stats::na.omit(v)
        }
        
        
        # if user wants to ignore leading or tailing white space
        if (ignore.whitespace) {
            # substitute whitespaces in the beginning and at the ending of each item in v
            v <- gsub("^\\s+|\\s+$", "", v)
        }
    }
    
    
    #----[ processing ]----#
    {
        # if user wants to only detect integers
        if (only.integer) {
            regexp_pattern <- "(^(-|\\+)?\\d+$)|(^(-|\\+)?(\\d*)e(-|\\+)?(\\d+)$)"
        # if user wants to detect all numbers
        }else{
            #regexp_pattern <- "^(-|\\+)?\\d+(\\.?\\d+)?$"
            regexp_pattern <- "(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))$)|(^(-|\\+)?((\\.?\\d+)|(\\d+\\.\\d+)|(\\d+\\.?))e(-|\\+)?(\\d+)$)"
        }

        # perform the regexp
        output <- grepl(pattern = regexp_pattern, x = v)

        # check for existance of exceptions
        exception_index <- is.element(v, exceptions)
        # if there are is exception detected
        if (any(exception_index)) {
            # turn their output value to TRUE
            output[exception_index] <- TRUE
        }
        
        
        # if user wants to keep NA
        if (!na.rm) {
            # NAs are marked as FALSE by grepl and we replace it with TRUE instead
            output[is.na(v)] <- TRUE
        }
        
        
        # return the result
        return(output)
    }
}
