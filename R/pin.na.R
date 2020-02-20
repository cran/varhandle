## Function Description:
##     This function finds NAs in a vector, data.frame or matrix and returns a
##     data.frame which contains two columns that includes the row number and column
##     number of each NA.

pin.na <- function(x = NULL, na.value = NA){
    #----[ checking the input ]----#
    {
        # check if user has not provided x argument
        if (is.null(x)) {
            stop("Please provide parameter \"x\". It can be a matrix, data.frame or vector.")
        }
    }


    #----[ pre-processing ]----#
    {
        ## convert to character if it is factor
        if (inherits(x, "factor")) {
            x <- as.character(x)
        }
    }


    #----[ processing ]----#
    {
        ## count the NAs
        # create an empty variable to be filled in the following for loop
        missingness_index <- c()
        # iterate through the items in na.value
        for (i in na.value) {
            # if i is NA
            if (is.na(i)) {
                missingness_index <- c(missingness_index, which(is.na(x)))
            }else{
                missingness_index <- c(missingness_index, which(x == i))
            }
        }
        
        
        # if x is either data.frame or matrix
        if (inherits(x, c("data.frame", "matrix"))) {
            # if the count of NA is not zero
            if (length(missingness_index) != 0) {
                # get the column number
                clmn <- (missingness_index %/% dim(x)[1]) + 1
                # get the row number
                rw <- missingness_index %% dim(x)[1]

                clmn[rw == 0] <- clmn[rw == 0] - 1
                rw[rw == 0] <- dim(x)[1]

                # create the output
                output <- data.frame(row_index = rw, column_index = clmn)
            }else{
                ## no NA was found
                output <- NULL
            }
            
        # if x is either numeric, integer or character vector
        }else if (inherits(x, c("numeric", "integer", "character"))) {
            # if the count of NA is not zero
            if (length(missingness_index) != 0) {
                output <- missingness_index
            }else{
                ## no NA was found
                output <- NULL
            }
        }else{
            # if the input is something other than classes above, we can not handle it!!
            stop("Please provide the d which can be a matrix, data.frame or vector.")
        }

        # return the final result
        return(output)
    }
}
