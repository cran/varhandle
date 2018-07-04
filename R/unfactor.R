## Function Description:
##     Turn factors to their real values.
##     If given a matrix or data.frame, it detects the factor columns and
##     unfactor them. If everything in that column are just numbers and a
##     decimal character, it change it to numeric otherwise to character.

unfactor <- function(obj = NULL){
    #----[ checking the input ]----#
    {
        # if the obj was not defined by user
        if (is.null(obj)) {
            stop("Please provide the obj which can be a matrix, data.frame or a vector.")
        }else if (!class(obj) %in% c("data.frame", "matrix", "factor")) {
            # if the provided object was not a data.frame, matrix or fector vector, throw an error.
            stop(paste("Please provide the obj which can be a matrix, data.frame or a vector. The provided obj has the class of", class(obj)))
        }
    }


    #----[ processing ]----#
    {
        # save the obj class
        obj_class <- class(obj)

        # if the obj is a factor vector
        if (obj_class == "factor") {
            # convert obj to character
            obj <- as.character(obj)
            # check if there is nothing but numbers
            if (all(check.numeric(v = obj, na.rm = FALSE))) {
                # everything is numbers, so change it to numeric
                obj <- as.numeric(obj)
            }

        # if the object was matrix or data.frame
        }else{
            # find the index for factor columns
            factor_columns_indecies <- which(sapply(obj, is.factor))
            # iterate through the factor columns and trun them into appropriate class
            obj[, factor_columns_indecies] <- data.frame(lapply(obj[, factor_columns_indecies], function(x) {
                # turn the x into character
                x <- as.character(x)
                # check if there is nothing but numbers
                if (all(check.numeric(v = x, na.rm = FALSE))) {
                    # everything is numbers, so change it to numeric
                    x <- as.numeric(x)
                }
                # return theresult from lapply function
                return(x)
            }), stringsAsFactors = F)
        }

        # return the result
        return(obj)
    }
}
