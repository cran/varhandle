## Function Description:
##     Turn factors to their real values.
##     If given a matrix or data.frame, it detects the factor columns and
##     unfactor them. If everything in that column are just numbers and a
##     decimal character, it change it to numeric otherwise to character.

unfactor <- function(obj = NULL, auto_class_conversion = TRUE, verbose = FALSE){
    #-------[ checking the input ]-------#
    {
        # if the obj was not defined by user
        if (is.null(obj)) {
            stop("Please provide the obj which can be a matrix, data.frame or a vector.")
        }else if (!inherits(obj, c("data.frame", "matrix", "factor"))) {
            # if the provided object was not a data.frame, matrix or fector vector, throw an error.
            stop(paste("Please provide the obj which can be a matrix, data.frame or a vector. The provided obj has the class of", paste(class(obj), collapse = ", ")))
        }
    }
    
    
    #-------[ internal functions ]-------#
    {
        # an internal function to perform conversion on one single vector
        inner_func_convert <- function(x){
            # turn the x into character
            x <- as.character(x)
            
            # if user want the conversion to numeric happens automatically
            if(auto_class_conversion){
                # check if there is nothing but numbers
                if (all(check.numeric(v = x, na.rm = FALSE))) {
                    # everything is numbers, so change it to numeric
                    x <- as.numeric(x)
                }
            }
            
            # return theresult from lapply function
            return(x)
        }
        
        
        # an internal function to create messages if verbose is TRUE
        inner_func_msg <- function(...){
            message_text <- paste(unlist(list(...)), collapse = "")
            if(verbose){
                message(message_text)
            }
        }
    }
    
    
    #-------[ processing ]-------#
    {
        # save the obj class
        obj_class <- class(obj)
        
        inner_func_msg("The provided object has the following class",
                       ifelse(length(obj_class) > 1, "es", ""),
                       ":\n\t", paste(obj_class, collapse = ", "), "\n")
        
        
        # if the obj is a factor vector
        if (inherits(obj, "factor")) {
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
            
            inner_func_msg("The provided object has ",
                           length(factor_columns_indecies),
                           " columns with class of factor.\n")
            
            if(length(factor_columns_indecies)){
                if(length(factor_columns_indecies) == 1) {
                    # iterate through the factor columns and trun them into appropriate class
                    obj[, factor_columns_indecies] <- inner_func_convert(obj[, factor_columns_indecies])
                }else{
                    # iterate through the factor columns and trun them into appropriate class
                    obj[, factor_columns_indecies] <- data.frame(lapply(data.frame(obj[, factor_columns_indecies]),
                                                                        inner_func_convert),
                                                                 stringsAsFactors = F)
                }
            }else{
                inner_func_msg("No factor was found, hence no action was taken.")
            }
        }
        
        
        # return the result
        return(obj)
    }
}
