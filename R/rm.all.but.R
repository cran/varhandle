## Function Description:
##     This function removes all variables except those which are specified in
##     the given character vector.

rm.all.but <- function(keep=NULL, envir=.GlobalEnv, keep_functions=TRUE, gc_limit=100){
    #----[ checking the input ]----#
    {
        ## Check the envir attribute
        if (!is.environment(envir)){
            stop("You should specify an existing environment")
        }

        ## ckeck if keep is defined
        if (is.null(keep)){
            stop("The parameter `keep` is not defined. It should be a chacter vector with length 1 or more.")
        }

        ## Check if the keep is a character vector
        if (class(keep) != "character" | typeof(keep) != "character" ){
            stop("The value of `keep` parameter should be a chacter vector with length 1 or more.")
        }

        ## check if the length of keep is more than or equal to 1
        if (!length(keep)){
            stop("The value of `keep` parameter should be a chacter vector with length 1 or more.")
        }

        ## if the keepFunctions is not a logical vector
        if (!is.logical(keep_functions) | length(keep_functions) != 1){
            stop("The value of the `keepFunctions` should ve either TRUE or FLASE.")
        }

        ## check if the gc_limit has a valid value
        if (!is.numeric(gc_limit) | length(gc_limit) != 1){
            stop("The value of `gc_limit` parameter should be numeric with length 1. It's unit is MB.")
        }
    }



    #----[ processing ]----#
    {
        # If the keep has a name that is not in ls(), show error with the location of bad variable name
        if (any(!is.element(keep, ls(envir = as.environment(envir))))){
            bad_var_location <- which(is.element(keep, ls(envir = as.environment(envir))) == FALSE)
            stop(paste("All the items in the keep should be a real existing variable.\nThe item number", bad_var_location, "is not among variables of selected environment!\n", sep = " "))
        }


        # in case user decides to only operates on non-function variables
        if (keep_functions){
            # only put non-functions in the list
            var_list <- setdiff(ls(envir), lsf.str(envir = envir))
        # in case user wants to consider functions as well and remove them too
        }else{
            # put everything in the list
            var_list <- ls(envir)
        }

        # list the name of variables that should be removed
        removables <- var_list[!(var_list %in% keep)]

        # get to total sum of the variables that are going to be removed in bytes
        total_size <- sum(sapply(removables,
            function(x){
                object.size(get(x, envir = as.environment(envir)))
            }))

        # remove the variables
        remove(list = removables, envir = as.environment(envir))

        # if the total size of removed varibale exceeded the threshold
        if (total_size > (gc_limit * 1024 ^ 2)){
            # call garbage collection
            gc()
        }
    }
}
