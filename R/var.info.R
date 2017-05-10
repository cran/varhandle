## create good info table for variables
# http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
# the total memory the variables are using.

var.info <- function(list = "ALL", envir = .GlobalEnv, human.readable = TRUE,
                     sortby = "size", decreasing = TRUE, n = Inf, progressbar = FALSE){
    #-------[ checking the input ]-------#
    {
        if(is.null(list)){
            stop("The parameter \"list\" should be defined. It should contain a list of variable names with the length of 1 or more.")
        }else if(length(list) == 1){
            if(list == "ALL"){
                # if user has selected nothing for list or "ALL", the function will consider it as ls() for provided environment
                var_list <- ls(envir = envir)
            }else{
                # if user has provided a name, pt it into the var_list
                var_list <- list
            }
        }else{
            # if user has provided a series of names, pour them into the var_list
            var_list <- list
        }

        # If the keep has a name that is not in ls(), show error with the location of bad variable name
        if(any(!is.element(var_list, ls(envir = as.environment(envir))))){
            # get the index of faulty variable names
            bad_var_location <- which(!is.element(var_list, ls(envir = as.environment(envir))))
            stop(paste("All the items in the \"list\" should be a real existing variable.\nThe item",
                       paste(var_list[bad_var_location], collapse = ", "),
                       "is not among variables of the selected environment!\n", sep = " "))
        }

        # check if the parameter n `sortby` argument is among valid column names
        sortby.index <- match(sortby, c("name", "class", "size", "detail"))
        if(is.na(sortby.index) | length(sortby) != 1){
            stop("The column specified by the parameter \"sortby\" is not valid. Valid columns are:\n  name, class, size, detail\nOne column should be selected.")
        }

        # check human.readable input
        if(!is.logical(human.readable)){
            stop("The parameter \"human.readable\" should either be TRUE or FALSE.")
        }

        # check decreasing input
        if(!is.logical(decreasing)){
            stop("The parameter \"decreasing\" should either be TRUE or FALSE.")
        }

        # check the n input
        if(class(n) != "numeric" | n < 1 | length(n) != 1){
            stop("The parameter \"n\" should be a single positive integer number more than zero.")
        }

        # check the progressbar input
        if(!is.logical(progressbar)){
            stop("The parameter \"progressbar\" should either be TRUE or FALSE.")
        }
    }


    #-------[ processing ]-------#
    {
        ## define variables to fill in for loop
        var.size.readable <- vector()
        var.size.byte <- vector()
        var.class <- vector()
        var.detail <- vector()

        # if user wants to have progressbar
        if(progressbar){
            # create the progressbar
            pb <- txtProgressBar(min = 1, max = length(var_list), style = 3)
        }

        # iterate through the var_list
        for(i in var_list){
            # get the variable
            the.var <- get(i, envir = as.environment(envir))

            ## get the size
            # size in byte format
            var.size.byte <- c(var.size.byte, object.size(x = the.var))

            # if user wants to get object sizes in human readable format
            if(human.readable){
                # this will be in human readable format
                var.size.readable <- c(var.size.readable, format(object.size(x = the.var), units = "auto"))
            }

            ## get the class
            var.class <- c(var.class, paste0(class(get(i, envir = as.environment(envir))), collapse = ", "))

            ## get dim for matrix/dataframe
            if(any(class(the.var) %in% c("data.frame", "matrix"))){
                var.detail <- c(var.detail, paste("dimension:", paste0(dim(the.var), collapse = ", ")))
            }else if(class(the.var) %in% c("integer", "numeric", "character", "factor", "logical")){
                var.detail <- c(var.detail, paste("length:", length(the.var)))
            }else{
                var.detail <- c(var.detail, NA)
            }

            # if user wants to have progressbar
            if(progressbar){
                # increment the progressbar by one step
                setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
            }
        }
        # if user wants to have progressbar
        if(progressbar){
            # close the progressbar
            close(pb)
        }

        ## construct the output
        # if user has selected to have human readable variable size values
        if(human.readable){
            output <- data.frame(name = var_list,
                                 class = var.class,
                                 size = var.size.readable,
                                 detail = var.detail)
        }else{
            output <- data.frame(name = var_list,
                                 class = var.class,
                                 size = var.size.byte,
                                 detail = var.detail)
        }

        ## sort based on user's preference
        if(sortby == "size" & human.readable){
            output <- output[order(var.size.byte, decreasing = decreasing), ]
        }else{
            output <- output[order(output[, sortby.index], decreasing = decreasing), ]
        }

        ## correct the rownames
        row.names(output) <- 1:nrow(output)

        ## select the number of desired rows
        if(is.finite(n)){
            #  convert to integer
            n <- as.integer(n)
            # if the n is larger than number of rows we have
            if(nrow(output) < n){
                # use number of output rows as n
                n <- nrow(output)
            }
            # select the columns
            output <- output[1:n, ]
        }
    }

    ## return the output
    return(output)
}
