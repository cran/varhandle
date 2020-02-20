## create good info table for variables
# http://stackoverflow.com/questions/1358003/tricks-to-manage-the-available-memory-in-an-r-session
# the total memory the variables are using.

var.info <- function(list = "ALL", regex = NULL, envir = .GlobalEnv,
                     human.readable = TRUE, sortby = "size", decreasing = TRUE,
                     n = Inf, beautify = FALSE, progressbar = FALSE){
    #-------[ checking the input ]-------#
    {
        #-------[ envir ]-------#
        {
            if(!is.environment(envir)){
                stop("The provided object for the \"envir\" argument is not an environment. use the following command to verify if the object is environment:\n\tis.environment()")
            }
        }
        
        #-------[ list ]-------#
        {
            if (is.null(list)) {
                stop("The parameter \"list\" should be defined. It should contain a list of variable names with the length of 1 or more.")
            }else if (!inherits(list, "character")) {
                stop('The parameter \"list\" should contain the name of variables you want to get their info in form of a character vector (put the names in "")')
            }else if (length(list) == 1) {
                if (list == "ALL") {
                    # if user has selected nothing for list or "ALL", the function will consider it as ls() for provided environment
                    var_list <- ls(envir = envir)
                    
                    # if the variable list is empty
                    if(length(var_list) == 0){
                        # if user have not set a specific environment
                        if(identical(envir, .GlobalEnv)){
                            stop("There is no varibale to be processed.")
                        }else{
                            stop("There is no varibale in the specified environment to be processed.")
                        }
                    }
                }else{
                    # if user has provided a name, put it into the var_list
                    var_list <- list
                }
            }else{
                # if user has provided a series of names, pour them into the var_list
                var_list <- list
            }
            
            
            # If there is a name that is not in ls(), show error with the location of bad variable name
            if (any(!is.element(var_list, ls(envir = as.environment(envir))))) {
                # get the index of faulty variable names
                bad_var_location <- which(!is.element(var_list, ls(envir = as.environment(envir))))
                stop(paste("All the items in the \"list\" should be a real existing variable.\nThe item",
                        paste(var_list[bad_var_location], collapse = ", "),
                        "is not among variables of the selected environment!\n", sep = " "))
            }
        }
        
        #-------[ regex ]-------#
        {
            # if user has defined a regular expression to narrow down the search space for variable names
            if(!is.null(regex)){
                if(!any(grepl(x = var_list, pattern = regex))){
                    stop("After applying the provided regex, no variable is left in the list to be processed! Check you regular expression.")
                }
                
                # apply the regular expression to the list of variables
                var_list <- var_list[grepl(x = var_list, pattern = regex)]
            }
        }
        
        #-------[ sortby ]-------#
        {
            # check if the parameter `sortby` argument is among valid column names
            sortby.index <- match(sortby, c("name", "class", "size", "detail"))
            if (is.na(sortby.index) | length(sortby) != 1) {
                stop("The column specified by the parameter \"sortby\" is not valid. Valid columns are:\n  name, class, size, detail\nOne column should be selected.")
            }
        }

        #-------[ human.readable ]-------#
        {
            # check human.readable input
            if (!is.logical(human.readable)) {
                stop("The parameter \"human.readable\" should either be TRUE or FALSE.")
            }
        }

        #-------[ decreasing ]-------#
        {
            # check decreasing input
            if (!is.logical(decreasing)) {
                stop("The parameter \"decreasing\" should either be TRUE or FALSE.")
            }
        }

        #-------[ n ]-------#
        {
            # check the n input
            if ((!inherits(n, "numeric")) | n < 1 | length(n) != 1) {
                stop("The parameter \"n\" should be a single positive integer number more than zero.")
            }
        }

        #-------[ beautify ]-------#
        {
            # check the progressbar input
            if (!is.logical(beautify)) {
                stop("The parameter \"beautify\" should either be TRUE or FALSE.")
            }
        }
        
        #-------[ progressbar ]-------#
        {
            # check the progressbar input
            if (!is.logical(progressbar)) {
                stop("The parameter \"progressbar\" should either be TRUE or FALSE.")
            }
        }
    }


    #-------[ processing ]-------#
    {
        ## define variables to fill in the for loop
        var.size.readable <- vector()
        var.size.byte <- vector()
        var.class <- vector()
        var.detail <- vector()

        # if user wants to have progressbar
        if (progressbar) {
            # create the progressbar
            pb <- txtProgressBar(min = 1, max = length(var_list), style = 3)
        }

        # iterate through the var_list
        for (i in var_list) {
            # get the variable
            the.var <- get(i, envir = as.environment(envir))

            ## get the size
            # size in byte format
            var.size.byte <- c(var.size.byte, object.size(x = the.var))

            # if user wants to get object sizes in human readable format
            if (human.readable) {
                # this will be in human readable format
                var.size.readable <- c(var.size.readable,
                                       format(object.size(x = the.var),
                                              units = "auto"))
            }

            # get the class and append to the collective variable
            var.class <- c(var.class,
                           paste0(class(get(i, envir = as.environment(envir))),
                                  collapse = ", "))

            ## get dim for matrix/dataframe
            if (inherits(the.var, c("data.frame", "matrix", "Matrix"))) {
                var.detail <- c(var.detail, paste("dimension:", paste0(dim(the.var), collapse = ", ")))
            }else if (inherits(the.var, c("integer", "numeric", "character", "factor", "logical", "list"))) {
                var.detail <- c(var.detail, paste("length:", length(the.var)))
            }else{
                var.detail <- c(var.detail, NA)
            }

            # if user wants to have progressbar
            if (progressbar) {
                # increment the progressbar by one step
                setTxtProgressBar(pb, getTxtProgressBar(pb) + 1)
            }
        }
        # if user wants to have progressbar
        if (progressbar) {
            # close the progressbar
            close(pb)
        }

        ## construct the output
        # if user has selected to have human readable variable size values
        if (human.readable) {
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
        if((sortby == "size") & human.readable) {
            output <- output[order(var.size.byte, decreasing = decreasing), ]
        }else{
            output <- output[order(output[, sortby.index], decreasing = decreasing), ]
        }
        
        # if user have chosen to get beautified table
        if(beautify){
            # add visual cue for sorting direction in the column name
            colnames(output)[sortby.index] <- paste0(ifelse(decreasing == TRUE,
                                                            paste0("[", intToUtf8(0x25BE),"] "),
                                                            paste0("[", intToUtf8(0x25B4),"] ")),
                                                 colnames(output)[sortby.index])
        }

        # correct the rownames
        row.names(output) <- 1:nrow(output)

        ## select the number of desired rows
        if (is.finite(n)) {
            #  convert to integer
            n <- as.integer(n)
            # if the n is larger than number of rows we have
            if (nrow(output) < n) {
                # use number of output rows as n
                n <- nrow(output)
            }
            # select the columns
            output <- output[1:n, ]
        }
    }

    # return the output
    return(output)
}
