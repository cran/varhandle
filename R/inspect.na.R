## Function Description:
##     This function provides a summary of NAs in the given matrix or data.frame
##     either feature-wise (by column) or sample-wise (by row).

inspect.na <- function(d, hist=FALSE, summary=TRUE, byrow=FALSE, barplot=TRUE){
    if(!exists("d")){  # if the d was not defined by user
        stop("Please provide the d which can be a matrix or data.frame.")
    }else if(class(d) %in% c("data.frame", "matrix")){  # if the data was provided, check the class of the provided data
        # use the pin.na function to pin the NAs
        pin.na.output <- pin.na(d)

        if(!is.null(pin.na.output)){  # check if there is any NA
            if(byrow){  # if user want to have the results row-wise
                # extract the rw column that contains the rows that contain NA
                pin.na.output.rw <- pin.na.output[, "row_index"]

                # calculated the NA frequency in each of the reported columns
                na.density <- unlist(lapply(unique(pin.na.output.rw),
                                            function(x){
                                                sum(pin.na.output.rw==x)
                                            }))
                # calculate the ratio of having NA regarding the total number of values in the row
                na.ratio <- na.density/ncol(d)
                # return the report data.frame
                result <- data.frame(row_index = unique(pin.na.output.rw),
                                     row_name = row.names(d)[unique(pin.na.output.rw)],
                                     number_of_NAs = na.density,
                                     ratio_of_NA = na.ratio)

            }else{  # if user want to have the results column-wise
                # extract the clmn column that contains the columns that contain NA
                pin.na.output.clmn <- pin.na.output[, "column_index"]

                # calculated the NA frequency in each of the reported columns
                na.density <- unlist(lapply(unique(pin.na.output.clmn),
                                            function(x){
                                                sum(pin.na.output.clmn==x)
                                            }))
                # calculate the ratio of having NA regarding the total number of values in the column
                na.ratio <- na.density/nrow(d)
                # return the report data.frame
                result <- data.frame(column_index = unique(pin.na.output.clmn),
                                     column_name = colnames(d)[unique(pin.na.output.clmn)],
                                     number_of_NAs = na.density,
                                     ratio_of_NA = na.ratio)
            }

            # if user want to have the histogram to be plotted
            if(hist){
                # plot the histogram
                hist(pin.na.output.clmn,
                     ncol(d),
                     xlab = paste("Index number of the",
                                if(byrow){"rows"}else{"columns"}),
                     ylab = "NA Frequency")
            }

            # if user wants to have barplot as well
            if(barplot){
                colorlist <- rep.int("gray", nrow(result))
                colorlist[result$ratio_of_NA>0.1] <- "yellow"
                colorlist[result$ratio_of_NA>0.3] <- "orange"
                colorlist[result$ratio_of_NA>0.5] <- "red"

                # create a backup from the user's current par() settings
                default_par <- par(no.readonly = TRUE)

                par(mar = c(0, 0, 0, 0))
                layout(matrix(c(1, 2, 3), byrow = TRUE, ncol = 1), heights = c(1, 4, 0.7), widths = c(4))

                # plot{1}
                par(mar = c(0, 0, 0, 1))
                plot.new()
                # text(x = 0.5, y = 0.5, labels = "Missing values", cex = 2, font = 2)
                text(x = 0.5,
                     y = 0.5,
                     labels = paste("Missing values for",
                                    if(byrow){
                                        "rows"
                                    }else{
                                        "columns"
                                    }),
                     cex = 2,
                     font = 2)

                # plot{2}
                # make label text perpendicular to axis and set the margins
                par(las = 2, mar = c(8, 4, 0, 1))
                barplot(result$ratio_of_NA,
                        names.arg = if(byrow){
                                        result$row_name
                                    }else{
                                        result$column_name
                                    },
                        col = colorlist,
                        ylab = "Ratio of NA",
                        # xlab = if(byrow){"Row Names"}else{"Column Names"})
                        xlab = "")
                # draw lines for each cutoff
                abline(h = 0.5, col = "gray88", lty = 2)
                abline(h = 0.3, col = "gray88", lty = 2)
                abline(h = 0.2, col = "gray88", lty = 2)
                abline(h = 0.1, col = "gray88", lty = 2)
                # plot{3}
                par(mar = c(0, 0, 0, 1))
                plot.new()
                legend("top",
                       fill = c("red", "orange", "yellow", "gray"),
                       legend = c(">50%", ">30%", ">10%", "<=10%"),
                       horiz = TRUE,
                       text.width = strwidth(c(">50%", ">30%", ">10%", "<=10%"))*1.4)

                # set back the par() settings we changed to user's original setting
                par(mfrow=default_par$mfrow, mar=default_par$mar)
            }

            # if user wants to get the summary data.frame
            if(summary){
                # return the result data.frame to user
                return(result)
            }
        # if pin.na function didn't find any NAs
        }else{
            return(NULL)
        }

    }else{
        stop("Please provide the d which can be a matrix or data.frame")
    }
}
