# varhandle 2.0.6

* Fixed some typos in the documentation
* Fixed the issue related to not non-updated `man/varhandle-package.Rd`. Thanks to Kurt Hornik for pointing it out.

### Changes to existing functions

* `to.dummy()`
    - Updated to use `inherits()` instead of comparing `class()` manually

# varhandle 2.0.5

This is a minor version and nothing has changed except supporting newer R which led to *removal* of this package from CRAN!! read more here:

https://developer.r-project.org/Blog/public/2019/11/09/when-you-think-class.-think-again/index.html

-------

# varhandle 2.0.4

### Changes to existing functions

* `save.var()`
    - Fixed a bug that was throwing error when user was setting the `newdir = FALSE`
    - Fixed adding an extra whitespace in the final files right after the variable names and right before the file extension
* `check.numeric()`
    - Fixed [issue #3](https://bitbucket.org/mehrad_mahmoudian/varhandle/issues/3/checknumeric-doesnt-recognize-scientific). Added the ability to detect scientific numbers (e.g "8.6e-10")
    - Fixed typos in the comments of the code
* `unfactor()`
    - Fixed [issue #2](https://bitbucket.org/mehrad_mahmoudian/varhandle/issues/2/wrong-replacement-of-values-in-function). If the provided object was a dataframe and only one column was a factor, the function was replacing the values of that column with only the first appearing level.
    - Added an argument to suppress auto class conversion to let the user control if the autoconversion whould happen or everything should be returned as character [issue #1](https://bitbucket.org/mehrad_mahmoudian/varhandle/issues/1/unfactor-deletes-leading-zeros-and).
    - Added an argument to control verbosity of the function. By default is it `verbose = FALSE`, but by setting it to `TRUE` you will get messages about different steps. This would be useful for debugging or using in pipelines.
    - Now you can use the function on any vector or dataframe and it will not complain or change anything if it does not find any factors.
* `var.info()`
    - Fixed an issue that was caused by user providing an object that was not a character vector containing the variable name. It happened that users were providing matrix objects with size of few gigabytes and the function was trying to find the variable names from the matrix! Now the function complains if the provided object is not a character vector.
    - Added an argument named `regex` to accept a valid regular expression as input and apply it on the list of variables. This is for example very useful when you want to only get the info of those variables that end with "_df" without listing all names manually.
    - Added an argument named `beautify` that accepts boolean (TRUE/FALSE) with the default value of FALSE. At the moment this just adds a "[▼]" or "[▲]" to the begining of the name of the column that the sorting is based on. This feature is aiming to beautify the output and make it more user-friendly and eye-friendly. In future more details can be added to the output of the function by this. Let me know if you have any suggestions :)
    
-------

# varhandle 2.0.3

### Changes to existing functions

* `rm.all.but()`
    - Fixed a bug that was only expecting regular expression.
* `pin.na()`
    - Added the possibility to define the missingness character or value (can be more than one). So now user can define what should be concidered as missing value (e.g na.value = c(NA, " ", "."))
* `inspect.na()`
    - The added feature to `pin.na()` was also added to this function.
    - Fixed the issue that the code was breaking if the given matrix didn't have columns names.
* `check.numeric()`
    - Fixed a bug that was returning TRUE when a numeric vector with some continuous was provided along with the flag `only.integer = TRUE`. Now the function checks the entire vector when it is of class numeric or integer and the flag `only.integer` is turned on.
* `var.info()`
    - Now support showing the dimension of object with class "Matrix" from a package with the same name in the detail column of the output.
    - Now support showing the length of lists in the detail column of the output.

-------

# varhandle 2.0.2

### Changes to existing functions

* `var.info()`
    - Added progressbar and an argument to turn it on or off. Default is on.
    - Fixed a bug that was returning a warning when user was providing more than one variable name.
* `unfactor()`
    - Fixed a bug that when a vector was fed, the function was returning a warning. (now compatible with `_R_CHECK_LENGTH_1_CONDITION_`)
* `rm.all.but()`
    - Added the ability to auto detect and handle regular expression alone or in combination with variable names, so that it is more convenient for user to keep variables based on regular expression as well.

-------

# varhandle 2.0.1

### Changes to existing functions

* `pin.na()`
    - Change the type of output to data.frame to make it easier to access
      via `$`.
    - Now returns NULL in case it does not find any NA. This change has
	   been done to make it easier to combine it with `is.NULL()`
* `check.numeric()`
    - The rm.na argument has changed to na.rm in order to make it similar
      to the convention that other packages and functions are using.
    - The function now detects "-.2", "3.", "" and NA as numbers as well.
    - The default value of argument `na.rm` has changed to `FALSE` in order to
      take NAs into account.
    - An option added to ignore leading and tailing whitespace characters
      from items in vector before assessing if they can be converted to
      numeric.
* `rm.all.but()`
    - Added the ability to call garbage collection if the size of the
      removed variables exceed the new parameter `gc_limit`.
    - Added a new parameter `keep_functions` to automatically exclude all
      functions from being removed.
* `var.info()`
    - Now can handle matrix-like objects with multiple classes.


### New functions

* `inspect.na()`: This function is calls `pin.na()` and produce a human readable
                  data.frame of NA status of columns in addition to a barplot
                  and/or histogram.

-------

## varhandle 2.0.0

### Changes to existing functions
* `pin.na()`
    - Change the type of output to data.frame to make it easier to access
       via `$`
    - Now returns NULL in case it does not find any NA. This change has
       been done to make it easier to combine it with `is.NULL()`
* `check.numeric()`
    - The rm.na argument has changed to na.rm in order to make it similar
       to the convention that other packages and functions are using.
    - The function now detects `"-.2"`, `"3."`, `""` and NA as numbers as well.
    - The default value of na.rm has changed to FALSE in order to take
       NAs into account.
    - An option added to ignore leading and tailing whitespace characters
       from items in vector before assessing if they can be converted to
       numeric.
* `rm.all.but()`
    - Added the ability to call garbage collection if the size of the
       removed variables exceed the new parameter `gc_limit`.
    - Added a new parameter `keep_functions` to automatically exclude all
       functions from being removed.


### New functions

* `inspect.na()` : This function is calls pin.na and produce a human readable
                data.frame of NA status of columns in addition to a barplot.

