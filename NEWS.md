# varhandle 2.0.2

###Changes to existing functions

* `var.info()`
    - Added progressbar and an argument to turn it on or off. Default is on.
    - Fixed a bug that was returning a warning when user was providing more than one variable name.
* `unfactor()`
    - Fixed a bug that when a vector was fed, the function was retuning a warning. (now compatible with `_R_CHECK_LENGTH_1_CONDITION_`)
* `rm.all.but()`
    - Added the ability to auto detect and handle regular expression alone or in combination with varibale names, so that it is more convenient for user to keep variables based on regular expression as well.

-------

# varhandle 2.0.1

###Changes to existing functions

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
