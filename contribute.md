## Make this package better!

### Report a bug!

If you discover a problem with the workings of `flyover`, please restart your R session and try your code again.  If the issue persists, I'd be grateful if
you would [file an issue](https://github.com/ataustin/flyover/issues).  The
chances of a quick fix are higher if you can make it super simple for me to
identify the cause of the problem:

* let me know what operating system you're using, and your version
* post the output of `sessionInfo()` in the body of your issue
* describe your inputs and what the expected ouput was, along with any warnings
or errors you encounter
* **most importantly**, include a reproducible example in your post.  A reproducible example is a bit of code that you can run, _from a fresh R session_, and reproduce your issue.  It should not reference any scripts or data that I don't have access to.  For more information, please see [this Tidyverse article](https://www.tidyverse.org/help/) and consider using the `reprex` package.


### Suggest an improvement!

If you have a suggestion for how to improve the package, I'd love to hear about it!  Please [file an issue](https://github.com/ataustin/flyover/issues) and describe your idea.

### Make a pull request!

If you have an improvement and like to get your hands dirty, please fork this repo and submit a pull request when you've made your changes.  I ask that you kindly consider the following when making a PR:

* please do your best to adhere to existing code style.  I generally obey guidelines found in the [Tidyverse style guide](https://style.tidyverse.org/).
* please ensure new features are documented in roxygen comments and vignettes, as applicable.  Please render the help pages (`devtools::document()`) and the documentation (`pkgdown::build_site()`) as applicable.
* please ensure new functions are thoroughly tested and that all tests pass (`devtools::test()`).
* if you are directly addressing an open issue, please add `Closes #xx` to the top of your comments so the issue is tied to your PR.

Thank you for your contribution!