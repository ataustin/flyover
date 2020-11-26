# flyover
Painlessly generate high-level visual comparisons of distributions between groups or data sets.


## Purpose
This package is for you if:

* you are dealing with many variables whose distributions may differ in important ways among groups;
* you have refactored a data process and want to ensure that the new data is acceptably similar to the old data;
* you are pulling data on a regular schedule and need to monitor data drift;
* you wish to examine data quality visually rather than relying on statistical tests and thresholds.

`flyover` provides utilities to quickly generate and organize plots of distributions split by a grouping variable.


## How it works
This package is largely powered by [ggplot2](https://ggplot2.tidyverse.org/) for building plots, and the awesome [trelliscopejs](https://hafen.github.io/trelliscopejs/) by [Ryan Hafen](https://ryanhafen.com/) for providing a viewer to navigate them.

For detailed instructions, see the [documentation](https://ataustin.github.io/flyover/).


## Install
`devtools::install_github("ataustin/flyover")`

Note that the dependencies are large (especially trelliscopejs) and may take a long time to install.


## Quickstart
A typical `flyover` workflow has the following steps:

1. Combine different data sets into a single table.
2. Apply a plotting function to the columns of the table.
3. Build a display to navigate the plots.

If you are comparing distribtions of variables between an old data process and a new one, your workflow might look like this:

```
old_data <- read.csv("old-data.csv")
new_data <- read.csv("new-data.csv")

enlist_data(old_data, new_data, names = c("old data", "new data")) %>%
  build_plots(flyover_histogram) %>%
  build_display(display_name = "histograms", output_dir = "display-hist")
```

Get started with the articles in the [documentation](https://ataustin.github.io/flyover/) or jump right into the gallery of [displays](https://ataustin.github.io/flyover/articles/05-gallery.html).


## Tips
If you build a display from the console or as a batch job from an R script, you can point your browser directly at the output directory to render the display.  You can also render the display inside an R Markdown document by making the call to `build_display` the last line of a code chunk (ensure the output directory specification is a relative file path, or you will get a knitting error).


## Contributing and reporting problems

Please see `contribute.md` (coming soon!).