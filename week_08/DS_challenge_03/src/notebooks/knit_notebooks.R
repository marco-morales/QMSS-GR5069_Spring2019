#  #######################################################################
#       File-Name:      knit_notebooks.R
#       Version:        R 3.5.2
#       Date:           March 3, 2019
#       Author:         MM <marco.morales@columbia.edu>
#       Purpose:        knit R Markdown notebooks seamlessly from a script
#       Input Files:    sample_notebook.Rmd
#       Output Files:   sample_notebook.md
#       Data Output:    NONE
#       Dependencies:   NONE
#       Required by:    NONE
#       Status:         COMPLETED
#       Machine:        Mac laptop
#  #######################################################################


library(ezknitr)
library(here)


# ::::::::::::::::::::::::::: USEFUL DEFINITIONS ::::::::::::::::::::::::::::::

# define output files and locations
report_1 <- here("src", "notebooks", "sample_notebook.Rmd")


# ::::::::::::::::::::::::::: USEFUL DEFINITIONS ::::::::::::::::::::::::::::::

# define parameters for your report
ezknit(file = report_1,
       out_dir = "reports",  # output path relative to `here()` location
       fig_dir = "images",   # figure path relative to `out_dir`
       verbose = TRUE,
       keep_html = FALSE)
