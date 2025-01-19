This is an R package that combines Multi-Criteria Decision Analysis
and geospatial techniques to visualize a spatial fishing pressure index (FPI) 
for vessels that are not equipped with monitoring devices. 
The outcomes of FPI are combined with table G (effort by country)
and table a (landings) to estimate spatial fishing effort, landings weight and value of landings.

The `tools4MCDA` contains two demos and a Tutorial. Type,

    > file.edit(system.file("demo", "demo1.R", package = "tools4MCDA")) 
    > file.edit(system.file("demo", "demo2.R", package = "tools4MCDA")) 

to access the source codes of the demos and

    path <- system.file("doc", "tools4MCDATutorial.pdf", package = "tools4MCDA")
    system(paste0('open "', path, '"'))

to read how demo1 is implemented in a step by step approach.

CRAN
----

To install the most recent package from CRAN type:

    > install.packages("tools4MCDA")
    > library(tools4MCDA)

In case you receive an error that `tools4MCDA.rdb` is corrupt, restart R
after loading the package.

Example
-------

This is a basic example which shows you how to implement `tools4MCDA` in
the Eastern Ionian Sea:

    > demo("demo1", package = "tools4MCDA", ask = FALSE)
