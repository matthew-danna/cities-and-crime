##### EXAMPLE 1
# install a package
install.packages('devtools') # this function installs packages you need
# run a function from devtools
devtools::install_github("brooke-watson/BRRR")
# load a library
library(BRRR)
skrrrahh(0)
skrrrahh_list()
skrrrahh(4)

##### EXAMPLE 2
install.packages('gtrendsR')
library(gtrendsR)
# run a query
trends <- gtrends("flan", geo = "US-MT", "all")
# graph it
plot(trends)

trends2 <- gtrends(c("barbie","oppenheimer","flan"), geo = "US",
                   time = "today 12-m")
plot(trends2)


