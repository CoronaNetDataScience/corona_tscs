# Pull new data from NYTimes/Coronavirus project and compile new model

require(dplyr)
require(tidyr)


system2("rsync",args=c("-avz","~/covid-tracking-data/data/","~/corona_tscs/data"))
system2("rsync",args=c("-avz","~/covid-19-data/*.csv","~/corona_tscs/data"))
system2("rsync",args=c("-avz","~/corona_tscs/data/*.csv","~/saudiwin.github.io/content/post/data/"))


blogdown::build_site()



# copy models back


system2("rsync",args=c("-avz","~/saudiwin.github.io/content/post/data/*.rds","~/corona_tscs/data/"))