# Download clean data from OSF
# install.packages("devtools")
#devtools::install_github("Between-the-Fjords/dataDownloader")
library(dataDownloader)

node <- "npfa9"

# 3_community
get_file(node = node,
         file = "seedclim.2020.4.15.zip",
         path = "community/data/",
         remote_path = "3_Community_data")
