install.packages("rinat")
yes
library(rinat)
monarchs <- get_inat_obs(query = "Monarch Butterfly")
unique(monarchs$scientific_name)
bounds <- c(38.44047, -125, 40.86652,)