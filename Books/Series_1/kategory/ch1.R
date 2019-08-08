#Load file

Trafic <- read.csv("TAccident.csv", header = TRUE)

head(Trafic)

TAplace <- matrix(c(3327,22196,2947,40224), nrow = 2, dimnames = 
list(Place = c("country", "urban"), Injury = c("Yes", "No")))

TAplace
