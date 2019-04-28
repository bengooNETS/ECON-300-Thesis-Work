##Code for outputting the random numbers seen by subjects in ambiguous treatments
set.seed(1112019)
##Always a winner
aw = round(rnorm(20, 11, 0.40), digits = 2)
awString = rep(NA, 20)

for (i in 1:20) {
  awString[i] = paste("$", aw[i], sep = "")
}

awString

##Potential Loss
pl = round(c(rnorm(5, 9, 0.5), rnorm(15, 11.66, 0.5)), digits = 2)
plString = rep(NA, 20)

for (i in 1:20) {
  plString[i] = paste("$", pl[i], sep = "")
}

sample(plString, 20)
