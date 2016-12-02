

#domain_file = function (data){sprintf("Hello")} 

# makes all combination of the Domains data frame
combi=combn(Domains,3, simplify = FALSE)

for (i in 1:length(combi)){
  ss=combi[[i]]
  sss =stack(ss)$values
  sss=sss[sss !=""]
  print(sss)
} 



ss=combi[[1]]

# Takes all the test names in a combination
sss =stack(ss)$values

# remove empty spaces
sss=sss[sss !=""]

#get all the scores for the test in a perticular combination
data[, c(sss)]

# See how to change the file names in R
write.csv(fa$scores, file = "GF06_3_Gscore_TF")