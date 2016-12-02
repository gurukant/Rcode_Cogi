raw_data <- read.csv('Scores_final.csv', header = TRUE)
data1 <- raw_data[,2:ncol(raw_data)]

# apply log transformation 3back, Logic, Math, Motor R, Motor L
log_cols <- c(1, 8, 9, 15, 16)
data2 <- data1
for(i in log_cols){
  data2[i] <- log(data2[, i])
}
data3<-data2

#apply tranform for VS col 11
data3[, 11] <- log(26- data3[, 11])

res_list <- list()
res_combinations <- NULL
max_list <- NULL
max_indexes <- NULL
TOTAL = 18
START = 1
END = 18
for (m in START:END){
  print(paste(c("processing m = ", m), collapse = " "))
  res = array(0, dim=choose(TOTAL, m))
  cols <- combn(seq(1, ncol(data)), m, FUN = NULL, simplify = TRUE)
  cur_max <- 0
  all_std_dev <- matrix(list(), nrow=ncol(cols), ncol=2)
  #cur_max_var <- 0
  for (i in 1:ncol(cols)){
    pca <- prcomp(data[,cols[, i]], center = TRUE, scale. = TRUE)
    prop_of_var = pca$sdev^2/sum(pca$sdev^2)
    max_var = max(prop_of_var)
    max_pca = max(pca$sdev)
    all_std_dev[[i,1]] <- max(pca$sdev)
    all_std_dev[[i,2]] <- cols[, i]
    res[i] <- max_pca
    if(max_pca > cur_max){ #maintain a list of maximums for every m 
      cur_max = max_pca
      max_index = c(m,i,cols[, i]) #save index along with column information
    }
  }
  print(paste(c("max index =", max_index, ", max std dev= ", cur_max), collapse = " "))
  if(max_index && cur_max_var){
    max_list <- c(max_list, list(max_index, cur_max))
    max_indexes <- c(max_indexes, cur_max)
  }
  df <- as.data.frame(all_std_dev)
  df2 <- as.data.frame(lapply(df, unlist))
  res_combinations <- c(res_combinations, tail(na.omit(df[order(df2$V1),]), decreasing = TRUE),5))
res_list <- c(res_list, res)
}










all_std_dev
df <- as.data.frame(all_std_dev)
df2 <- as.data.frame(lapply(df, unlist))
na.omit(df[order(df2$V1),])

order(df2$V1)[1:length(order(df2$V1))]

typeof(order(df2$V1))
?unlist

order(df2$V1)
df[95,]

sort(all_std_dev)
sort.list(all_std_dev[,1])
list(all_std_dev[,1])


print(res_combinations)

print(max_indexes)
plot(max_indexes)
#print(max_list)
print(res_list)
########################
#TRIAL 
pca <- prcomp(data[, c(2, 3, 4, 7, 8, 9, 10, 11, 12, 13)], center = TRUE, scale. = TRUE)
summary(pca)
pca$rotation


c(2, 3, 4, 7, 8, 9, 10, 11, 12, 13)
c(2, 3, 4, 7, 8, 9, 10, 11, 12, 13, 17, 18 )
c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18 )

for (i in 6:13){
  fit <- factanal(data[, c(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)], i , rotation = "varimax" )
  print(paste(c("************ Number of Factors = ",i), collapse = " "))
  print(fit)  
}

?factanal

summary(fit)
fit$PVAL
fit <- factanal(data[,c()], 5, rotation = "varimax" )



apply(data, 2, qqnorm.default)

qqnorm(data1$VS,ylab='VS')
qqline(data1$VS)


qqnorm.default(data2$X3Back)
qqline(data2$X3Back)
data1$BT

data<-data1 #UTF
data<-data3 #TF

#M=7 Math, Lang, Exe #1
fa=factanal(x = data[, c(2, 3, 4, 7, 8, 9, 10)], factors = 3, rotation = "none", scores="regression")
fa1=factanal(x = data[, c(2, 3, 4, 7, 8, 9, 10)], factors = 3, rotation = "promax", scores="regression")

#M=6 Math, Lang, Music #1
fa=factanal(x = data[, c(3, 7, 8, 9,17,18)], factors = 3, rotation = "none", scores="regression")
fa1=factanal(x = data[, c(3, 7, 8, 9, 17,18)], factors = 3, rotation = "promax", scores="regression")

#M=6 Math, Lang, Motor #2
fa=factanal(x = data[, c(3, 7, 8, 9,15,16)], factors = 3, rotation = "none", scores="regression")
fa1=factanal(x = data[, c(3, 7, 8, 9, 15,16)], factors = 3, rotation = "promax", scores="regression")

#M=7 Math, Lang, Mem, VS #2
fa=factanal(x = data[, c(1,3, 7, 8, 9,11,14)], factors = 3, rotation = "none", scores="regression")
fa1=factanal(x = data[, c(1,3, 7, 8, 9, 11,14)], factors = 3, rotation = "promax", scores="regression")

#M=6 Math, Lang, Fluency #3
fa=factanal(x = data[, c(3, 7, 8, 9,12,13)], factors = 2, rotation = "none", scores="regression")
fa1=factanal(x = data[, c(3, 7, 8, 9, 12,13)], factors = 2, rotation = "promax", scores="regression")

#M=9 Math, Lang+Flu, Exe
fa =factanal(x = data[, c(2, 3, 4, 7, 8, 9, 10, 12, 13)], factors = 3, rotation = "none", scores="regression")
fa1=factanal(x = data[, c(2, 3, 4, 7, 8, 9, 10, 12, 13)], factors = 3, rotation = "promax", scores="regression")

#M=10 Math, Lang+Flu, Exe, VS
fa =factanal(x = data[, c(2, 3, 4, 7, 8, 9, 10, 11, 12, 13)], factors = 4, rotation = "none", scores="regression")
fa1=factanal(x = data[, c(2, 3, 4, 7, 8, 9, 10, 11, 12, 13)], factors = 4, rotation = "promax", scores="regression")

#M=12 Math, Lang+Flu, Exe, VS, Music
fa =factanal(x = data[, c(2, 3, 4, 7, 8, 9, 10, 11, 12, 13,17,18)], factors = 5, rotation = "none", scores="regression")
fa1 =factanal(x = data[, c(2, 3, 4, 7, 8, 9, 10, 11, 12, 13,17,18)], factors = 5, rotation = "promax", scores="regression")

#M=14 MAth, Lang+ Flu, Exe, VS, Music, Mem
fa =factanal(x = data[, c(1,2, 3, 4, 7, 8, 9, 10, 11, 12, 13, 14, 17, 18)], factors = 6, rotation = "none",scores="regression")
fa1 =factanal(x = data[, c(1,2, 3, 4, 7, 8, 9, 10, 11, 12, 13, 14, 17, 18)], factors = 6, rotation = "promax",scores="regression")

fa =factanal(x = data[, c(1,2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)], factors = 8, rotation = "promax",scores="regression")

write.csv(fa$scores, file = "GF06_3_Gscore_TF")
write.csv(fa1$scores, file = "GF06_3_Dscore_TF")
