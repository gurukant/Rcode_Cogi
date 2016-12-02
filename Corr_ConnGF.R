raw_data <- read.csv('Conn_GF.csv', header = TRUE)
dataa<-raw_data[,2:ncol(raw_data)]

cor.test(dataa$X1., dataa$GF_12_UTF, method = "spearman")

cor.test( dataa, adjust = "none")
a <- corr.test(dataa, use = "pairwise",method = "spearman", adjust="none")

corr.test

library("psych")

write.csv(a$r,"Connectivity_Score_Corr_110416.csv")
write.csv(a$p,"Connectivity_Score_pval_110416.csv")
a$n
