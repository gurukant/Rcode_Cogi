data[1,7]

raw_data <- read.csv('Scores_final.csv', header = TRUE)
Domains <- read.csv('Domains_details.csv', header = TRUE, stringsAsFactors=FALSE)

data<-raw_data
a=colnames(data)
n=9
for (i in 1:n-2){
  for (j in i+1:18-1){
    for (k in j+1:18){
      cat (Domains[i])
      cat (Domains[j])
      cat (Domains[k])}
}
}


i=1
j=2
k=3
cat(data[1,i], data[1,j], data [1,k])

a=colnames(data)
data$X3Back
aa=a[2]
data$aa

a <-colnames(data)
n<-sprintf("data$%s", a[4])
eval(parse(text=n))

domain_file(data)
mean()
mean
