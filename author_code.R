#AUTHOR: Athira Das

getwd()
# Save the data file to a location on your hard drive and specify the path here (Windows systems use forward slashes)
dir_path <- "D:/MSBA/Fall2016/IDS564/labs/regular lab 4/"
setwd(dir_path)
# clear everything out of memory
rm(list=ls())  

## Load package
library(igraph)

infile<-"Macrae_table5.5_mjbook.csv"

infile2<-"Coauthorship_GoyalEtAl.csv"

infile3<-"HamRadioOperators_Killworth.csv"

macrae_frame=read.csv(infile, header = TRUE, sep = ",")
macrae_frame$no <-macrae_frame$Number.of.prisoners

goyal_frame=read.csv(infile2, header = TRUE, sep = ",")
goyal_frame$no <-goyal_frame$Number.of.authors

ham_frame=read.csv(infile3, header = TRUE, sep = ",")
ham_frame$no <-ham_frame$Number.of.Operators





#Function to find the beta, alpha, alpha1..
regress<- function(df, alpha)
{
  df$tot <- df$Degree*df$no
  print (df$tot)
  df$degreecont <- df$no/sum(df$no)
  df$cumsum <- cumsum(df$degreecont)
  df$y <- log(1-df$cumsum)
  df$y[which(df$y==-Inf)] = NA
  avg <- sum(df$tot) / sum(df$no)
  m <- avg /2 
  X <- 2/(1-alpha)
  df$x <- log(df$Degree+(m*X*alpha))
  df$x[which(df$x==-Inf)] = NA
  reg = lm(y ~ x, data=df)
  s <- summary(reg)
  print (s);
  beta <- reg$coefficients["x"]
  alpha1 <- (beta + 2) / beta
  #print (paste("alpha 0 -> ",alpha,"alpha1 ->",alpha1,sep=" "))
  return(cbind(alpha,alpha1))
}


alpha0 <- 0.11
alpha1 <- 0.10
alphas = c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.99,0.999)

#For prisoner data
regress(macrae_frame, alpha0)
regress(macrae_frame, alpha1)
alpha1_final <- lapply(alphas, function(x) {
                                regress(macrae_frame, x)
                              })
alpha1_final

#for co authorship data
regress(goyal_frame, alpha0)
regress(goyal_frame, alpha1)
alpha2_final <- lapply(alphas, function(x) {
  regress(goyal_frame, x)
                                })
alpha2_final

#for ham radio data
regress(ham_frame, alpha0)
regress(ham_frame, alpha1)
alpha3_final <- lapply(alphas, function(x) {
                                  regress(ham_frame, x)
                                })
alpha3_final






