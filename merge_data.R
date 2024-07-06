rm(list=ls())
library(data.table)
#################
transac <- read.csv("transaction_dt.csv")
sales <- read.csv("sales_dt.csv")
funded <- read.csv("funded_dt.csv")

trans_sale <- merge(transac[,-1], sales[,-1], by = "retrival_ref", all = TRUE) 
allx <- merge(trans_sale, funded[,-1], by = "summary_slip", all = TRUE) 





###########################################
transaction_dt <- data.frame(cbind(c(1001:1100),sample.int(999999,100, replace = FALSE),
                                   floor(runif(n=100, min=100, max=2000)))) 
colnames(transaction_dt) <- c("merchant_ref", "retrival_ref", "other1")

sales_dt <- data.frame(cbind(transaction_dt$retrival_ref,sample.int(9999999,100, replace = FALSE),
                                   floor(runif(n=100, min=1000, max=10000)))) 
colnames(sales_dt) <- c("retrival_ref","summary_slip",  "other2")

funded_dt <- data.frame(cbind(sample.int(999999,100, replace = TRUE),
                             floor(runif(n=100, min=1000, max=10000)), sales_dt$summary_slip)) 
colnames(funded_dt) <- c("other3",  "other4", "summary_slip")
