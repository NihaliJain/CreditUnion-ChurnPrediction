# Date:November 24, 2018
# Analysis : Data cleaning and variable creation for performing Exploratory data analysis of credit union data
# -------------------

library(lubridate)
library(noncensus)
library(usmap)
library(data.table)


data(zip_codes)

#reading as a data frame first to autmatically add "1"/"2" next ro duplicated column names 
member = read.csv("Member Dataset.csv")

#Using data table syntax moving forward
member = as.data.table(member)
#creating age groups
member[,Age_bin := ifelse(Age < 14, "Kids", ifelse(Age < 22, "Teens",
                                                        ifelse(Age<35, "Millenials",
                                                               ifelse(Age<55, "Middle_Aged","55+"))))]

#combining diffrent loan and savings accounts to one of each kind
member[is.na(member)] = 0
member[,Loan_jul := Visa + Mortgage +Home.Equity+Other.Loan+Vehicle]
member[,Savings_jul := Checking +Savings+CD+IRA+Money.Market]
member[,Loan_aug := Visa.1 + Mortgage.1 +Home.Equity.1+Other.Loan.1+Vehicle.1]
member[,Savings_aug := Checking.1 +Savings.1+CD.1+IRA.1+Money.Market.1]
member[,Loan_sep := Visa.2 + Mortgage.2 +Home.Equity.2+Other.Loan.2+Vehicle.2]
member[,Savings_sep := Checking.2 +Savings.2+CD.2+IRA.2+Money.Market.2]

#Adding type of customer
member[,Cust_type := ifelse(ClosedDate == "", "Active", ifelse(mdy(ClosedDate) < ymd("2018-07-31"),
                                                                    "Closed_bfr_jul","Churned_Recently"))]
#Duration
member[,loyalty_months := ifelse(Cust_type== "Active",
                               interval(mdy(EarliestMemAcctDate), mdy("10-01-2018")) %/% months(1),
                               interval(mdy(EarliestMemAcctDate), mdy(ClosedDate)) %/% months(1))]

#Zip correction
member[,ZipCode_Validated := as.character(ZipCode_Validated)]
member[, zip := paste0(strrep('0',5-nchar(ZipCode_Validated)),ZipCode_Validated)]


EDA_variables = member[,.(Member_ID,zip,Age_bin,Cust_type,loyalty_months,
                          July.Trans, August.Trans,September.Trans,
                          Loan_jul,Savings_jul,Loan_aug,Savings_aug,Loan_sep,Savings_sep)]

#adding city and state from zipcode library
EDA = merge(EDA_variables,zip_codes,"zip")
write.csv(EDA,"CU_EDA_variables.csv", row.names = F)
plot_usmap(data = EDA, regions = "counties", values = "loyalty_months", lines = "blue") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Loyal customers", label = scales::comma
  ) + theme(legend.position = "right")


summary(EDA_variables)

Jul_trans = EDA[,.(Member_ID,Cust_type,Trans = July.Trans, savings = Savings_jul,loan = Loan_jul, mon = "Jul")]
Aug_trans = EDA[,.(Member_ID,Cust_type,Trans = August.Trans, savings = Savings_aug, loan = Loan_aug, mon = "Aug")]
Sep_trans = EDA[,.(Member_ID,Cust_type,Trans = September.Trans, savings = Savings_sep, loan = Loan_sep, mon = "Sep")]
all_tr = rbind(Jul_trans,Aug_trans,Sep_trans)

all_summary = all_tr[,.(sum(Trans),sum(savings),sum(loan)), by = .(mon,Cust_type)]

write.csv(all_summary,"All_sumary.csv")
