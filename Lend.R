library(choroplethr)
library(choroplethrMaps)
library(DescTools)
library(dplyr)
library(ggplot2)
library(readr)

options(scipen=999)
setwd("D:\\train")
loanbook <- read_csv("loan.csv")

# load the state names
data(state.regions)

# merge the loan book with the state names
loanbook <- merge(loanbook, state.regions, by.x = "addr_state", by.y = "abb")


## Data overview

# print dimentions
dim(loanbook)

# print column names
colnames(loanbook)

str(loanbook)
summary(loanbook)

## Missing variables


library(readxl)

dataDictionary <- read_excel("LCDataDictionary.xlsx")

# fields available in the data dictionary
dd_names <- as.character(na.omit(dataDictionary$LoanStatNew))

# fields available in the loan book
loanbook_names <- names(loanbook)

# show the fields described in data dictionary but not in the loan book
setdiff(dd_names, loanbook_names)

Desc(loanbook$loan_amnt, main = "Loan amount distribution", plotit = TRUE)

loanbook$issue_d <- as.Date(gsub("^", "01-", loanbook$issue_d), format="%d-%b-%Y")

amnt_df <- loanbook %>% 
  select(issue_d, loan_amnt) %>% 
  group_by(issue_d) %>% 
  summarise(Amount = sum(loan_amnt))

ts_amnt <- ggplot(amnt_df, 
                  aes(x = issue_d, y = Amount))
ts_amnt + geom_line() + xlab("Date issued")



Desc(loanbook$loan_status, plotit = T)


box_status <- ggplot(loanbook, aes(loan_status, loan_amnt))
box_status + geom_boxplot(aes(fill = loan_status)) +
  theme(axis.text.x = element_blank()) +
  labs(list(
    title = "Loan amount by status",
    x = "Status",
    y = "Amount"))  




amnt_df_grade <- loanbook %>% 
  select(issue_d, loan_amnt, grade) %>% 
  group_by(issue_d, grade) %>% 
  summarise(Amount = sum(loan_amnt))

ts_amnt_grade <- ggplot(amnt_df_grade, 
                  aes(x = issue_d, y = Amount))
ts_amnt_grade + geom_area(aes(fill=grade)) + xlab("Date issued")



state_by_value <-
loanbook %>% group_by(region) %>%
  summarise(value = sum(loan_amnt, na.rm=TRUE))

ggplot(state_by_value, aes(long, lat, group = statename)) +
  geom_polygon(aes(fill = revenue))

state_by_volume <-
loanbook %>% group_by(region) %>%
  summarise(value = n())

state_choropleth(state_by_volume, title = "Volume by State")




