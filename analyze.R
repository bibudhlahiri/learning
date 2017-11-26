#Track if there are one or more big expenses for the month
map_big_expenses_for_month <- function(big_expenses, salary_date, month_end_date)
{
  big_expenses_for_month <- subset(big_expenses, ((Tran.Date >= salary_date) & (Tran.Date <= month_end_date)))
  ifelse((nrow(big_expenses_for_month) == 0), 0, sum(big_expenses_for_month$DR))
}

map_cc_payments_for_month <- function(credit_card_payments, salary_date, month_end_date)
{
  cc_payments_for_month <- subset(credit_card_payments, ((Tran.Date >= salary_date) & (Tran.Date <= month_end_date)))
  ifelse((nrow(cc_payments_for_month) == 0), 0, sum(cc_payments_for_month$DR))
}

map_cfos_for_month <- function(credits_from_other_sources, salary_date, month_end_date)
{
  cfos_for_month <- subset(credits_from_other_sources, ((Tran.Date >= salary_date) & (Tran.Date <= month_end_date)))
  ifelse((nrow(cfos_for_month) == 0), 0, sum(cfos_for_month$CR))
}

map_reg_credit_for_month <- function(regular_credits, salary_date, month_end_date)
{
  reg_credit_for_month <- subset(regular_credits, ((Tran.Date >= salary_date) & (Tran.Date <= month_end_date)))
  ifelse((nrow(reg_credit_for_month) == 0), 0, sum(reg_credit_for_month$CR))
}

get_monthly_expense <- function(statement, salary_deposit_ser_no, month_end_ser_no)
{
  debit_txns <- subset(statement, (SRL.NO > salary_deposit_ser_no) & (SRL.NO <= month_end_ser_no) & (!is.na(DR)) & 
                       (!(grepl("073691900000902", PARTICULARS))))
  sum(debit_txns$DR)
}

get_atm_withdraw_for_month <- function(atm_withdrawals, salary_date, month_end_date)
{
  atm_withdraw_this_month <- subset(atm_withdrawals, ((Tran.Date >= salary_date) & (Tran.Date <= month_end_date)))
  sum(atm_withdraw_this_month$DR)
}

statement <- read.csv("C:\\Users\\bibudh.lahiri\\Financials\\916010009890335.csv")
statement$SRL.NO <- as.numeric(as.character(statement$SRL.NO))
salary_deposits <- subset(statement, grepl("SALARY PAYMENT", PARTICULARS) & (as.numeric(substr(as.character(Tran.Date), 1, 2)) >= 28))
ser_nums_salary_deposits <- salary_deposits$SRL.NO
ser_nums_preceding_salary_deps <- ser_nums_salary_deposits - 1
lines_preceding_salary_deps <- subset(statement, (SRL.NO %in% ser_nums_preceding_salary_deps))
drops <- c("CHQNO", "SOL", "X", "X.1", "X.2")
salary_deposits <- salary_deposits[ , !(names(salary_deposits) %in% drops)]
salary_deposits$line_type <- "salary"
lines_preceding_salary_deps <- lines_preceding_salary_deps[, !(names(lines_preceding_salary_deps) %in% drops)]
lines_preceding_salary_deps$line_type <- "month_end"
relevant <- rbind(salary_deposits, lines_preceding_salary_deps)
relevant$Tran.Date <- as.Date(relevant$Tran.Date, "%d-%m-%Y")
relevant <- relevant[order(relevant$SRL.NO),] 
#Should start with a salary deposit and end with a month_end
if (relevant[1, "line_type"] != "salary")
  relevant <- relevant[-1,]
if (relevant[nrow(relevant), "line_type"] != "month_end")
  relevant <- relevant[-nrow(relevant),]

salary_deposits <- subset(relevant, (line_type == "salary"))
drops <- c("DR", "line_type")
salary_deposits <- salary_deposits[ , !(names(salary_deposits) %in% drops)]

colnames(salary_deposits) <- c("salary_deposit_ser_no", "salary_date", "salary_desc", "salary_amt", "bal_after_salary")

month_ends <- subset(relevant, (line_type == "month_end"))
drops <- c("CR", "line_type")
month_ends <- month_ends[ , !(names(month_ends) %in% drops)]
colnames(month_ends) <- c("month_end_ser_no", "month_end_date", "month_end_desc", "month_end_txn_amt", "month_end_bal")

relevant <- cbind(salary_deposits, month_ends)

#TBD: monthly_expense should be calculated as total debit, because bal_after_salary is not reliable as credits from other sources 
#can come in the middle of the month
#relevant$monthly_expense <- relevant$bal_after_salary - relevant$month_end_bal
relevant$monthly_expense <- apply(relevant, 1, 
                                   function(row)get_monthly_expense(statement, as.numeric(row["salary_deposit_ser_no"]), 
								                                    as.numeric(row["month_end_ser_no"])))

credits_from_other_sources <- subset(statement, ((!is.na(CR)) & (!(grepl("SALARY PAYMENT", PARTICULARS)))))
drops <- c("CHQNO", "SOL", "X", "X.1", "X.2")
credits_from_other_sources <- credits_from_other_sources[ , !(names(credits_from_other_sources) %in% drops)]
credits_from_other_sources$Tran.Date <- as.Date(credits_from_other_sources$Tran.Date, "%d-%m-%Y")
relevant$cfos_for_month <- apply(relevant, 1, 
                                       function(row)map_cfos_for_month(credits_from_other_sources, row["salary_date"], row["month_end_date"]))
									   
#regular_credits are credits from other sources except lump-sum credits made by ourselves for big payments
regular_credits <- subset(statement, (!is.na(CR)) & (!(grepl("SALARY PAYMENT", PARTICULARS))) & 
                                     (!(grepl("IMPETUS INFOTECH", PARTICULARS))) & 
									 (!(grepl("By Clg", PARTICULARS))) &
									 (!(grepl("BIBUDH LAHIRI", PARTICULARS))))
drops <- c("CHQNO", "SOL", "X", "X.1", "X.2")
regular_credits <- regular_credits[ , !(names(regular_credits) %in% drops)]
regular_credits$Tran.Date <- as.Date(regular_credits$Tran.Date, "%d-%m-%Y")
relevant$reg_credit_for_month <- apply(relevant, 1, 
                                       function(row)map_reg_credit_for_month(regular_credits, row["salary_date"], row["month_end_date"]))

atm_withdrawals <- subset(statement, (grepl("ATM-CASH", PARTICULARS)))
drops <- c("CHQNO", "SOL", "X", "X.1", "X.2")
atm_withdrawals <- atm_withdrawals[ , !(names(atm_withdrawals) %in% drops)]
atm_withdrawals$Tran.Date <- as.Date(atm_withdrawals$Tran.Date, "%d-%m-%Y")
relevant$atm_withdrawals <- apply(relevant, 1, 
                                  function(row)get_atm_withdraw_for_month(atm_withdrawals, row["salary_date"], row["month_end_date"]))

big_expenses <- subset(statement, ((DR >= 5000) & (!(grepl("INB/CREDIT CARD/", PARTICULARS))) & 
				(!(grepl("KOTAK", PARTICULARS))) &
				(!(grepl("Shikha Rastog", PARTICULARS))) &
				(!(grepl("Utkarsha L", PARTICULARS))) &
				(!(grepl("010791900000763", PARTICULARS))) & 
				(!(grepl("17197630000208", PARTICULARS))) &
				(!(grepl("Footprints Ch", PARTICULARS))) &
				(!(grepl("ATM-CASH", PARTICULARS))) & 
				(!(grepl("073691900000902", PARTICULARS)))
				))
drops <- c("CHQNO", "SOL", "X", "X.1", "X.2")
big_expenses <- big_expenses[ , !(names(big_expenses) %in% drops)]
big_expenses$Tran.Date <- as.Date(big_expenses$Tran.Date, "%d-%m-%Y")
cat(paste("Big expenses add up to ", sum(big_expenses$DR), "\n", sep = "")) #28,89,992.5

#Two big expenses, 134739 and 42630, were transfers to Madhu as her salary for the initial months got into Bibudh's account

relevant$big_exp_for_month <- apply(relevant, 1, function(row)map_big_expenses_for_month(big_expenses, row["salary_date"], row["month_end_date"]))
relevant$monthly_expense_without_big <- relevant$monthly_expense - relevant$big_exp_for_month
mewb <- relevant$monthly_expense_without_big
mewb <- mewb[mewb >= 0]
cat(paste("Average monthly expense without the big expenses is ", sum(mewb)/15, "\n", sep = ""))  #77,419.97
#Rs. 88,269.51 is the average monthly expense, excluding big one-time expenses	

relevant$savings <- relevant$salary_amt + relevant$reg_credit_for_month - relevant$monthly_expense_without_big

credits_from_madhu <- subset(statement, ((!is.na(CR)) & (grepl("MADHUMITA SAHA", PARTICULARS))))

#TBD: The credit card-related results should be ignored for now, becuase the billing cycle for the credit card
#is not same as the calendar month
credit_card_payments <- subset(statement, (grepl("INB/CREDIT CARD/xxxxxxxxxxxx9266", PARTICULARS)))
cat(paste("Average monthly expense on credit card is ", mean(credit_card_payments$DR), "\n", sep = "")) #Rs. 37,067.81

drops <- c("CHQNO", "SOL", "X", "X.1", "X.2")
credit_card_payments <- credit_card_payments[ , !(names(credit_card_payments) %in% drops)]
credit_card_payments$Tran.Date <- as.Date(credit_card_payments$Tran.Date, "%d-%m-%Y")
relevant$cc_payment_for_month <- apply(relevant, 1, 
                                       function(row)map_cc_payments_for_month(credit_card_payments, row["salary_date"], row["month_end_date"]))
relevant$expenses_not_on_cc <- relevant$monthly_expense_without_big - relevant$cc_payment_for_month

cat(paste("Total savings (from regular income and expenses) is ", sum(relevant$savings), 
          ", average monthly savings = ", mean(relevant$savings), "\n", sep = ""))  #99,952.57

#Down payment for home and car added up to 1054500 + 527200 + 695000 = 22,76,700

drops <- c("salary_desc", "bal_after_salary", "month_end_desc", "month_end_txn_amt", "month_end_bal", "cc_payment_for_month", "expenses_not_on_cc",
           "salary_deposit_ser_no", "month_end_ser_no")
relevant <- relevant[ , !(names(relevant) %in% drops)]
write.csv(relevant, file = "C:\\Users\\bibudh.lahiri\\Financials\\report.csv")


