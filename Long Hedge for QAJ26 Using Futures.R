# =========================
# QAJ26 Brent (CME BZ) 
# Hedge long 10 contracts for 10,000 bbl
# DATES enter 2025-02-25, exit 2026-02-20
# =========================

# files 
csv_file <- "qaj26_daily_historical-data-download-02-25-2026.csv"

#10000 barrels /1000 barrels per contract
n_contracts  <- 10  
# num barrels per contract
contract_size <- 1000 
#from barchart linked in word doc
init_margin_per_contract  <- 6343  
maint_margin_per_contract <- 5767 

start_date <- as.Date("2025-02-25")
end_date   <- as.Date("2026-02-20")

# --- 1) load & prep data (base R only) 
df <- read.csv(csv_file, stringsAsFactors = FALSE)
# Barchart uses date format M/D/YYYY 
df$Date <- as.Date(df$Time, format = "%m/%d/%Y")

# keep rows in hedge window and order by date
w <- df[df$Date >= start_date & df$Date <= end_date, ]
w <- w[order(w$Date), ]

# use latest as the daily settlement proxy
settle <- as.numeric(w$Latest)

#  compute daily changes P&L for a long position 
#
# delta price (first day has NA; set its P&L = 0)
#daily PNL=(Ft-Ft-1)(contract size)
delta <- c(NA, diff(settle))
daily_pnl_per_contract <- delta * contract_size
daily_pnl_per_contract[1] <- 0  # no P&L on entry day
daily_pnl_total <- daily_pnl_per_contract * n_contracts
#cumulative profit calculation shows total gain.loss on the hedge
cum_pnl_total   <- cumsum(daily_pnl_total)

#Inital margian total (6346)(10)=36430
#Maitence margin total (5767)(10)
init_margin_total  <- init_margin_per_contract  * n_contracts
maint_margin_total <- maint_margin_per_contract * n_contracts

margin_bal <- numeric(length(daily_pnl_total))
margin_call <- numeric(length(daily_pnl_total))

# start with the initial margin on entry day
margin_bal[1] <- init_margin_total
#add daily varition margin
margin_call[1] <- 0

# each subsequent day: apply daily P&L; if below maintenance, top to initial
for (i in 2:length(daily_pnl_total)) {
  # apply variation margin (yesterday’s balance +/- today’s P&L)
  bal <- margin_bal[i - 1] + daily_pnl_total[i]
  
  call <- 0
  #checking the maitnence margin
  if (bal < maint_margin_total) {
    call <- init_margin_total - bal   # top-up back to initial
    bal  <- bal + call
  }
  margin_bal[i]  <- bal
  margin_call[i] <- call
}

# build a Hull-style table and save it
out <- data.frame(
  Date = w$Date,
  Settle = settle,
  Delta = delta,
  DailyPnL_total = daily_pnl_total,
  CumPnL_total = cum_pnl_total,
  MarginBalance = margin_bal,
  MarginCall = margin_call
)

write.csv(out, "QAJ26_HullTable_2025-02-25_to_2026-02-20.csv", row.names = FALSE)

#  Answers and quick checks
entry_price <- out$Settle[1]
exit_price  <- out$Settle[nrow(out)]
pnl_from_diff <- (exit_price - entry_price) * contract_size * n_contracts
pnl_from_sum  <- sum(out$DailyPnL_total)

# largest margin call
#and number of margin calls 
idx_max_call <- which.max(out$MarginCall)
largest_call_date <- out$Date[idx_max_call]
largest_call_amt  <- out$MarginCall[idx_max_call]

cat("\n--- RESULTS ---\n")
cat("Entry settle (", format(start_date), "): $", round(entry_price, 2), "\n", sep = "")
cat("Exit  settle (", format(end_date),   "): $", round(exit_price, 2),  "\n", sep = "")
cat("Total P&L (diff method): $", round(pnl_from_diff, 2), "\n", sep = "")
cat("Total P&L (sum daily)  : $", round(pnl_from_sum, 2),  "\n", sep = "")
cat("Largest margin call on ", format(largest_call_date),
    " for $", format(round(largest_call_amt, 2), big.mark=","), "\n", sep = "")
cat("Number of margin calls : ", sum(out$MarginCall > 0), "\n", sep = "")
cat("Table saved to: QAJ26_HullTable_2025-02-25_to_2026-02-20.csv\n")