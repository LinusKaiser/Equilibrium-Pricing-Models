

SP500_Comps <- GetSP500Stocks()
str(SP500_Comps)

View(SP500_Comps)


SP500_Comps$GICS.Sector <- as.factor(SP500_Comps$GICS.Sector)
SP500_Comps$GICS.Sub.Industry <- as.factor(SP500_Comps$GICS.Sub.Industry)

nlevels(SP500_Comps$GICS.Sector)
nlevels(SP500_Comps$GICS.Sub.Industry)
levels(SP500_Comps$GICS.Sector)
levels(SP500_Comps$GICS.Sub.Industry)


SP500_returns <- BatchGetSymbols(ticker = "^GSPC", first.date = Sys.Date()-365, last.date = Sys.Date(), freq.data = "daily")
SP500_stock_returns <- BatchGetSymbols(ticker = SP500_Comps$Tickers, first.date = Sys.Date()-365, last.date = Sys.Date(), freq.data = "daily", cache.folder = file.path(tempdir(), "SP500_stock_returns"))

str(SP500_stock_returns$df.tickers)

View(SP500_stock_returns$df.tickers)
tq_performance_fun_options()


just_SP500_returns <- select(SP500_returns$df.tickers, ref.date, ret.closing.prices)
grouped_SP500_stock_returns <- SP500_stock_returns$df.tickers %>%
  group_by(ticker)
  

RaRb <- left_join(grouped_SP500_stock_returns, just_SP500_returns, by = c("ref.date" = "ref.date"))
SP500_Betas <- tq_performance(RaRb,Ra = ret.closing.prices.x, Rb = ret.closing.prices.y, performance_fun = CAPM.beta)


#Mechanism to calculate stock specific returns
#daywise_buy/sell_indicator
#
#Indicator
#Abnormal_daily_return-correlation_delta_(*5*)_days
#

indicator_df <- select(RaRb,ticker,ref.date,ret.closing.prices.x,ret.closing.prices.y) 
colnames(indicator_df) <- c("ticker", "ref.date", "ret.closing.prices.stock", "ret.closing.prices.SP")


abnormal_daily_returns_df <- indicator_df %>% 
  select(ticker, ref.date, ret.closing.prices.stock, ret.closing.prices.SP) %>% 
  ungroup() %>% 
  mutate(ret.abnormal = indicator_df$ret.closing.prices.stock-indicator_df$ret.closing.prices.SP) %>%
  group_by(ticker)

correlation_delta_5_days
correlation_vector <- vector(length = 0)
for (row in 1:length(abnormal_daily_returns_df$ret.closing.prices.SP)){
  if ((row-5) < 1){
    counter <- 1
  }
  print(row)
  if (row>1){
    correlation_5_days <- cor(abnormal_daily_returns_df$ret.closing.prices.SP[counter:row],abnormal_daily_returns_df$ret.abnormal[counter:row],use="complete.obs")
    correlation_vector <- append(correlation_vector,correlation_5_days)
  }
}
cor_returns_df <- abnormal_daily_returns_df %>% mutate(correlation = correlation_5_days)
print(cor_returns_df)
  
abnormal_daily_returns_df$ret.closing.prices.SP[1:3]

indicator_df <- mutate(ungroup(indicator_df), ret.abnormal = indicator_df$ret.closing.prices.stock-indicator_df$ret.closing.prices.SP, Indicator = abnormal_daily_returns_df$ret.abnormal-correlation_delta_5_days)


for (row in 1:length(abnormal_daily_returns_df$ret.closing.prices.SP)){
  print(row)
}


cor(abnormal_daily_returns_df$ret.closing.prices.SP[1:1],abnormal_daily_returns_df$ret.abnormal[1:1],use="complete.obs")
