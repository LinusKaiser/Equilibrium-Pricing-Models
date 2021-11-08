

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

Ra <- SP500_stock_returns$df.tickers %>%
  group_by(ticker) %>%
  tq_performance(Ra = ret.closing.prices, Rb = Null, performance_fun = CAPM.beta)
