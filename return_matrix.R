#Matrix Function
#
#Arguments: a datafrane of a list of stocks and a dataframe of an sinlge index of the BatchGetSymbols package
#Body: splits up the stocks' returns into a matrix cloumnwise and adds missing results if row number of a
#      single stock's returns is less than the number of rows of the index's returns; added returns
#      are derived based on a normal distribution using the sample mean and standard deviation of the particular stock
#Returns: a list of three data frames, one that holds the matrix of returns (each column one stock's returns)
#         , one that holds the stock tickers that didn't meet the requirements for being included, and one that
#         contains the tickers of the stocks where returns have been added using a normal distribution

#Matrix function - 
derive_return_matrix <- function(stock_data_frame, market_index){
  sdf <- stock_data_frame
  tickers <- sdf$df.control$ticker
  index_returns <- market_index$df.tickers["ret.closing.prices"]
  colnames(index_returns)[1] <- market_index$df.control$ticker
  df_outs <- data.frame(tickers = character(), reason = character())
  df_less <- data.frame(tickers = character(), number = numeric())
  
  for(tick in tickers){
    ticker_returns <- filter(sdf$df.tickers, ticker == tick) %>% select(ret.closing.prices)
    if (nrow(ticker_returns) > 0 && !((sum(na.trim(ticker_returns == 0))/nrow(na.trim(ticker_returns))) > 0.10)){
      
      if (nrow(ticker_returns) == nrow(index_returns)){
        index_returns[tick] <- ticker_returns 

      }else if (nrow(ticker_returns) < nrow(index_returns)){
        rows_left <- nrow(ticker_returns) - nrow(index_returns)
        normal_return_buffer <- as.matrix(na.trim(ticker_returns))
        return_buffer <- ticker_returns
        df_less[nrow(df_less)+1,1] <- tick
        df_less[nrow(df_less),2] <- -rows_left
        
        while(rows_left < 0){
          return_buffer[nrow(return_buffer)+1,1] <- rnorm(1,mean(normal_return_buffer),sd(normal_return_buffer))

          if(nrow(return_buffer) == nrow(index_returns)){
            index_returns[tick] <- return_buffer
          }
          rows_left <- nrow(return_buffer) - nrow(index_returns)
        }
        
      }else if(nrow(ticker_returns) > nrow(index_returns)){
        #print("ticker longer than index", col = "red")
      }
      
    }else if (!(nrow(ticker_returns) > 0)){
      df_outs[nrow(df_outs)+1,1] <- tick
      df_outs[nrow(df_outs),2] <- "No rows"
      
    }else if ((sum(na.trim(ticker_returns == 0))/nrow(na.trim(ticker_returns))) > 0.10){
      df_outs[nrow(df_outs)+1,1] <- tick
      df_outs[nrow(df_outs),2] <- "> 0.1 zeros"
    }
    
  }
  df_index_returns <- data.frame(na.trim(index_returns))
  return(list(df.returns = df_index_returns, df.outs = df_outs, df.less = df_less))
}


DOW_stock_returns <- BatchGetSymbols(tq_index("DOW")$symbol, first.date = Sys.Date()-420, last.date = Sys.Date(), bench.ticker = "^DJI", freq.data = "weekly", how.to.aggregate = "last")
DOW_returns <- BatchGetSymbols(c("^DJI"),first.date = Sys.Date()-420, last.date = Sys.Date(), bench.ticker = "^DJI", freq.data = "weekly", how.to.aggregate = "last")

DOW_return_matrix <- derive_return_matrix(DOW_stock_returns, DOW_returns)
DOW_cov_matrix <- cov(as.matrix(DOW_return_matrix$df.returns))
DOW_cor_matrix <- cor(as.matrix(DOW_return_matrix$df.returns))

DOW_beta_vector <- DOW_cov_matrix[,1]/DOW_cov_matrix[1,1]
View(DOW_beta_vector)

corrplot(DOW_cor_matrix)

DOW_return_matrix

tq_performance_fun_options()














