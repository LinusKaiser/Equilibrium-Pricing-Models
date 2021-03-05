#BETA Vector

#Packages
library(quantmod)
library(BatchGetSymbols)
library(tidyverse)
library(ddplyr)
library(corrplot)


#Beta function - takes a datafrane of a list of stocks and a dataframe of an index of the BatchGetSymbols package
derive_beta <- function(stock_data_frame, market_index){
  sdf <- stock_data_frame
  tickers <- sdf$df.control$ticker
  index_returns <- market_index$df.tickers["ret.closing.prices"]
  #print(str_interp("index_returns${class(index_returns)}"))
  #print(index_returns)
  colnames(index_returns)[1] <- market_index$df.control$ticker
  df_outs <- data.frame(tickers = character(), reason = character())
  
  for(tick in tickers){
    ticker_returns <- filter(sdf$df.tickers, ticker == tick) %>% select(ret.closing.prices)
    if (nrow(ticker_returns) > 0 && !((sum(na.trim(ticker_returns == 0))/nrow(na.trim(ticker_returns))) > 0.10)){
      #print(str_interp("ticker_returns${class(ticker_returns)}"))
      #print(str_interp("return_data_frame${class(return_data_frame)}"))
      
      if (nrow(ticker_returns) == nrow(index_returns)){
        index_returns[tick] <- ticker_returns 
        #print(tick)
        #print(index_returns)
        
      }else if (nrow(ticker_returns) < nrow(index_returns)){
        rows_left <- nrow(ticker_returns) - nrow(index_returns)
        #print(str_interp("ticker_returns ${nrow(ticker_returns)} == ${nrow(index_returns)}"))
        normal_return_buffer <- as.matrix(na.trim(ticker_returns))
        #print(normal_return_buffer)
        #View(index_returns)
        return_buffer <- ticker_returns
        
        while(rows_left < 0){
          return_buffer[nrow(return_buffer)+1,1] <- rnorm(1,mean(normal_return_buffer),sd(normal_return_buffer))
          #print(return_buffer)
          #print(tail(return_buffer), 50)
          #print(str_interp("${tick} has ${-rows_left} less than the ${market_index$df.tickers$ticker[1]}"))
          #print(str_interp("return_buffer ${nrow(return_buffer)} == ${nrow(index_returns)} index_returns"))
          
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
  index_returns <- na.trim(index_returns)
  covariance_matrix <- var(as.matrix(index_returns))
  correlation_matrix <- cor(as.matrix(index_returns))
  df_correlation_matrix <- data.frame(correlation_matrix)
  beta_vector <- covariance_matrix[,market_index$df.control$ticker]/covariance_matrix[market_index$df.control$ticker,market_index$df.control$ticker]
  df_beta_vector <- data.frame(Beta = beta_vector)
  colnames(df_correlation_matrix)[1] <- market_index$df.tickers$ticker[1]
  return(list(df.betas = df_beta_vector, df.corr = df_correlation_matrix, df.outs = df_outs))
}


#loading the dataframe of all SP500 companies by passing in a list of tickers (GetSP500Stocks()$Tickers) of all SP500 companies
SP_return_frame <- BatchGetSymbols(GetSP500Stocks()$Tickers, first.date = Sys.Date()-365, last.date = Sys.Date(),bench.ticker = "^GSPC",freq.data = "weekly",how.to.aggregate = "last")
#loading the dataframe of SP500
SP500_returns <- BatchGetSymbols(c("^GSPC"), first.date = Sys.Date()-365, last.date = Sys.Date(),bench.ticker = "^GSPC",freq.data = "weekly",how.to.aggregate = "last")

#Deriving all betas of all SP500 firms with SP500 as market index
SP_500_betas <- derive_beta(SP_return_frame, SP500_returns)
SP_500_betas


FTSE_stock_frame <- BatchGetSymbols(GetFTSE100Stocks()$tickers, first.date = as.Date(as.character(Sys.Date()), format = "%Y-%d-%m")-365, last.date = as.Date(as.character(Sys.Date()), format = "%Y-%d-%m"),bench.ticker = "^FTSE",freq.data = "weekly",how.to.aggregate = "last")
FTSE_frame <- BatchGetSymbols("^FTSE", first.date = as.Date(as.character(Sys.Date()), format = "%Y-%d-%m")-365, last.date = as.Date(as.character(Sys.Date()), format = "%Y-%d-%m"),bench.ticker = "^FTSE",freq.data = "weekly",how.to.aggregate = "last")

FTSE_100_betas <- derive_beta(FTSE_stock_frame, FTSE_frame)
FTSE_100_betas

corrplot(as.matrix(FTSE_100_betas$df.corr))
