### read zip data
library(readr)

df <- read_csv(unzip("data_raw/intel_trade_data/international-trade-december-2022-quarter.zip", 
                     "output_csv_full.csv"))


unique(df$time_ref)
unique(df$account) 
unique(df$code)
unique(df$country_code)
unique(df$product_type)
unique(df$status)

df_cc <- read_csv(unzip("data_raw/intel_trade_data/international-trade-december-2022-quarter.zip", 
                     "country_classification.csv"))

df_gc <- read_csv(unzip("data_raw/intel_trade_data/international-trade-december-2022-quarter.zip", 
                        "goods_classification.csv"))

df_sc <- read_csv(unzip("data_raw/intel_trade_data/international-trade-december-2022-quarter.zip", 
                        "services_classification.csv"))
