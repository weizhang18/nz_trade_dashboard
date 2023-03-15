### read zip data
library(readr)

df <- read_csv(unzip(file_stats_nz_trade, 
                     "output_csv_full.csv"))


unique(df$time_ref)
unique(df$account) 
unique(df$code)
unique(df$country_code)
unique(df$product_type)
unique(df$status)

df_cc <- read_csv(unzip(file_stats_nz_trade, 
                     "country_classification.csv"))

df_gc <- read_csv(unzip(file_stats_nz_trade, 
                        "goods_classification.csv"))

df_sc <- read_csv(unzip(file_stats_nz_trade, 
                        "services_classification.csv"))

unlink(c("output_csv_full.csv",
         "country_classification.csv",
         "goods_classification.csv",
         "services_classification.csv"))


"TOT (OMT FOB)"
"	
TOT (BoP basis)"

"A12" "TOT" "Services"