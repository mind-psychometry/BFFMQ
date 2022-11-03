Bangla_FFMQ <- readxl::read_excel("ProcessedData/full_data_december_8_2021.xlsx")
DT::datatable(data)
saveRDS(Bangla_FFMQ, "ProcessedData/Bangla_FFMQ.rds")

data <- foreign::read.spss("SPSS data/master data.sav")

## Recoding data$EI7
data$EI7 <- as.character(data$EI7)
data$EI7 <- fct_recode(data$EI7,
  "5" = "1",
  "4" = "2",
  "2" = "4",
  "1" = "5"
)