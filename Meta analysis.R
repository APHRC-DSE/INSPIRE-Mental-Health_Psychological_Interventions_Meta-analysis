# Load required libraries
library(dplyr)
library(meta)
library(metafor)
# Load the data
data <- read.csv("PRT.csv", stringsAsFactors = FALSE)
colnames(data)


# Temp Function to show dataframe in Excel --------------------------------

show_in_excel <- function(.data){
  
  tmp <- paste0(tempfile(), ".csv")
  
  write.csv(.data, tmp)
  
  fs::file_show(path = tmp)
  
}


#data %>% show_in_excel()



data$Effect.Size <- as.numeric(data$Effect.Size)
data$Sample <- as.numeric(data$Sample)
meta_data <- data[complete.cases(data[, c("Effect.Size", "Sample", "First.Author")]), c("First.Author", "Effect.Size", "Sample")]






meta_data$sei <- 1 / sqrt(meta_data$Sample)
meta_analysis <- rma(yi = Effect.Size, sei = sei, data = meta_data, method = "REML")
summary(meta_analysis)





meta_data <- data %>%
  filter(!is.na(`Experimental.Outcome`) & !is.na(`Outcome.Measure`) & !is.na(`Sample`)) %>%
  mutate(TE = `Experimental.Outcome` - `Outcome.Measure`,
         Variance = ((`Effect.Size` * `Sample`)^2) / `Sample`,
         sei = sqrt(Variance / `Sample`)) %>%
  select(PMID, `First.Author`, Sample, TE, sei)

ma <- metagen(TE, sei, data = meta_data,
              study = paste(meta_data$PMID, meta_data$`First.Author`, sep = "_"),
              subset = !is.na(meta_data$TE))

summary(ma)


#write.xlsx(dt$data, "dtdata.xlsx")

# Perform sensitivity analysis using a loop
sensitivity_results <- data.frame(study = character(), SMD = numeric(), lower = numeric(), upper = numeric())

for (i in 1:nrow(meta_data)) {
  subset_data <- meta_data[-i, ]
  subset_ma <- metagen(TE, sei, data = subset_data,
                       study = paste(subset_data$PMID, subset_data$`First.Author`, sep = "_"),
                       subset = !is.na(subset_data$TE))
  sensitivity_results <- rbind(sensitivity_results, data.frame(
    study = paste("Excluding", meta_data$`First.Author`[i]),
    SMD = subset_ma$TE.random,
    lower = subset_ma$lower.random,
    upper = subset_ma$upper.random
  ))
}

print(sensitivity_results)






forest(meta_analysis, slab = meta_data$First.Author, xlim = c(-10, 10), alim = c(-10, 10),
       xlab = "Effect Size", refline = 0, cex = 0.8, ylim = c(-1, nrow(meta_data) + 1))


print(meta_analysis)

funnel(meta_analysis)


eggers_test <- regtest(meta_analysis)


print(eggers_test)










