

##############################
#      Preprocesing data     #
##############################

### LIBRARIES ###
library(dplyr)



# Loading csv files
data_processed<-as.data.frame(readRDS("fixed_scraped_dataset.rds"))

# Initial analysis
dim(data_processed) 
str(data_processed)


# Is there any not numerical variable? find out why 
non_numeric <- sapply(data_processed[, c(3:18)], function(x) ! is.numeric(x)) %>% which() %>% names()

non_numeric_reason=list() 
for (variable in non_numeric) {
  non_numeric_reason[[variable]]<-data_processed[[variable]][grep('[^0-9]+',data_processed[[variable]])]%>%unique()
}
non_numeric_reason%>%print() # So far just CS variable is not numerical
data_processed$CS[data_processed$CS=="--"] <- 0

# Convert to numeric all numerical variables (so far it is just CS)
data_processed[, non_numeric] <- sapply(data_processed[, non_numeric], function(x) as.numeric(x))


# Missing values treatment
n_missings_by_col <- sapply(data_processed, function(x) is.na(x) %>% sum()) 
data_processed[is.na(data_processed)] <- 0


# Save procesed data into rds file and in data_backup
saveRDS(data_processed, file = "data_preprocessed.rds")
saveRDS(data_processed,
        file = paste0(
          "data_backups/data_preprocessed_",
          format(Sys.time(), "%Y-%m-%d_%Hh%M"),
          ".rds"
        ))

