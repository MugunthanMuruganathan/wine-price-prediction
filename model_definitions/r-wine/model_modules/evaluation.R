
LoadPackages <- function() {
  library("methods")
  library("jsonlite")
  library("caret")
  library("gbm")
  library("DBI")
  library("dplyr")
  library("tdplyr")
}

evaluate <- function(data_conf, model_conf, ...) {
  model <- readRDS("artifacts/input/model_wine_lm.rds")
  print("Evaluating model...")

  suppressPackageStartupMessages(LoadPackages())

  # Connect to Vantage
  con <- aoa_create_context()

  table <- tbl(con, sql(data_conf$sql))

    # Create dataframe from tibble, selecting the necessary columns and mutating integer64 to integers
  data <- table %>% mutate(
					   WinterRain = as.integer(WinterRain),
                       AGST = as.integer(AGST),
                       HarvestRain = as.integer(HarvestRain),
					   Age = as.integer(Age),
					   FrancePop = as.integer(FrancePop),
					   Price = as.integer(Price)) %>% as.data.frame()

  predicted_price <- predict(model, newdata=data)
  #preds <- as.integer(ifelse(probs > 0.5, 1, 0))

  #cm <- confusionMatrix(table(preds, data$HasDiabetes))
  
  #SSE = sum(predicted_price$residuals^2)
  print ("The SSE value is ")
  SSE = sum((data$Price- predicted_price)^2)
  
  print ("The SSE value is ")
  
  print (SSE)

#  png("artifacts/output/confusion_matrix.png", width = 860, height = 860)
#  fourfoldplot(cm$table)
#  dev.off()

#  preds$pred <- preds
#  metrics <- cm$overall
  metrics <- SSE

  write(jsonlite::toJSON(metrics, auto_unbox = TRUE, null = "null", keep_vec_names=TRUE), "artifacts/output/metrics.json")
}
