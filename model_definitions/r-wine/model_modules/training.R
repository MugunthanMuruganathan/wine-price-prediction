LoadPackages <- function() {
    library("gbm")
    library("DBI")
    library("dplyr")
    library("tdplyr")

}

suppressPackageStartupMessages(LoadPackages())

train <- function(data_conf, model_conf, ...) {
    # Connect to Vantage
    con <- aoa_create_context()

	print (" ...... Before setting the connection ")

    table <- tbl(con, sql(data_conf$sql))

	#print data_conf$featureNames 
	print (" ...... After setting the connection ")
	#print data_conf$targetNames 
	
    # Create dataframe from tibble, selecting the necessary columns and mutating integer64 to integers
    # select both the feature and target columns (ignorning e.g. entity key)
    columns <- unlist(c(data_conf$featureNames, data_conf$targetNames), use.name = TRUE)

	print (" ...... After setting the columns ")
	print (" ...... Before getting the data ")
	
    data <- table %>% select(all_of(columns)) %>% mutate(
                       YearId = as.integer(YearId),
                       WinterRain = as.integer(WinterRain),
                       AGST = as.integer(AGST),
                       HarvestRain = as.integer(HarvestRain),
					   Age = as.integer(Age),
					   FrancePop = as.integer(FrancePop),
					   Price = as.integer(Price)) %>% as.data.frame()

	print (" ...... After getting the data ")
	
    # Load hyperparameters from model configuration
    hyperparams <- model_conf[["hyperParameters"]]

    print(" Data has been collected. Training model...")

    # Train model
    model <- lm(Price~ AGST + WinterRain + HarvestRain + Age,
                 data=data#,
                 #shrinkage=hyperparams$shrinkage,
                 #distribution = 'bernoulli',
                 #cv.folds=hyperparams$cv.folds,
                 #n.trees=hyperparams$n.trees,
                 #verbose=FALSE
				 )

    print("Model Trained!")

    # Get optimal number of iterations
    #best.iter <- lm.perf(model, plot.it=FALSE, method="cv")

    # clean the model (R stores the dataset on the model..
    model$data <- NULL

    # how to save only best.iter tree?
    # model$best.iter <- best.iter
    # model$trees <- light$trees[best.iter]

    # Save trained model
    print("Saving trained model...")
    saveRDS(model, "artifacts/output/model_wine_lm.rds")
}
