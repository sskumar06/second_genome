setwd("C:\\Users\\sk\\Documents\\Data_Science\\Second_Genome\\")

halo_tolerant <- read.csv("halotolerant_input.out", stringsAsFactors = F)

halo_sensitive <- read.csv("halo_sensitive_input.out", stringsAsFactors = F)

fresh <- read.csv("fresh_input.out", stringsAsFactors = F)

# To remove unwanted rows
# grep("^tr|K8CLK0|K8CLK0_CROSK Corresponds", fresh$Feature)
# fresh[165, 1]
# fresh[164, 1]
# fresh[159, 1]
# fresh[158, 1]

fresh <- fresh[-c(158, 159, 164, 165), ]
fresh <- na.omit(fresh)
non_halo_tolerant <- rbind(fresh, halo_sensitive)

halo_tolerant$y <- 1
non_halo_tolerant$y <- 0

View(halo_tolerant[duplicated(halo_tolerant),])
View(non_halo_tolerant[duplicated(non_halo_tolerant),])


# if(nrow(halo_tolerant) < nrow(non_halo_tolerant)){
#   temp <- non_halo_tolerant[sample(nrow(non_halo_tolerant), nrow(halo_tolerant)), ]
#   model_input <- rbind(halo_tolerant, temp)
# }else if(nrow(non_halo_tolerant) < nrow(halo_tolerant)){
#   temp <- halo_tolerant[sample(nrow(halo_tolerant), nrow(non_halo_tolerant)),]
#   model_input <- rbind(non_halo_tolerant, temp)
# }

# Sample only 100 rows from each data frame
temp <- non_halo_tolerant[sample(nrow(non_halo_tolerant), 100), ]
temp1 <- halo_tolerant[sample(nrow(halo_tolerant), 100),]
model_input <- rbind(temp, temp1)
validation_non_halo_tolerant <- subset(non_halo_tolerant, !(non_halo_tolerant$Feature %in% temp$Feature))
validation_halo_tolerant <- subset(halo_tolerant, !(halo_tolerant$Feature %in% temp1$Feature))

### Random Forest variable importance
library(randomForest)
library(graphics)

independentcolumns <- (2:1438)
dependentcolumn <- 1439
noise_variable_number <- 100

model_input[,2] <- as.numeric(model_input[,2])
x <- model_input[,independentcolumns]

for(i in 1:noise_variable_number)
{
  if(i==1)
  {
    noise <- data.frame(runif(nrow(model_input),min=0,max=1))
  }
  else
  {
    noise <- data.frame(noise,runif(nrow(model_input),min=0,max=1))
  }
  names(noise)[i] <- paste("Random Var ",i)
}

x <- data.frame(x,noise)
sx <- scale(x)
y <- model_input[,dependentcolumn] 

rf1 <- randomForest(sx, as.factor(y), ntree=1000, importance = TRUE)
rank <- rf1$importance

varImpPlot(rf1,main="Random Forest Variable Rank - halotolerant vs. non halotolerant")
write.table(rank,"RF_variable_importance.csv", append=FALSE,FALSE, col.names=TRUE,",",dec=".")


##Important variables
rf_variables <- read.csv("variable_list.csv", stringsAsFactors = F)
model_input_refined <- model_input[, c("Feature", "y", rf_variables[,"variable_list"])]
validation_non_halo_tolerant_refined <- validation_non_halo_tolerant[, c("Feature", "y", rf_variables[,"variable_list"])]
validation_halo_tolerant_refined <- validation_halo_tolerant[, c("Feature", "y", rf_variables[,"variable_list"])]

## Model Training - Random Forest
rf_model <- randomForest(y=as.factor(model_input_refined$y), x=model_input_refined[, c(3:240)], ntree=1000, proximity = T)


## Validation - Random Forest
validation_non_halo_tolerant_refined$prediction <- predict(rf_model, newdata = validation_non_halo_tolerant_refined, type="response")
validation_halo_tolerant_refined$prediction <- predict(rf_model, newdata = validation_halo_tolerant_refined, type="response")



