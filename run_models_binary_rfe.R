######## ---------- LOAD PACKAGES ----------------------------------------######
library("dplyr")
library("caret")
library("ggplot2")
library("randomForest")
library("MLmetrics")
library("tidyr")
library("gridExtra")
library("pROC")

######## ---------- RUN MODELS WITH RFE ----------------------------------######
read_files <- function(file) {
  data <- read.csv(file, stringsAsFactors = FALSE)
  #names(data) <- paste0(substring(file, 32,39),".", names(data)) #keystrokes
  names(data) <- paste0(substring(file, 27,34),".", names(data)) # time
  data
}


run_class_model <- function(timeframe, trainmethod, path, tol = 1){

  pattern = ifelse(timeframe == 5,  "_0_to_5",
            ifelse(timeframe == 10, "_0_to_5|_5_to_10|_0_to_10",
            ifelse(timeframe == 15, "_0_to_5|_5_to_10|_10_to_15|_0_to_15",
            ifelse(timeframe == 20, "_0_to_5|_5_to_10|_10_to_15|_15_to_20|_0_to_20",
            ifelse(timeframe == 25, "_0_to_5|_5_to_10|_10_to_15|_15_to_20|_20_to_25|_0_to_25",
            ifelse(timeframe == 30, "_0_to_5|_5_to_10|_10_to_15|_15_to_20|_20_to_25|_25_to_30|_0_to_30", NA))))))


  files = list.files(path, pattern, full.names = TRUE)
  keylog <- do.call(cbind, lapply(files, read_files))

  keylog2 <- keylog %>%
    #rename(total_score = `0_to_5_k.c.total`) %>% # keystrokes
    rename(total_score = `0_to_5_m.c.total`) %>% #time
    select(-contains("c.total"))

  print(timeframe)
  print(trainmethod)

  keylog2 <- keylog2 %>%
    mutate(at_risk = ifelse(keylog2[,"total_score"] < 5.5,"at_risk", "not_at_risk"))

  keylog3 <- keylog2 %>%
    mutate(at_risk = factor(at_risk))

  keylog2 <- select(keylog2, -contains("pid"), -total_score, -at_risk )
  y <- keylog3[,"at_risk"]


  if (trainmethod == "baseline(majorityclass)"){
    predicted <- factor(ifelse(sum(as.numeric(y)) >=
                                 0.5 * length(y), 1, 0), levels = c(0,1),
                        labels = c("at_risk", "not_at_risk"))
    predicted <- rep(predicted, length(y))
    roc_obj <- roc(as.numeric(y), as.numeric(predicted))
    auca <- auc(roc_obj)
    kappa <- confusionMatrix(predicted, y)$overall["Kappa"]
    TP <- confusionMatrix(predicted, y)$table[1,1]
    FP <- confusionMatrix(predicted, y)$table[2,1]
    FN <- confusionMatrix(predicted, y)$table[1,2]
    TN <- confusionMatrix(predicted, y)$table[2,2]
    Precision <- TP/(TP+FP)
    Recall <- TP/(TP+FN)
    Recall <- 0 # no students at risk identified
    Fscore <- 2*(Precision*Recall)/(Precision+Recall)
    out <- data.frame(trainmethod, timeframe, Variables = "none", AUC = auca,
                      Precision, Recall, `F`, AUCSD = "na", PrecisionSD = "na",
                      RecallSD = "na", FSD = "na", ncol.x. = ncol(keylog2),
                      bestFeat = "none", row.names = NULL)
  } else {
    #remove low variance predictors
    x <- scale(keylog2[,-nearZeroVar(keylog2)])
    #remove highly correlated predictors
    x <- x[, -findCorrelation(cor(x), .8)]
    x <- as.data.frame(x)

    if (trainmethod == "rf"){
      rfFuncs$summary <- prSummary
      rfFuncs$selectSize <- pickSizeTolerance
      control <- rfeControl(functions=rfFuncs, method="cv", number = 10,
                            verbose = FALSE, returnResamp = "final")
    } else {
      caretFuncs$summary <- prSummary
      caretFuncs$selectSize <- pickSizeTolerance

      control <- rfeControl(functions=caretFuncs, method="cv", number = 10,
                            verbose = FALSE, returnResamp = "final")
    }
    # run the RFE algorithm
    set.seed(42)
    maxsize <- ifelse(ncol(x) < 42, ncol(x), 42)
    results <- rfe(x=x, y=y, sizes=c(1:maxsize),
                   rfeControl=control,
                   method = trainmethod,
                   family = binomial("logit"),
                   maxit = 100 ,
                   metric = "F",
                   maximize = T,
                   tol = tol,
                   trControl = trainControl(method = "cv", number = 10,
                                            classProbs = T,
                                            summaryFunction = prSummary,
                                            returnResamp = "final"))

      bestSubset <- results$results[results$bestSubset,]
      bestFeatures <- results$optVariables


    out <- data.frame(trainmethod,timeframe, bestSubset, ncol(x),
                      bestFeat = paste(bestFeatures, collapse = ","),
                      row.names = NULL)

  }
  return(out)
}


# print results for 2-30 minutes
methods <- c("glmnet", "svmRadial", "rf", "baseline(majorityclass)", "nb")

times <- seq(5,30,5)

#resultsglm <- do.call(rbind, lapply(times, run_class_model, methods[1], "data/time/"))
#  saveRDS(resultsglm, "models/results_keys_glm.rda", compress = "xz")
resultssvm <- do.call(rbind, lapply(times, run_class_model, methods[2], "data/time/"))
  saveRDS(resultssvm, "models/results_time_svm.rda", compress = "xz")
resultsrf <- do.call(rbind, lapply(times, run_class_model, methods[3], "data/time/"))
  saveRDS(resultsrf, "models/results_time_rf.rda", compress = "xz")
resultsbase <- do.call(rbind, lapply(times, run_class_model, methods[4], "data/time/"))
  saveRDS(resultsbase, "models/results_time_base.rda", compress = "xz")
resultsnb <- do.call(rbind, lapply(times, run_class_model, methods[5], "data/time/"))
  saveRDS(resultsnb, "models/results_time_nb.rda", compress = "xz")


#############----------COMBINE RESULTS-------------------------------###########

#time
resultssvm <- readRDS("models/results_keys_svm.rda")
resultsrf <- readRDS("models/results_keys_rf.rda")
resultsnb <- readRDS("models/results_keys_nb.rda")
resultsbase <- readRDS("models/results_keys_base.rda")

results <- resultssvm %>%
  rbind(resultsnb) %>%
  rbind(resultsrf) %>%
  rbind(resultsbase)  %>%
  mutate(trainmethod =
           ifelse(trainmethod == "nb", "Naive Bayes",
           ifelse(trainmethod == "svmRadial", "Support vector machine",
           ifelse(trainmethod == "rf", "Random forest",
                                "Baseline (majority class)"))))

write.csv(results, "data/results_keys_F.csv", row.names = F)

# keystrokes
resultssvm <- readRDS("models/results_keys_svm.rda")
resultsrf <- readRDS("models/results_keys_rf.rda")
resultsnb <- readRDS("models/results_keys_nb.rda")
resultsbase <- readRDS("models/results_keys_base.rda")

results <- resultssvm %>%
  rbind(resultsnb) %>%
  rbind(resultsrf) %>%
  rbind(resultsbase)  %>%
  mutate(trainmethod =
           ifelse(trainmethod == "nb", "Naive Bayes",
                  ifelse(trainmethod == "svmRadial", "Support vector machine",
                         ifelse(trainmethod == "rf", "Random forest",
                                "Baseline (majority class)"))))

write.csv(results, "data/results_keys_F", row.names = F)







