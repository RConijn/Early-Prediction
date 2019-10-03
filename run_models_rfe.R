######## ---------- LOAD PACKAGES ----------------------------------------######
library("dplyr")
library("caret")
library("ggplot2")
library("randomForest")
library("MLmetrics")

######## ---------- RUN MODELS WITH RFE ----------------------------------######
read_files <- function(file) {
  data <- read.csv(file, stringsAsFactors = FALSE)
  names(data) <- paste0(substring(file, 32,39),".", names(data)) #keystrokes
  #names(data) <- paste0(substring(file, 27,34),".", names(data)) # time
  data
}


run_model <- function(timeframe, trainmethod, path, tol = 1){

  pattern = ifelse(timeframe == 5,  "_0_to_5",
            ifelse(timeframe == 10, "_0_to_5|_5_to_10|_0_to_10",
            ifelse(timeframe == 15, "_0_to_5|_5_to_10|_10_to_15|_0_to_15",
            ifelse(timeframe == 20, "_0_to_5|_5_to_10|_10_to_15|_15_to_20|_0_to_20",
            ifelse(timeframe == 25, "_0_to_5|_5_to_10|_10_to_15|_15_to_20|_20_to_25|_0_to_25",
            ifelse(timeframe == 30, "_0_to_5|_5_to_10|_10_to_15|_15_to_20|_20_to_25|_25_to_30|_0_to_30", NA))))))


  files = list.files(path, pattern, full.names = TRUE)
  keylog <- do.call(cbind, lapply(files, read_files))

  keylog2 <- keylog %>%
    rename(total_score = `0_to_5_k.c.total`) %>% # keystrokes
    #rename(total_score = `0_to_5_m.c.total`) %>% #time
    select(-contains("c.total"))

  print(timeframe)
  print(trainmethod)
  y <- keylog2[, "total_score"]
  keylog2 <- select(keylog2, -contains("pid"), -total_score)

    ## BASELINE
  if (trainmethod == "baseline(mean)"){
    predicted <- mean(y)
    rmse <- RMSE(predicted,y)
    mae <- MAE(predicted, y)
    r2 <- R2_Score(predicted, y)

    out <- data.frame(trainmethod, timeframe, Variables = "none", RMSE = rmse,
                      Rsquared = r2, MAE = mae, RMSESD = "na",
                      RsquaredSD = "na", MAESD = "na", ncol.x. = ncol(keylog2),
                      bestFeat = "none")

  } else {
    #remove low variance predictors
    x <- scale(keylog2[,-nearZeroVar(keylog2)])
    #remove highly correlated predictors
    x <- x[, -findCorrelation(cor(x), .8)]
    x <- as.data.frame(x)

    set.seed(8)

    if (trainmethod == "rf"){
      rfFuncs$selectSize <- pickSizeTolerance
      control <- rfeControl(functions=rfFuncs, method="cv", number = 10,
                            verbose = FALSE, returnResamp = "final")
  #  } else if(trainmethod == "lm"){
  #    lmFuncs$selectSize <- pickSizeTolerance
  #    control <- rfeControl(functions=lmFuncs, method="cv", number = 10,
  #                          verbose = FALSE, returnResamp = "final")
    } else {
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
                   metric = "RMSE",
                   maximize = T,
                   tol = tol,
                   trControl = trainControl(method = "cv", number = 10,
                                            returnResamp = "final"))

    bestSubset <- results$results[results$bestSubset,]
    bestFeatures <- results$optVariables


    out <- data.frame(trainmethod, timeframe, bestSubset, ncol(x),
                      bestFeat = paste(bestFeatures, collapse = ","),
                      row.names = F)
    }

  return(out)

}


# print results for 2-30 minutes
methods <- c("lm", "svmRadial", "rf", "baseline(mean)")
"lm"

#############------------KEYSTROKE MODELS-------------------------------########
resultslm <- do.call(rbind, lapply(seq(5,30,5), run_model, methods[1], "data/keystroke/"))
saveRDS(resultslm, "models/results_keys_lm_reg.rda", compress = "xz")
resultssvm <- do.call(rbind, lapply(seq(5,30,5), run_model, methods[2], "data/time/"))
saveRDS(resultssvm, "models/results_time_svm_reg.rda", compress = "xz")
resultsrf <-  do.call(rbind, lapply(seq(5,30,5), run_model, methods[3], "data/time/"))
saveRDS(resultsrf, "models/results_time_rf_reg.rda", compress = "xz")
resultsbase <- do.call(rbind, lapply(seq(5,30,5), run_model, methods[4], "data/time/"))
saveRDS(resultsbase, "models/results_time_base_reg.rda", compress = "xz")


resultslm <- readRDS("models/results_keys_lm_reg.rda")
resultssvm <- readRDS("models/results_keys_svm_reg.rda")
resultsrf <- readRDS("models/results_keys_rf_reg.rda")
resultsbase <- readRDS("models/results_keys_base_reg.rda")

#############------------TIME MODELS------------------------------------########
resultslm <- do.call(rbind, lapply(seq(5,30,5), run_model, methods[1], "data/time/"))
saveRDS(resultslm, "models/resultstimelm42.rda", compress = "xz")
resultssvm <- do.call(rbind, lapply(seq(5,30,5), run_model, methods[2], "data/time/"))
saveRDS(resultssvm, "models/resultstimesvm42.rda", compress = "xz")
resultsrf <-  do.call(rbind, lapply(seq(5,30,5), run_model, methods[3], "data/time/"))
saveRDS(resultsrf, "models/resultstimerf42.rda", compress = "xz")
resultsbase <- do.call(rbind, lapply(seq(5,30,5), run_model, methods[4], "data/time/"))
saveRDS(resultsbase, "models/resultstimebase42.rda", compress = "xz")

resultslm <- readRDS("models/resultstimelm42.rda")
resultssvm <- readRDS("models/resultstimesvm42.rda")
resultsrf <- readRDS("models/resultstimerf42.rda")
resultsbase <- readRDS("models/resultstimebase42.rda")

#############------------COMBINE RESULTS--------------------------------########
results <- resultssvm %>%
 # rbind(resultslm) %>%
  rbind(resultsrf) %>%
  rbind(resultsbase) %>%
  mutate(trainmethod =
           #ifelse(trainmethod == "lm", "Linear regression",
           ifelse(trainmethod == "svmRadial", "Support vector machine",
           ifelse(trainmethod == "rf", "Random forest",
                  "Baseline (mean)")))#)


#############------------PLOT RESULTS-----------------------------------########
#RMSE
results %>%
  ggplot(aes(x=timeframe, y=RMSE, group=trainmethod)) +
  geom_line(aes(color=trainmethod))+
  geom_point(aes(color=trainmethod))+
  theme_bw() +
  scale_x_continuous(breaks = seq(0,30,5), minor_breaks = seq(0 , 30, 1)) +
  scale_color_discrete(name = "Regression model:") +
  xlab("Keystrokes") +
  theme(legend.position="bottom") +
  ylim(1.30, 1.65)

#plot RMSE for slides
results %>%
  ggplot(aes(x=timeframe, y=RMSE, group=trainmethod)) +
  geom_line(aes(color=trainmethod), size = 2)+
  geom_point(aes(color=trainmethod), size = 2)+
  theme_bw() +
  theme(text = element_text(size = 20)) +
  scale_x_continuous(breaks = seq(0,30,5), minor_breaks = seq(0 , 30, 1)) +
  scale_color_discrete(name = "Regression model:") +
  xlab("Timeframe (mins)") +
  ylim(1.20, 1.40)

#MAE
results %>%
  ggplot(aes(x=timeframe, y=MAE, group=trainmethod)) +
  geom_line(aes(color=trainmethod))+
  geom_point(aes(color=trainmethod)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0,30,5), minor_breaks = seq(0 , 30, 1)) +
  scale_color_discrete(name = "Regression model:") +
  xlab("Timeframe (x/30th keystroke)") +
  theme(legend.position="bottom")

