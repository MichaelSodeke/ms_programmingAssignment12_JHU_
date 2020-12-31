# [4-C2_prediction.r]

## ---------------- create pauser -----------------------------------------------------------
pause <- function(x=2.5)
{                                                                                                                                                                                                                                                                                                            
	message("\n || >>> current file: [4-C2_prediction.r]")
	message("\n\t Preparing [next] code chunck...")
	p1 <- proc.time()
        Sys.sleep(x)
        proc.time() - p1                                                                                                                                                                                                                                                                 
}

## ---------------- prediction --------------------------------------------------------------
message("\n\n | [4.] prediction ...")
message(" --------------------------------------------------------------------")
# predict manner in which participants performed dumbbell exercises
message("\t\n\n\n || [4.0.1] predicting based on [classe] variable in [trn.pre] ...")
y_ <- matrix(predict(Brf_,newdata=tst.pre))
ACC <- confusionMatrix(factor(y_), tst.pre$classe)$overall[1]
OSE <- 1 - ACC
val <- data.frame("accuracy"=ACC,"out.of.sample.error"=OSE); rownames(val) <- NULL
val %>% print()
pause()
# predict manner in which participants performed dumbbell exercises
message("\t\n\n\n || [4.0.1] predicting based on [classe] variable in [trn.final] ...")
y_ <- matrix(predict(Brf_,newdata=tst.final))
y_ %>% print()
pause()