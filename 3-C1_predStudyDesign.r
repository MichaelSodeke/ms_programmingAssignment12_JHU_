# [3-C1_predStudyDesign.r]

## ---------------- create pauser -----------------------------------------------------------
pause <- function(x=2.5)
{                                                                                                                                                                                                                                                                                                            
	message("\n || >>> current file: [3-C1_predStudyDesign.r]")
	message("\n\t Preparing [next] code chunck...")
	p1 <- proc.time()
        Sys.sleep(x)
        proc.time() - p1                                                                                                                                                                                                                                                                 
}

## ---------------- prediction study design -------------------------------------------------
message("\n\n | [3.] prediction study design...")
message(" --------------------------------------------------------------------")
# define error rate
message("\t\n\n\n || [3.0.1] PSD: defining error rate type... [accuracy]")
accuracy <- 0
pause()
# perform k-fold cross validation--[CART algorithm]
message("\t\n\n\n || [3.0.2] performing [K-FOLD] cross validation--[CART algorithm]...")
set.seed(16033)
no_cores <- detectCores() - 1
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
Bcart_ <- 0

tic()
stash("Bcart_", {
	Sys.sleep(5)
	Bcart_ <- train(classe ~ .,method="rpart",data=trn,
			trControl=trainControl(method="cv",number=5,p=0.60,allowParallel=T))
})
toc()

Bcart_ %>% print()
pause()
# perform k-fold cross validation--[rf algorithm]
message("\t\n\n\n || [3.0.3] performing [K-FOLD] cross validation--[rf algorithm]...")
Brf_ <- 0

tic()
stash("Brf_", {
	Sys.sleep(5)
	Brf_ <- train(classe ~ .,method="rf",data=trn,
		      trControl=trainControl(method="cv",number=5,p=0.60,allowParallel=T))
})
toc()

Brf_ %>% print()
pause()
# perform k-fold cross validation--[CART algorithm] on selected regressors
message("\t\n\n\n || [3.0.4] performing [K-FOLD] cross validation--[CART algorithm]...")
df1 <- trn %>% select(classe,total_accel_belt,total_accel_arm,
	              total_accel_dumbbell,total_accel_forearm)
Bcart2_ <- 0

tic()
stash("Bcart2_", {
	Sys.sleep(5)
	Bcart2_ <- train(classe ~ .,method="rpart",data=df1,
			 trControl=trainControl(method="cv",number=5,p=0.60,allowParallel=T))
})
toc()

Bcart2_ %>% print()
stopCluster(cl)
registerDoSEQ()
pause()
# perform k-fold cross validation--[rf algorithm] on selected regressors
message("\t\n\n\n || [3.0.5] performing [K-FOLD] cross validation--[rf algorithm]...")
df1 <- trn %>% select(classe,total_accel_belt,total_accel_arm,
		      total_accel_dumbbell,total_accel_forearm)
Brf2_ <- 0

tic()
stash("Brf2_", {
	Sys.sleep(5)
	Brf2_ <- train(classe ~ .,method="rf",data=df1,
		       trControl=trainControl(method="cv",number=5,p=0.60,allowParallel=T))
})
toc()

Brf2_ %>% print()
pause()
# compare accuracies
message("\t\n\n\n || [3.0.6] comparing accuracies...")
check <- list("|[CART--ALL]|"=Bcart_,"|[CART--selected]|"=Bcart2_,
	      "|[RF--ALL]|"=Brf_,"|[RF--selected]|"=Brf2_)
check %>% print()
pause()
# Bcart_: classe vs probability score
message("\t\n\n\n || [3.0.7] plotting Bcart_: [classe vs probability score] ...")
p1 <- ggdendrogram(Bcart_$finalModel,rotate=T,theme_dendro=F)
p1 <- p1 + labs(title = "Bcart_: Classe vs. Probability score",
		x = "Classe",
		y = "Probability score")
p1 <- p1 + my_theme() + theme(legend.position = "none")
p1 %>% print()
pause()
# Brf_: trees vs. error rate
message("\t\n\n\n || [3.0.8] plotting Brf_: [trees vs error rate] ...")
df <- as.data.frame(plot(Brf_$finalModel))
df <- df %>% mutate(trees=1:Brf_$finalModel$ntree)
df2 <- reshape2::melt(df,id.vars="trees")
colnames(df2)[2:3] <- c("type","error")
p2 <- ggplot(data=df2,mapping=aes(x=trees,y=error,color=factor(type)))
p2 <- p2 + geom_line(stat="identity")
p2 <- p2 + scale_colour_brewer(palette="Greys")
p2 <- p2 + labs(title = "Brf_: Trees vs. Error Rate",
		x = "Trees",
		y = "Error rate")
p2 <- p2 + my_theme() + theme(legend.position = "none")
p2 %>% print()
pause()
# Bcart2_: classe vs probability score
message("\t\n\n\n || [3.0.9] plotting Bcart2_: [classe vs probability score] ...")
p3 <- ggdendrogram(Bcart2_$finalModel,rotate=T,theme_dendro=F)
p3 <- p3 + labs(title = "Bcart2_: Classe vs. Probability score",
		x = "Classe",
		y = "Probability score")
p3 <- p3 + my_theme() + theme(legend.position = "none")
p3 %>% print()
pause()
# Brf2_: trees vs. error rate
message("\t\n\n\n || [3.0.10] plotting Brf2_: [trees vs error rate] ...")
df <- as.data.frame(plot(Brf2_$finalModel))
df <- df %>% mutate(trees=1:Brf2_$finalModel$ntree)
df2 <- reshape2::melt(df,id.vars="trees")
colnames(df2)[2:3] <- c("type","error")
p4 <- ggplot(data=df2,mapping=aes(x=trees,y=error,color=factor(type)))
p4 <- p4 + geom_line(stat="identity")
p4 <- p4 + scale_colour_brewer(palette="Greys")
p4 <- p4 + labs(title = "Brf2_: Trees vs. Error Rate",
		x = "Trees",
		y = "Error rate")
p4 <- p4 + my_theme() + theme(legend.position = "none")
p4 %>% print()
pause()
# view all plots
message("\t\n\n\n || [3.0.11] viewing all plots...")
plot <- ggarrange(p1,p2,p3,p4, nrow=2, ncol=2)
plot %>% print()
ggsave("./report_and_figures/figures/plot2.png",plot,dpi=500,width=45,height=25,units="cm",device="png")
# complete model selection
message("\t\n\n\n || [3.0.12] completing model selection...")
varImp(Bcart_) %>% print()
varImp(Brf_) %>% print()
varImp(Bcart2_) %>% print()
varImp(Brf2_) %>% print()
modelSelect <- list("|[training set]|"=tibble(trn),"|[pre-testing set]|"=tibble(tst.pre),
		    "|[final-testing set]|"=tibble(tst.final),"|[preferred model]|"=Brf_)
modelSelect %>% print()