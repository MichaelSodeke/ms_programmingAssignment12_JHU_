# [1-C1_dataEng.r]

## ---------------- create directory --------------------------------------------------------
if (!file.exists("CrossFitData"))
{
	dir.create("CrossFitData")
}

## ---------------- create pauser -----------------------------------------------------------
pause <- function(x=2.5)
{                                                                                                                                                                                                                                                                                                            
	message("\n || >>> current file: [1-C1_dataEng.r]")
	message("\n\t Preparing [next] code chunck...")
	p1 <- proc.time()
        Sys.sleep(x)
        proc.time() - p1                                                                                                                                                                                                                                                                 
}

## ---------------- get link location -------------------------------------------------------
##-ls("package:...")
##-args(...) %>% print()
message("\n\n | [1.] downloading data via URL link...")
message(" --------------------------------------------------------------------")
trnUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
tstUrl <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
#download.file(trnUrl, destfile="CrossFitData/fitTrain.csv", method = "curl", mode="wb")
#download.file(tstUrl, destfile="CrossFitData/fitTest.csv", method = "curl", mode="wb")
#print(list.files("./CrossFitData"))
pause()
## ---------------- read data ---------------------------------------------------------------
message("\n\n | [1.2] reading datasets...")
message(" --------------------------------------------------------------------")
file1 <- "./CrossFitData/fitTrain.csv"
file2 <- "./CrossFitData/fitTest.csv"
df.trn <- read.csv(file1)
df.tst <- read.csv(file2)
pause()
## ---------------- data cleaning -----------------------------------------------------------
message("\n\n | [1.3] cleaning datasets...")
message(" --------------------------------------------------------------------")
# check data summary
message("\t\n\n\n || [1.3.1] checking data summary...")
df.trn %>% str()
df.tst %>% str()
pause()
# convert features to lower case
message("\t\n\n\n || [1.3.2] converting features to lower case...")
colnames(df.trn) <- str_to_lower(colnames(df.trn))
colnames(df.tst) <- str_to_lower(colnames(df.tst))
pause()
# check for na values in training set
message("\t\n\n\n || [1.3.3] checking for na values in training set...")
columns.trn1 <- matrix(colnames(df.trn))
datatype.trn1 <- matrix( sapply( columns.trn1,function(x){ class( df.trn[,x] ) } ) )
check.nas1 <- matrix( sapply( columns.trn1,function(x){ sum(is.na( df.trn[,x] )*1) } ) )
info1 <- cbind(columns.trn1,datatype.trn1,check.nas1)
colnames(info1) <- c("name","type","na")
info1 %>% head(30) %>% print()
pause()
# check for na values in testing set
message("\t\n\n\n || [1.3.4] checking for na values in testing set...")
columns.tst1 <- matrix(colnames(df.tst))
datatype.tst1 <- matrix( sapply( columns.tst1,function(x){ class( df.tst[,x] ) } ) )
check.nas2 <- matrix( sapply( columns.tst1,function(x){ sum(is.na( df.tst[,x] )*1) } ) )
info2 <- cbind(columns.tst1,datatype.tst1,check.nas2)
colnames(info2) <- c("name","type","na")
info2 %>% head(30) %>% print()
pause()
# remove na values
message("\t\n\n\n || [1.3.5] removing na values...")
srch1 <- data.frame(info1)$na == 0
srch2 <- data.frame(info2)$na == 0
pause()
# recheck for na values in training set
message("\t\n\n\n || [1.3.6] rechecking for na values in training set...")
columns.trn2 <- columns.trn1[srch1]
datatype.trn2 <- matrix( sapply( columns.trn2,function(x){ class( df.trn[,x] ) } ) )
check.nas3 <- matrix( sapply( columns.trn2,function(x){ sum(is.na( df.trn[,x] )*1) } ) )
info3 <- cbind(columns.trn2,datatype.trn2,check.nas3)
colnames(info3) <- c("name","type","na")
info3 %>% head(30) %>% print()
pause()
# recheck for na values in testing set
message("\t\n\n\n || [1.3.7] rechecking for na values in testing set...")
columns.tst2 <- columns.tst1[srch2]
datatype.tst2 <- matrix( sapply( columns.tst2,function(x){ class( df.tst[,x] ) } ) )
check.nas4 <- matrix( sapply( columns.tst2,function(x){ sum(is.na( df.tst[,x] )*1) } ) )
info4 <- cbind(columns.tst2,datatype.tst2,check.nas4)
colnames(info4) <- c("name","type","na")
info4 %>% head(30) %>% print()
pause()
# check character type predictors in training set
message("\t\n\n\n || [1.3.8] checking character type predictors in training set...")
df.trn2 <- df.trn[,columns.trn2]
srch3 <- matrix( sapply( colnames(df.trn2),function(x){ is.character( df.trn2[,x] ) } ) )
df.ch <- df.trn2[,srch3]
str(df.ch)
srch4 <- matrix( sapply( colnames(df.ch),
		    function(x){ if (sum((df.trn2[,x] == "")*1) > 1 ){ TRUE }else{ FALSE } } ) )
newTypes <- matrix( sapply( colnames(df.ch[,srch4]),
		    function(x){ as.numeric( df.trn2[,x] ) } ), ncol=ncol(df.ch[,srch4]) )
df.ch[,srch4] <- newTypes
df.trn2[,colnames(df.ch[,srch4])] <- newTypes
df.trn2 %>% str()
pause()
# check character type predictors in testing set
message("\t\n\n\n || [1.3.9] checking character type predictors in testing set...")
df.tst2 <- df.tst[,columns.tst2]
srch5 <- matrix( sapply( colnames(df.tst2),function(x){ is.character( df.tst2[,x] ) } ) )
df.ch <- df.trn2[,srch5]
df.ch %>% str()
srch6 <- matrix( sapply( colnames(df.ch),
		    function(x){ if (sum((df.tst2[,x] == "")*1) > 1 ){ TRUE }else{ FALSE } } ) )
newTypes <- matrix( sapply( colnames(df.ch[,srch6]),
		    function(x){ as.numeric( df.tst2[,x] ) } ), ncol=ncol(df.ch[,srch6]) )
df.ch[,srch6] <- newTypes
df.tst2[,colnames(df.ch[,srch6])] <- newTypes
df.tst2 %>% str()
pause()
# recheck for na values in training set
message("\t\n\n\n || [1.3.10] rechecking for na values in training set...")
columns.trn3 <- matrix(colnames(df.trn2))
datatype.trn3 <- matrix( sapply( columns.trn3,function(x){ class( df.trn2[,x] ) } ) )
check.nas3 <- matrix( sapply( columns.trn3,function(x){ sum(is.na( df.trn2[,x] )*1) } ) )
info5 <- cbind(columns.trn3,datatype.trn3,check.nas3)
colnames(info5) <- c("name","type","na")
info5 %>% head(30) %>% print()
pause()
# remove na values
message("\t\n\n\n || [1.3.11] remove na values...")
srch7 <- data.frame(info5)$na == 0
pause()
# recheck for na values in training set
message("\t\n\n\n || [1.3.12] rechecking for na values in training set...")
columns.trn4 <- columns.trn3[srch7]
datatype.trn4 <- matrix( sapply( columns.trn4,function(x){ class( df.trn2[,x] ) } ) )
check.nas4 <- matrix( sapply( columns.trn4,function(x){ sum(is.na( df.trn2[,x] )*1) } ) )
info6 <- cbind(columns.trn4,datatype.trn4,check.nas4)
colnames(info6) <- c("name","type","na")
info6 %>% head(30) %>% print()
pause()
# check columns in training and testing set
message("\t\n\n\n || [1.3.13] checking columns in training and testing set...")
df.trn3 <- df.trn2[,columns.trn4]
df.trn3 %>% str()
df.tst2 %>% str()
colnames(df.trn3) == colnames(df.tst2)
pause()
# fix date formating
message("\t\n\n\n || [1.3.14] fixing date formating...")
df.trn3$cvtd_timestamp <- dmy_hm(df.trn3$cvtd_timestamp)
df.tst2$cvtd_timestamp <- dmy_hm(df.tst2$cvtd_timestamp)
pause()
# prepare training set
message("\t\n\n\n || [1.3.15] preparing training set...")
df.trn3$classe <- factor(df.trn3$classe)
df.trn3 <- df.trn3[,-c(1:7)]
df.trn4 <- data.frame(df.trn3)
srch <- createDataPartition(df.trn4$classe, p = 3/4)[[1]]
trn <- df.trn4[srch,]
trn %>% str()
View(trn)
pause()
# prepare pre testing set
message("\t\n\n\n || [1.3.16] preparing [pre] testing set...")
tst.pre <- df.trn4[-srch,]
tst.pre %>% str()
View(tst.pre)
pause()
# prepare final testing set
message("\t\n\n\n || [1.3.17] preparing [final] testing set...")
df.tst2 <- df.tst2[,-c(1:7)]
#df.tst2$problem_id <- NULL
tst.final <- data.frame(df.tst2)
tst.final %>% str()
View(tst.final)
pause()