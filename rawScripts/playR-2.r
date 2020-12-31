require(dplyr)
require(ggplot2)
require(ggpubr)
require(car))
require(MASS)

#==========[case 1]
	#--[0] data cleaning
df <- mtcars
df$mpg.c <- df$mpg - mean(df$mpg)
	#--[1] descriptive statistics
p1 <- qplot(y=mpg.c, x=hp, data=df, geom="point", color=am) # [bad] linear regression
p2 <- qplot(y=am, x=mpg.c, data=df, geom="point", color=am) # [good] logistic regression | https://support.minitab.com/en-us/minitab-express/1/help-and-how-to/modeling-statistics/regression/how-to/simple-binary-logistic-regression/interpret-the-results/all-statistics-and-graphs/graphs/
ggarrange(p1, p2, nrow=1, ncol=2)
	#--[2] model
X <- as.matrix( cbind(1, df$mpg.c, df$wt) ); colnames(X) <- c("(Int.)", "mpg.c", "wt")
Y <- matrix( df$am ); colnames(Y) <- "am"
B_ <- matrix(rep(0,dim(X)[2])); colnames(B_) <- NULL
	#--[3] estimates
n <- matrix(1:dim(X)[1])
converge <- function(X=NULL, Y=NULL, B.old=NULL, trials=100000)
{
	i <- 0
	while ( i < trials )
	{
		# calculate maximum likelihood estimates via newton-raphson method
		ETA <- X %*% B.old
		p <- (1)/(1 + exp(-ETA))
		W <- diag( x=as.vector( (t(n)%*%p)[1] * (1-p) ) )
		ETA_ <- ETA + solve(W) %*% (Y - p)
		B.new <- solve( t(X) %*% W %*% X ) %*% (t(X) %*% W) %*% ETA_
		# check for convergence
		if ( ((B.old != B.new)[1] == TRUE) & ((B.old != B.new)[2] == TRUE) )
		{
			B.old <- B.new
			next
		}
		else
		{
			break
		}
		# reapply newton-raphson method
		i <- i + 1
	}
	list(ETA_, B.new, W, p)
}
values <- converge(X=X, Y=Y, B.old=B_)
ETA_ <- values[[1]]
o <- exp(ETA_) # odds
p_ <- (o)/(1+o) # same as p

B_ <- values[[2]]
W <- values[[3]]
p <- values[[4]]

LOGIT.Y <- log(p/(1-p))
e_ <- p - p_; # residual
	#--[4] sum of squares
ss.res <- t(p - p_) %*% (p - p_)
ss.reg <- t(p - mean(p_)) %*% (p - mean(p_))
ss.tot <- ss.res + ss.reg
R.2 <- ss.reg/ss.tot # or R.2 <- ( 1 - (ss.res/ss.tot) )
	#--[5] result
fit1 <- glm(am ~ mpg.c + wt, family=binomial, data=df)
verify.1 <- list(BUILT.in.FITTED=head( matrix(predict(fit1, type="response")) ), p.VALUES.vs.FITTED=head(cbind(p, p_)),
		 PREDICTORS=head(X), BUILT.in.COEF=summary( fit1 )$coef, COEFFICIENTS=B_, RESIDS=e_)
	#--[6] model selection | check for colinearity and multicollinearity with VIF score
df.vif <- X[,-1]

XN <- matrix( df.vif[,2] ); colnames(XN) <- "wt"  # [mpg.c] as a function of all the other regressors
X1 <- matrix( df.vif[,1] ); colnames(X1) <- "mpg.c"
B.X1_ <- solve(t(XN) %*% XN) %*% (t(XN) %*% X1); colnames(B.X1_) <- NULL
X1_ <- XN %*% B.X1_
ss.res.vif <- t(X1 - B.X1_[1]) %*% (X1 - B.X1_[1]); rownames(ss.res.vif) <- NULL
ss.reg.vif <- t(X1 - mean(B.X1_[1])) %*% (X1 - mean(B.X1_[1])); rownames(ss.reg.vif) <- NULL
ss.tot.vif <- ss.res.vif + ss.reg.vif
R.2.vif <- ss.reg.vif/ss.tot.vif
VIF.X1 <- (1/(1 - R.2.vif))

XN <- matrix( df.vif[,1] ); colnames(XN) <- "mpg.c"  # [wt] as a function of all the other regressors
X2 <- matrix( df.vif[,2] ); colnames(X2) <- "wt"
B.X2_ <- solve(t(XN) %*% XN) %*% (t(XN) %*% X2); colnames(B.X2_) <- NULL
X2_ <- XN %*% B.X2_
ss.res.vif <- t(X2 - B.X2_[1]) %*% (X2 - B.X2_[1]); rownames(ss.res.vif) <- NULL
ss.reg.vif <- t(X2 - mean(B.X2_[1])) %*% (X2 - mean(B.X2_[1])); rownames(ss.reg.vif) <- NULL
ss.tot.vif <- ss.res.vif + ss.reg.vif
R.2.vif <- ss.reg.vif/ss.tot.vif
VIF.X2 <- (1/(1 - R.2.vif))

cutoff.at.3 <- matrix( rbind(VIF.X1, VIF.X2) ) > 3
cutoff.at.5 <- matrix( rbind(VIF.X1, VIF.X2) ) > 5
cutoff.at.10 <- matrix( rbind(VIF.X1, VIF.X2) ) > 10
VIF <- data.frame( "Features"=c("mpg.c", "wt"), "VIF.Factor"=c(VIF.X1, VIF.X2), "VIF_3"=cutoff.at.3,
		   "VIF_5"=cutoff.at.5, "VIF_10"=cutoff.at.10)
vif( fit1 ) # vif = 3; remove 1 regressor
verify.2 <- list(BUILT.in.VIF=vif( fit1 ), VIF=VIF)

verify.1 # regression model
verify.2 # model validation

p3 <- qplot(y=wt, x=mpg.c, data=df, geom="point")



#==========[case 2]
	#--[0] data cleaning
df <- mtcars
df$mpg.c <- df$mpg - mean(df$mpg)
	#--[2] model
X <- as.matrix( cbind(1, df$mpg.c) ); colnames(X) <- c("(Int.)", "mpg.c")
Y <- matrix( df$am ); colnames(Y) <- "am"
B_ <- matrix(rep(0,dim(X)[2])); colnames(B_) <- NULL
	#--[3] estimates
n <- matrix(1:dim(X)[1])
converge <- function(X=NULL, Y=NULL, B.old=NULL, trials=100000)
{
	i <- 0
	while ( i < trials )
	{
		# calculate maximum likelihood estimates via newton-raphson method
		ETA <- X %*% B.old
		p <- (1)/(1 + exp(-ETA))
		W <- diag( x=as.vector( (t(n)%*%p)[1] * (1-p) ) )
		ETA_ <- ETA + solve(W) %*% (Y - p)
		B.new <- solve( t(X) %*% W %*% X ) %*% (t(X) %*% W) %*% ETA_
		# check for convergence
		if ( ((B.old != B.new)[1] == TRUE) & ((B.old != B.new)[2] == TRUE) )
		{
			B.old <- B.new
			next
		}
		else
		{
			break
		}
		# reapply newton-raphson method
		i <- i + 1
	}
	list(ETA_, B.new, W, p)
}
values <- converge(X=X, Y=Y, B.old=B_)
ETA_ <- values[[1]]
o <- exp(ETA_) # odds
p_ <- (o)/(1+o) # same as p

B_ <- values[[2]]
W <- values[[3]]
p <- values[[4]]

LOGIT.Y <- log(p/(1-p)) # logit scale
	#--[4] sum of squares
ss.res <- t(p - p_) %*% (p - p_)
ss.reg <- t(p - mean(p_)) %*% (p - mean(p_))
ss.tot <- ss.res + ss.reg
R.2 <- ss.reg/ss.tot # or R.2 <- ( 1 - (ss.res/ss.tot) )
	#--[5] mean squares
# HESSIAN <- -t(X) %*% W %*% X
# INFO.MAT <- -( HESSIAN )
# SE.. <- solve(INFO.MAT);  colnames(SE..) <- "Std. Error"
	#--[6]. hypothesis test | t-test (linear relation)
# z.B_ <- [...];  colnames(z.B_) <- "z value"
# z.alpha <- qnorm(1-(0.05/2))
# p_val.B_ <- 2*pnorm(z.B_);  colnames(p_val.B_) <- "Pr(>|z|)"
# p_val.alpha <- 2*pnorm(...)
	#--[7].  influencial observations and leverage | https://data.princeton.edu/wws509/r/c2s9
H <- X %*% solve(t(X) %*% X) %*% t(X)
create.hii <- function(mat=NULL)
{
# 	vec <- matrix(rep(0, nrow(mat)))
# 	for (i in 1:nrow(mat))
# 	{
# 		for (j in 1:ncol(mat))
# 		{
# 			if ( i == j)
# 			{
# 				vec[i] <- mat[i,j] 
# 			}
# 		}
# 	}
# 	vec
}
hii <- create.hii(mat=H) # leverage
e_ <- p - p_; # residual
# r_ <- e_/sqrt(ms.res[1] * (1 - hii)) # studentized residual
# D_ <- ((r_^2)/(k+1)) * ((hii)/(1-hii)) # Cook's distance
# inf.measures <- cbind(head(Y), head(Y_), head(e_), head(hii), head(r_),  head(D_))
# colnames(inf.measures) <- c("yi", "yi_", "ei_", "hii", "ri_", "Di_")
# infl <- c( "hii_LEVERAGE"=(2*(k + 1))/n )
	#--[.] result
fit2 <- glm(am ~ mpg.c, family=binomial, data=df)
verify.1 <- list(BUILT.in.FITTED=head( matrix(predict(fit2, type="response")) ), p.VALUES.vs.FITTED=head(cbind(p, p_)),
		 PREDICTORS=head(X), BUILT.in.COEF=summary( fit2 )$coef, COEFFICIENTS=B_, RESIDS=e_)
	#--[.] interpretation
df2 <- data.frame(Y, X=X[,2])
df3 <- data.frame(p_, Y, X=X[,2])

p.reg <- ggplot() # MPG vs. fitted [plot]
p.reg <- p.reg + geom_point(data=df2, mapping=aes(x=X, y=Y), alpha=0.4, color="black")
p.reg <- p.reg + geom_segment(data=df3, mapping=aes(x=X, y=Y, xend=X, yend=p_), color="grey30", alpha=0.2)
p.reg <- p.reg + geom_line(data=df3, mapping=aes(x=X, y=p_), color='grey30')
p.reg <- p.reg + labs(title = "MPG (centered) vs. Fitted",
		x = "MPG (centered)",
		y = "Auto-Manual (in Probabilties)")
#p.reg <- p.reg + my_theme()

p.res <- ggplot(data=df2, aes(x=X, y=e_)) # residuals vs. fitted [plot]
p.res <- p.res + geom_segment(aes(xend=X, yend=0), color="grey30", alpha=0.2)
p.res <- p.res + geom_point(alpha=0.4, color="black")
p.res <- p.res + geom_hline(yintercept=0, color='black')
p.res <- p.res + labs(title = "Residuals vs. Fitted",
		x = "Fitted",
		y = "Residuals")
#p.res <- p.res + my_theme()