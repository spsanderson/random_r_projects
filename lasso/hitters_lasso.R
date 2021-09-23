library(ISLR)
library(leaps)
library(glmnet)

func <- function(X,y) {
    
    # * Set your variables ----
    x <- X
    y <- y
    
    # Best Subset Selection
    regfit.bss  = regsubsets(x,y)
    bss.summary = summary(regfit.bss)
    bss.coef    = coef(regfit.bss, which.min(bss.summary$bic))
    
    # Forword Subset Selection
    regfit.fss  = regsubsets(x,y, method = "forward")
    fss.summary = summary(regfit.fss)
    fss         = coef(regfit.fss,which.min(fss.summary$bic))
    
    # Backword Subset Selection
    regfit.backward = regsubsets(x,y, method = "backward")
    bss.summary     = summary(regfit.backward)
    backward        = coef(regfit.backward,which.min(bss.summary$bic))
    
    # Lasso
    grid      <- 10^seq(10,-2, length=100)
    lasso.mod <- glmnet(x,y, alpha = 1, lambda = grid)
    cv.out    <- cv.glmnet(x,y, alpha = 1)
    best.lam  <- cv.out$lambda.min
    las       <- coef(lasso.mod,best.lam)
    
    newlist <- list("Best Subset Selection" = bss, "Forword Subset Selection" = fss,
                   "Backward Subset Selection" = backward, "Lasso" = las)
    return(newlist)
}

x1 = model.matrix(Salary ~ . , data = Hitters)[,-1]
y1 = na.omit(Hitters$Salary)

func(x1, y1)
