library(ISLR)
library(leaps)
library(glmnet)

func <- function(X,y) {
    
    # * Set your variables ----
    # We set our vairables internally to work 
    # with them inside of other functions
    x <- X
    y <- y
    
    # * Best Subset Selection ----
    regfit.bss  <- regsubsets(x,y)
    bss.summary <- summary(regfit.bss)
    bss.coef    <- coef(regfit.bss, which.min(bss.summary$bic))
    
    # * Forword Subset Selection ----
    regfit.fss  <- regsubsets(x,y, method = "forward")
    fss.summary <- summary(regfit.fss)
    fss         <- coef(regfit.fss,which.min(fss.summary$bic))
    
    # * Backward Subset Selection ----
    regfit.backward <- regsubsets(x,y, method = "backward")
    bss.summary     <- summary(regfit.backward)
    backward        <- coef(regfit.backward,which.min(bss.summary$bic))
    
    # * Lasso ----
    # Create a grid sequence of length 100
    grid      <- 10^seq(10,-2, length=100)
    # Run the glmnet on the x and y variables with lambda set to a search grid
    lasso.mod <- glmnet(x,y, alpha = 1, lambda = grid)
    cv.out    <- cv.glmnet(x,y, alpha = 1)
    best.lam  <- cv.out$lambda.min
    las       <- coef(lasso.mod,best.lam)
    
    newlist <- list("Best_Subset_Selection" = bss, "Forword_Subset_Selection" = fss,
                   "Backward_Subset_Selection" = backward, "Lasso" = las)
    return(newlist)
}

# For the hitters data the Best Subset Selection is the same as the Forward
# Subset Selection
x1 = model.matrix(Salary ~ . , data = Hitters)[,-1]
y1 = na.omit(Hitters$Salary)
func(x1, y1)
