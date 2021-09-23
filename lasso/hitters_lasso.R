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
    
    # Lasso
    grid      <- 10^seq(10,-2, length=100)
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


n <- 10
df <- data.frame(matrix(nrow = 1, ncol = 7))
colnames(df) <- c('Intercept','AtBat','Hits','Walks','CRBI','DivisionW','PutOuts')
for(i in 1:n){
    print(i)
    output <- func(x1, y1)
    bss    <- output

    df     <- rbind(df, df_tmp)
    
}
func(x1, y1)
