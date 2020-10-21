# Example
# load dataset for portfolios and covariates
data("SizedBM25")
data("mu_sigma")
data("ff_factors")

# select portfolio excess return for period of interest
# get factor data for period of interest
start.date <- 19640101
end.date <- 20151231
data.port <- subset(ff25_day, Date >= start.date & Date <= end.date)
data.factor <- subset(mu_sigma, Date >= start.date & Date <= end.date)
data.factor1 <- subset(ff_factors, Date >= start.date & Date <= end.date)
data.factor$SMB <- data.factor1$SMB
data.factor$HML <- data.factor1$HML

# check if the dates of portfolios and covariates matched
stopifnot(all(data.port$Date == data.factor$Date))

# get year and month
data.yyyymm <- as.numeric(substr(data.port$Date, 1, 6))
month.list <- sort(unique(data.yyyymm))

# get excess return of each portfolio
data.port <- data.port[, -1]
data.port.exc.ret <- as.data.frame(apply(data.port, 2, FUN = function(x, y){x - y}, data.factor$R_f))

# get monthly excess return of each portfolio
port.exc.ret.month <- aggregate(data.port.exc.ret, by = list(yyyymm = data.yyyymm), FUN = GetMonthReturn)[, -1]

# set rolling window, unit: month
roll.width <- 12
tol <- 0.001
MaxIter <- 1000
intercept_include <- FALSE
criterion <- 1
params <- c(tol, MaxIter, criterion)

# keep only the one-month ahead monthly return in testing period
port.exc.ret.month.test <- port.exc.ret.month[-(1:roll.width), ]

# Specify the months used for model estimation
month.list.est <- month.list[-length(month.list)]
zcapm.res <- ZCAPM(data.port.exc.ret, month.list.est, roll.width, data.yyyymm, data.factor[, c("R_a.R_f", "sigma_a")], params)
capm.res <- FactorModel(data.port.exc.ret, month.list.est, roll.width, data.yyyymm, data.frame(R_a.R_f = data.factor[, "R_a.R_f"]), intercept_include)
ff3.res <- FactorModel(data.port.exc.ret, month.list.est, roll.width, data.yyyymm, data.factor[, c("R_a.R_f", "SMB", "HML")], intercept_include)

num.mod <- nrow(zcapm.res$factor.loading) / 2
zcapm.res$factor.loading[(num.mod + 1): (num.mod * 2), ] <- zcapm.res$factor.loading[(num.mod + 1): (num.mod * 2), ] * 21

# test
fact.load.lst <- list(ZCAPM = zcapm.res,
                      LM = capm.res,
                      FF3 = ff3.res)
test_res <- fm_test(port.exc.ret.month.test, fact.load.lst)
print.summary(test_res)
