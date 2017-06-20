library(rstan)
library(loo)

d <- read.csv(file='input/data-50m.txt', header=TRUE)
data <- list(N=nrow(d), Age=d$Age, Weight=d$Weight, Y=d$Y)
fit1 <- stan(file='model/model7-5.stan', data=data, seed=1234)
fit1

fit2 <- stan(file='model/model7-5a.stan', data=data, seed=1234)
fit2


################################################################
# パッケージlooを用いたWAICの算出例

log_lik1 <- extract_log_lik(fit1)
waic1 <- waic(log_lik1)
print(waic1 , digits = 4)

# モデル2のWAIC
log_lik2 <- extract_log_lik(fit2)
waic2 <- waic(log_lik2)
print(waic2 , digits = 4)

# WAICの比較
compare(waic1, waic2)

