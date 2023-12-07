# 加载rjags包
library(rjags)

# 假设你的数据框架名为df，因变量为y，自变量为x1, x2, x3
data=na.omit(data)
# 定义JAGS模型
modelString = "
model {
  for (i in 1:N) {
    y[i] ~ dnorm(mu[i], tau)
    mu[i] <- beta0 + beta1*x1[i] + beta2*x2[i] + beta3*x3[i]+beta4*x4[i]+beta5*x5[i]+beta6*x6[i]+beta7*x7[i]+beta8*x8[i]
  }
  beta0 ~ dnorm(0, 0.0001)
  beta1 ~ dnorm(0, 0.0001)
  beta2 ~ dnorm(0, 0.0001)
  beta3 ~ dnorm(0, 0.0001)
  beta4 ~ dnorm(0, 0.0001)
  beta5 ~ dnorm(0, 0.0001)
  beta6 ~ dnorm(0, 0.0001)
  beta7 ~ dnorm(0, 0.0001)
  beta8 ~ dnorm(0, 0.0001)
  tau ~ dgamma(0.01, 0.01)
  sigma <- 1/sqrt(tau)
}
"

# 准备数据给JAGS
dataList = list(
  y = data$`% Adults with Obesity`,
  x1 = data$`Dentist Rate`,
  x2 = data$`Income Ratio`,
  x3 = data$`% Fair or Poor Health`,
  x4= data$`% Smokers`,
  x5=data$`% Excessive Drinking`,
  x6=data$`% Uninsured`,
  x7=data$`% Completed High School`,
  x8=data$`% Unemployed`,
  N = nrow(data)
)

# 初始化参数
initsList = list(
  beta0 = 0,
  beta1 = 0,
  beta2 = 0,
  beta3 = 0,
  beta4 = 0,
  beta5 = 0,
  beta6 = 0,
  beta7 = 0,
  beta8 = 0,
  tau = 1
)

# 设置模型参数
parameters = c("beta0", "beta1","beta2","beta3","beta4","beta5","beta6","beta7","beta8","sigma")

# 运行模型
model <- jags.model(textConnection(modelString), data = dataList, inits = initsList, n.chains = 3)
update(model, 1000) # burn-in期
samples <- coda.samples(model, variable.names = parameters, n.iter = 5000)

# 查看结果
summary(samples)
