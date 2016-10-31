#


require(tseries)
rm(list = ls())

dat_stock <- read.csv("StockData.csv")

two_asset = dat_stock[,3:4]

w1 = 0.5
n= nrow(two_asset)
returns = (two_asset[-1,]/two_asset[-n,]) - 1 #Computes simple returns
R1=returns[,1]
R2=returns[,2]
Rp = w1*R1 + (1-w1)*R2

meu_P = w1*mean(R1) + (1-w1)*mean(R2)
var_p = (w1*w1)*(sd(R1)*sd(R1)) + ((1-w1)*(1-w1))*(sd(R2)*sd(R2))+ 2*w1*(1-w1)*sd(R1)*sd(R2)*cor(R1,R2)
w_opt = ((sd(R2))^2 - sd(R1)*sd(R2)*cor(R1,R2))/((sd(R1))^2 - 2*sd(R1)*sd(R2)*cor(R1,R2) + (sd(R2))^2)
w_opt
var_p
#0.002221649(minimum at w1 = 0.8168216)

#Efficient frontier when row =1
w1=0.5
meu_P = w1*mean(R1) + (1-w1)*mean(R2)
var_p1 = ((w1*sd(R1)) + (1-w1)*sd(R2))^2
var_p2 = ((w1*sd(R1)) - (1-w1)*sd(R2))^2

sigma1 = sqrt(var_p1)
sigma2 = sqrt(var_p2)

w1 = (sigma1 - sd(R2))/(sd(R1)-sd(R2))
w2 = (-sigma2 + sd(R2))/(sd(R1)+sd(R2))


######################################

library(data.table)
library(ggplot2)
library(scales)

er_x <- mean(R1)
er_y <- mean(R2)

# II) risk (standard deviation) as a risk measure
sd_x <- sd(R1)
sd_y <- sd(R2)

# III) covariance
cov_xy <- cov(R1, R2)
#cov_xy <- sd(R1)*sd(R2)  #row = 1 straight line
#cov_xy <- -sd(R1)*sd(R2) #row = -1 two straight lines
#cov_xy = 0               #curve

# create 1000 portfolio weights (omegas)
x_weights <- seq(from = 0, to = 1, length.out = 1000)

# create a data.table that contains the weights for the two assets
two_assets <- data.table(wx = x_weights,
                         wy = 1 - x_weights)

# calculate the expected returns and standard deviations for the 1000 possible portfolios
two_assets[, ':=' (er_p = wx * er_x + wy * er_y,
                   sd_p = sqrt(wx^2 * sd_x^2 +
                                 wy^2 * sd_y^2 +
                                 2 * wx * (1 - wx) * cov_xy))]
# lastly plot the values
ggplot() +
  geom_point(data = two_assets, aes(x = sd_p, y = er_p, color = wx)) +
  geom_point(data = data.table(sd = c(sd_x, sd_y), mean = c(er_x, er_y)),
             aes(x = sd, y = mean), color = "red", size = 3, shape = 18) +
  # Miscellaneous Formatting
  theme_bw() + ggtitle("Possible Portfolios with Two Risky Assets") +
  xlab("Volatility") + ylab("Expected Returns") +
  scale_y_continuous(label = percent, limits = c(0, max(two_assets$er_p) * 1.2)) +
  scale_x_continuous(label = percent, limits = c(0, max(two_assets$sd_p) * 1.2)) +
  scale_color_continuous(name = expression(omega[x]), labels = percent)
