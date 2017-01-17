library(plsdepot)
data(vehicles)
cars = vehicles[ ,c(1:12,14:16,13)]


pls1 = plsreg1(cars[, 1:15], cars[, 16, drop = FALSE], comps = 3)
pls1$R2
plot(pls1)

plot(cars$price, pls1$y.pred, type = "n", xlab="Original", ylab = "Predicted")
title("Comparison of responses", cex.main = 0.9)
abline(a = 0, b = 1, col = "gray85", lwd = 2)
text(cars$price, pls1$y.pred, col = "#5592e3")


library(pls)
data(yarn)
data(oliveoil)
data(gasoline)

gasTrain <- gasoline[1:50,]
gasTest <- gasoline[51:60,]

gas1 <- plsr(octane ~ NIR, ncomp = 10, data = gasTrain, validation = "LOO")
gas_pred <- predict(gas1, ncomp = 2, newdata = gasTest)

plot(gasTest$octane, gas_pred, type="p")
