# 8장. 범주형 자료분석

# 8-1. 적합도 검정
x <- seq(0, 15, by=0.01)
dc <- dchisq(x, df=3)
alpha <- 0.05
tol <- qchisq(1-alpha, df=3)

par(mar=c(0,1,1,1))
plot(x, dc, type="l", axes=F, ylim=c(-0.03, 0.25), xlab="", ylab="")
abline(h=0)
tol.g <- round(tol, 2)
polygon(c(tol.g, x[x>tol.g], 15), c(0, dc[x>tol.g], 0), col="red")
text(0, -0.03, "0", cex=0.8)
text(tol, -0.03, expression(chi[0.05]^{2}==2.14), cex=0.8)

alpha2 <- 0.9254
tol2 <- qchisq(1-alpha2, df=3)
tol2.g <- round(tol2, 2)
polygon(c(tol2.g, x[x>tol2.g], 15), c(0, dc[x>tol2.g], 0), col="orange", density=20, angle=305)
text(tol2, -0.03, expression(chi[0.9254]^{2}==0.47), cex=0.8)
arrows(7, 0.17, 4, 0.12, length=0.05)
text(7, 0.177, expression(plain(P)(F>0.47) == 0.9254), cex=0.8)

# 1) 멘델의 유전법칙
x <- c(315, 101, 108, 32)
chisq.test(x, p=c(9, 3, 3, 1)/16) 
