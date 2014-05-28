set.seed(123) # stejné výsledky na všech PC
muzi <- rnorm(50, mean=6.8, sd=1)
zeny <- rnorm(50, mean=7, sd=1)
data.all <- data.frame(cbind(hodnoty=c(muzi,zeny), pohlavi=rep(c(0,1),each=50)) )
boxplot(data.all$hodnoty ~ factor(data.all$pohlavi), main="Srovnání støedních hodnot")

t.test(hodnoty~pohlavi, data=data.all)
t.test(hodnoty~pohlavi, data=data.all, alternative="l")
t.test(hodnoty~pohlavi, data=data.all, alternative="g")

prop.test(x=c(75, 70), n=c(100, 100), alternative="l")
prop.test(x=c(75, 70), n=c(100, 100))
prop.test(x=c(750, 700), n=c(1000, 1000))

data.chi <- as.data.frame(matrix(c(30, 90, 60, 40),
byrow=TRUE, ncol=2))

chisq.test(data.chi)
chisq.test(data.chi, simulate.p.val=TRUE, B=5000)
