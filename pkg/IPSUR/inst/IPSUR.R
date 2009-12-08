###################################################
### chunk number 1: 
###################################################
###  IPSUR.R - Introduction to Probability and Statistics Using R
###  Copyright (C) 2009  G. Jay Kerns, <gkerns@ysu.edu>
###  This program is free software: you can redistribute it and/or modify
###  it under the terms of the GNU General Public License as published by
###  the Free Software Foundation, either version 3 of the License, or
###  (at your option) any later version.
###  This program is distributed in the hope that it will be useful,
###  but WITHOUT ANY WARRANTY; without even the implied warranty of
###  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
###  GNU General Public License for more details.
###  You should have received a copy of the GNU General Public License
###  along with this program.  If not, see <http://www.gnu.org/licenses/>
###################################################


###################################################
### chunk number 2: 
###################################################
set.seed(42)
#library(random)
#i_seed <- randomNumbers(n = 624, col = 1, min = -1e+09, max = 1e+09)
#.Random.seed[2:626] <- as.integer(c(1, i_seed))
#save.image(file = "seed.RData")


###################################################
### chunk number 3: 
###################################################
options(useFancyQuotes = FALSE)
#library(prob)
library(RcmdrPlugin.IPSUR)
# Generate RcmdrTestDrive
n <- 168
# generate order 
order <- 1:n
# generate race 
race <- sample(c("White","AfAmer","Asian","Other"), size=n, prob=c(76,13,5,6), replace = TRUE)
race <- factor(race)
# generate gender and smoke 
tmp <- sample(4, size=n, prob=c(12,38,9,41), replace = TRUE) 
gender <- factor(ifelse(tmp < 3,"Male", "Female")) 
smoke <- factor(ifelse(tmp %in% c(1,3), "Yes", "No"))
# generate parking 
parking <- rgeom(n, prob = 0.4) + 1
# generate salary 
m <- 17 + (as.numeric(gender)-1) 
s <- 1 + (2 - as.numeric(gender)) 
salary <- rnorm(n, mean = m, sd = s)
# simulate reduction 
x <- arima.sim(list(order=c(1,0,0), ar=.9), n=n) 
reduction <- as.numeric((20*x + order)/n + 5)
# simulate before and after
before <- rlogis(n, location = 68, scale = 3) 
m <- (as.numeric(smoke)-1)*2.5 
after <- before - rnorm(n, mean = m, sd=0.1)
RcmdrTestDrive <- data.frame(order = order, race = race, smoke = smoke, gender = gender, salary = salary, reduction = reduction, before = before, after = after, parking = parking)
# clean up
remove(list = names(RcmdrTestDrive))
remove(x, n, m, s, tmp)


###################################################
### chunk number 4: 
###################################################
plot.htest <- function (x, hypoth.or.conf = 'Hypoth',...) { 
require(HH) 
if (x$method == "1-sample proportions test with continuity correction" || x$method == "1-sample proportions test without continuity correction"){
mu <- x$null.value
obs.mean <- x$estimate
n <- NA
std.dev <- abs(obs.mean - mu)/sqrt(x$statistic)
deg.freedom <- NA
if(x$alternative == "two.sided"){
alpha.right <- (1 - attr(x$conf.int, "conf.level"))/2
Use.alpha.left <- TRUE
Use.alpha.right <- TRUE
} else if (x$alternative == "less") {
alpha.right <- 1 - attr(x$conf.int, "conf.level")
Use.alpha.left <- TRUE
Use.alpha.right <- FALSE
} else {
alpha.right <- 1 - attr(x$conf.int, "conf.level")
Use.alpha.left <- FALSE
Use.alpha.right <- TRUE
}
} else if (x$method == "One Sample z-test"){
mu <- x$null.value
obs.mean <- x$estimate
n <- x$parameter[1]
std.dev <- x$parameter[2]
deg.freedom <- NA
if(x$alternative == "two.sided"){
alpha.right <- (1 - attr(x$conf.int, "conf.level"))/2
Use.alpha.left <- TRUE
Use.alpha.right <- TRUE
} else if (x$alternative == "less") {
alpha.right <- 1 - attr(x$conf.int, "conf.level")
Use.alpha.left <- TRUE
Use.alpha.right <- FALSE
} else {
alpha.right <- 1 - attr(x$conf.int, "conf.level")
Use.alpha.left <- FALSE
Use.alpha.right <- TRUE
} 
} else if (x$method == "One Sample t-test" || x$method == "Paired t-test"){
mu <- x$null.value
obs.mean <- x$estimate
n <- x$parameter + 1
std.dev <- x$estimate/x$statistic*sqrt(n)
deg.freedom <- x$parameter
if(x$alternative == "two.sided"){
alpha.right <- (1 - attr(x$conf.int, "conf.level"))/2
Use.alpha.left <- TRUE
Use.alpha.right <- TRUE
} else if (x$alternative == "less") {
alpha.right <- 1 - attr(x$conf.int, "conf.level")
Use.alpha.left <- TRUE
Use.alpha.right <- FALSE
} else {
alpha.right <- 1 - attr(x$conf.int, "conf.level")
Use.alpha.left <- FALSE
Use.alpha.right <- TRUE
}
} else if (x$method == "Welch Two Sample t-test"){
mu <- x$null.value
obs.mean <- -diff(x$estimate)
n <- x$parameter + 2
std.dev <- obs.mean/x$statistic*sqrt(n)
deg.freedom <- x$parameter
if(x$alternative == "two.sided"){
alpha.right <- (1 - attr(x$conf.int, "conf.level"))/2
Use.alpha.left <- TRUE
Use.alpha.right <- TRUE
} else if (x$alternative == "less") {
alpha.right <- 1 - attr(x$conf.int, "conf.level")
Use.alpha.left <- TRUE
Use.alpha.right <- FALSE
} else {
alpha.right <- 1 - attr(x$conf.int, "conf.level")
Use.alpha.left <- FALSE
Use.alpha.right <- TRUE
} 
} else if (x$method == " Two Sample t-test"){
mu <- x$null.value
obs.mean <- -diff(x$estimate)
n <- x$parameter + 2
std.dev <- obs.mean/x$statistic*sqrt(n)
deg.freedom <- x$parameter
if(x$alternative == "two.sided"){
alpha.right <- (1 - attr(x$conf.int, "conf.level"))/2
Use.alpha.left <- TRUE
Use.alpha.right <- TRUE
} else if (x$alternative == "less") {
alpha.right <- 1 - attr(x$conf.int, "conf.level")
Use.alpha.left <- TRUE
Use.alpha.right <- FALSE
} else {
alpha.right <- 1 - attr(x$conf.int, "conf.level")
Use.alpha.left <- FALSE
Use.alpha.right <- TRUE
}
}
return(normal.and.t.dist(mu.H0 = mu, obs.mean = obs.mean, std.dev = std.dev, n = n, deg.freedom = deg.freedom, alpha.right = alpha.right, Use.obs.mean = TRUE, Use.alpha.left = Use.alpha.left, Use.alpha.right = Use.alpha.right, hypoth.or.conf = hypoth.or.conf))
}


###################################################
### chunk number 5:  eval=FALSE
###################################################
## install.packages(IPSUR)
## library(IPSUR)
## read(IPSUR)


###################################################
### chunk number 6: 
###################################################
getOption("defaultPackages")


###################################################
### chunk number 7: two
###################################################
2 + 3       # add
4 * 5 / 6   # multiply and divide
7^8         # 7 to the 8th power


###################################################
### chunk number 8: 
###################################################
options(digits = 16)
10/3                 # see more digits
sqrt(2)              # square root
exp(1)               # Euler's constant, e
pi       
options(digits = 7)  # back to default


###################################################
### chunk number 9: 
###################################################
x <- 7*41/pi   # don't see the calculated value
x              # take a look


###################################################
### chunk number 10: five
###################################################
sqrt(-1)            # isn't defined
sqrt(-1+0i)         # is defined
(0 + 1i)^2          # should be -1
typeof((0 + 1i)^2)


###################################################
### chunk number 11: 
###################################################
x <- c(74, 31, 95, 61, 76, 34, 23, 54, 96)
x


###################################################
### chunk number 12: 
###################################################
x <- 1:5
sum(x)
length(x)
min(x)
mean(x)      # sample mean
sd(x)        # sample standard deviation


###################################################
### chunk number 13: 
###################################################
intersect


###################################################
### chunk number 14: 
###################################################
rev


###################################################
### chunk number 15: 
###################################################
methods(rev)


###################################################
### chunk number 16: 
###################################################
rev.default


###################################################
### chunk number 17: 
###################################################
wilcox.test
methods(wilcox.test)


###################################################
### chunk number 18: 
###################################################
exp


###################################################
### chunk number 19: 
###################################################
plot


###################################################
### chunk number 20: 
###################################################
x <- rnbinom(6, size = 4, prob = 0.25)
k <- sample(1:9, size = 3, replace = FALSE)


###################################################
### chunk number 21: fifteen
###################################################
x


###################################################
### chunk number 22: 
###################################################
x^k[1]
x - k[2]
log(x + k[3])


###################################################
### chunk number 23: 
###################################################
x <- round(rnorm(13, mean = 20, sd = 2), 1)


###################################################
### chunk number 24: 
###################################################
x


###################################################
### chunk number 25: 
###################################################
c(min(x), max(x))
mean(x)
c(max(x), min(x)) - mean(x)


###################################################
### chunk number 26: twenty
###################################################
x <- round(rnorm(12, mean = 3, sd = 0.3), 3) * 1000
names(x) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")


###################################################
### chunk number 27: 
###################################################
x


###################################################
### chunk number 28: 
###################################################
names(x) <- c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")
x


###################################################
### chunk number 29: 
###################################################
cumsum(x)


###################################################
### chunk number 30: 
###################################################
diff(x)


###################################################
### chunk number 31: twentyfive
###################################################
commute = sample(150:250, size = 10, replace = TRUE)/10
k = sample(1:10, size = 1)
new = sample(150:250, size = 1, replace = TRUE)/10



###################################################
### chunk number 32: 
###################################################
commute


###################################################
### chunk number 33: 
###################################################
c(max(commute), min(commute), mean(commute), sd(commute))
commute[k] <- new
c(max(commute), min(commute), mean(commute), sd(commute))


###################################################
### chunk number 34: 
###################################################
par(mfrow = c(1,3)) # 3 plots: 1 row, 3 columns
stripchart(uspop, xlab="length", ylim=c(0, 2))
stripchart(rivers, method="jitter", xlab="length")
stripchart(discoveries, method="stack", xlab="number of discoveries")
par(mfrow = c(1,1)) # back to normal


###################################################
### chunk number 35: 
###################################################
par(mfrow = c(1,2)) # 2 plots: 1 row, 2 columns
hist(volcano, freq = TRUE)
hist(volcano, freq = FALSE)
par(mfrow = c(1,1)) # back to normal


###################################################
### chunk number 36: 
###################################################
library(aplpack)
stem.leaf(UKDriverDeaths, depth = FALSE)


###################################################
### chunk number 37: 
###################################################
par(mfrow = c(2,1)) # 2 plots: 1 row, 2 columns
plot(LakeHuron, type = "p")
plot(LakeHuron, type = "h")
par(mfrow = c(1,1)) # back to normal


###################################################
### chunk number 38: 
###################################################
Tbl <- table(state.division)
Tbl            # frequencies
Tbl/sum(Tbl)   # relative frequencies


###################################################
### chunk number 39: 
###################################################
par(mfrow = c(1,2)) # 2 plots: 1 row, 2 columns
barplot(table(state.region), cex.names=0.60)
barplot(prop.table(table(state.region)), cex.names=0.60)
par(mfrow = c(1,1)) # back to normal


###################################################
### chunk number 40: 
###################################################
library(qcc)
pareto.chart(table(state.division), ylab="Frequency")


###################################################
### chunk number 41: 
###################################################
x <- c(5,7)
v <- (x<6)
v


###################################################
### chunk number 42: 
###################################################
x <- c(109, 84, 73, 42, 61, 51,54, 71, 47, 70, 65, 57,69, 82, 76, 60, 38, 81,76, 85, 58, 73, 65, 42)
stem.leaf(x)


###################################################
### chunk number 43: 
###################################################
stem.leaf(rivers)


###################################################
### chunk number 44: 
###################################################
stem.leaf(precip)


###################################################
### chunk number 45: 
###################################################
x = 5:8
y = 3:6
data.frame(x,y)


###################################################
### chunk number 46: 
###################################################
matplot(rnorm(100), rnorm(100), type="b", lty=1, pch=1)


###################################################
### chunk number 47: 
###################################################
library(lattice)
print(bwplot(~ weight | feed, data = chickwts))


###################################################
### chunk number 48: 
###################################################
library(lattice)
print(histogram(~age | education, data = infert))


###################################################
### chunk number 49: 
###################################################
library(lattice)
print(xyplot(Petal.Length ~ Petal.Width | Species, data = iris))


###################################################
### chunk number 50: 
###################################################
library(lattice)
print(coplot(conc ~ uptake | Type * Treatment, data = CO2))


###################################################
### chunk number 51: 
###################################################
attach(RcmdrTestDrive)
names(RcmdrTestDrive)


###################################################
### chunk number 52: "Find summary statistics"
###################################################
summary(RcmdrTestDrive)


###################################################
### chunk number 53: 
###################################################
table(race)


###################################################
### chunk number 54: 
###################################################
barplot(table(RcmdrTestDrive$race), main="", xlab="race", ylab="Frequency", legend.text=FALSE, col=NULL) 


###################################################
### chunk number 55: 
###################################################
x = tapply(RcmdrTestDrive$salary, list(gender=RcmdrTestDrive$gender), mean, na.rm=TRUE)
x


###################################################
### chunk number 56: 
###################################################
by(salary, gender, mean, na.rm=TRUE) # another way to do it


###################################################
### chunk number 57: 
###################################################
x[which(x==max(x))]


###################################################
### chunk number 58: 
###################################################
y = tapply(RcmdrTestDrive$salary, list(gender=RcmdrTestDrive$gender), sd, na.rm=TRUE)
y


###################################################
### chunk number 59: 
###################################################
boxplot(salary~gender, xlab="salary", ylab="gender", main="", notch=FALSE, varwidth=TRUE, horizontal=TRUE, data=RcmdrTestDrive) 


###################################################
### chunk number 60: 
###################################################
x = sort(reduction)


###################################################
### chunk number 61: 
###################################################
x[137]
IQR(x)
fivenum(x)
fivenum(x)[4] - fivenum(x)[2]


###################################################
### chunk number 62: 
###################################################
boxplot(reduction, xlab="reduction", main="", notch=FALSE, varwidth=TRUE, horizontal=TRUE, data=RcmdrTestDrive) 


###################################################
### chunk number 63: 
###################################################
in.fence = 1.5 * (fivenum(x)[4] - fivenum(x)[2]) + fivenum(x)[4]
out.fence = 3 * (fivenum(x)[4] - fivenum(x)[2]) + fivenum(x)[4]
which(x > in.fence)
which(x > out.fence)


###################################################
### chunk number 64: 
###################################################
c(mean(before), median(before))
c(mean(after), median(after))


###################################################
### chunk number 65: 
###################################################
boxplot(before, xlab="before", main="", notch=FALSE, varwidth=TRUE, horizontal=TRUE, data=RcmdrTestDrive) 


###################################################
### chunk number 66: 
###################################################
boxplot(after, xlab="after", notch=FALSE, varwidth=TRUE, horizontal=TRUE, data=RcmdrTestDrive) 


###################################################
### chunk number 67: 
###################################################
sd(before)
mad(after)
IQR(after)/1.349


###################################################
### chunk number 68: 
###################################################
library(e1071)
skewness(before)
kurtosis(before)


###################################################
### chunk number 69: 
###################################################
skewness(after)
kurtosis(after)


###################################################
### chunk number 70: 
###################################################
hist(before, xlab="before", data=RcmdrTestDrive) 


###################################################
### chunk number 71: 
###################################################
hist(after, xlab="after", data=RcmdrTestDrive) 


###################################################
### chunk number 72: 
###################################################
g <- Vectorize(pbirthday.ipsur)
plot( 1:50, g(1:50), xlab = "Number of people in room", ylab = "Prob(at least one match)")
abline(h = 0.5)
abline(v = 23, lty = 2)
remove(g)


###################################################
### chunk number 73: 
###################################################
library(prob)
S <- rolldie(2, makespace = TRUE)  # assumes equally likely model
head(S)                           #  first few rows


###################################################
### chunk number 74: 
###################################################
A <- subset(S, X1 == X2)
B <- subset(S, X1 + X2 >= 8)


###################################################
### chunk number 75: 
###################################################
prob(A, given = B)
prob(B, given = A)


###################################################
### chunk number 76: 
###################################################
prob(S, X1==X2, given = (X1 + X2 >= 8) )
prob(S, X1+X2 >= 8, given = (X1==X2) )


###################################################
### chunk number 77: 
###################################################
library(prob)
L <- cards()
M <- urnsamples(L, size = 2)
N <- probspace(M)


###################################################
### chunk number 78: 
###################################################
prob(N, all(rank == "A"))


###################################################
### chunk number 79: 
###################################################
library(prob)
L <- rep(c("red","green"), times = c(7,3))
M <- urnsamples(L, size = 3, replace = FALSE, ordered = TRUE)
N <- probspace(M)


###################################################
### chunk number 80: 
###################################################
.Table <- xtabs(~smoke+gender, data=RcmdrTestDrive)
addmargins(.Table) # Table with Marginal Distributions
remove(.Table)


###################################################
### chunk number 81: 
###################################################
rnorm(1)


###################################################
### chunk number 82: 
###################################################
rnorm(1)


###################################################
### chunk number 83: 
###################################################
rnorm(1)


###################################################
### chunk number 84: 
###################################################
rnorm(1)


###################################################
### chunk number 85: 
###################################################
rnorm(1)


###################################################
### chunk number 86: 
###################################################
rnorm(1)


###################################################
### chunk number 87: 
###################################################
rnorm(1)


###################################################
### chunk number 88: 
###################################################
rnorm(1)


###################################################
### chunk number 89: 
###################################################
rnorm(1)


###################################################
### chunk number 90: 
###################################################
rnorm(1)


###################################################
### chunk number 91: 
###################################################
x <- c(0,1,2,3)
f <- c(1/8, 3/8, 3/8, 1/8)


###################################################
### chunk number 92: 
###################################################
mu <- sum(x * f)
mu


###################################################
### chunk number 93: 
###################################################
sigma2 <- sum((x-mu)^2 * f)
sigma2
sigma <- sqrt(sigma2)
sigma


###################################################
### chunk number 94: 
###################################################
F = cumsum(f)
F


###################################################
### chunk number 95: 
###################################################
library(distrEx)     # note: distrEx depends on distr
X <- DiscreteDistribution(supp = 0:3, prob = c(1,3,3,1)/8)
E(X); var(X); sd(X)


###################################################
### chunk number 96: 
###################################################
A <- data.frame(Pr=dbinom(0:4, size = 4, prob = 0.5))
rownames(A) <- 0:4 
A


###################################################
### chunk number 97: 
###################################################
pbinom(9, size = 12, prob = 1/6) - pbinom(6, size = 12, prob = 1/6)
diff(pbinom(c(6,9), size = 12, prob = 1/6))  # same thing


###################################################
### chunk number 98: 
###################################################
plot(0, xlim = c(-1.2, 4.2), ylim = c(-0.04, 1.04), type = "n", xlab = "number of successes", ylab = "cumulative probability")
abline(h = c(0,1), lty = 2, col = "grey")
lines(stepfun(0:3, pbinom(-1:3, size = 3, prob = 0.5)), verticals = FALSE, do.p = FALSE)
points(0:3, pbinom(0:3, size = 3, prob = 0.5), pch = 16, cex = 1.2)
points(0:3, pbinom(-1:2, size = 3, prob = 0.5), pch = 1, cex = 1.2)


###################################################
### chunk number 99: 
###################################################
library(distr)
X <- Binom(size = 3, prob = 1/2)
X


###################################################
### chunk number 100: 
###################################################
d(X)(1)   # pmf of X evaluated at x = 1
p(X)(2)   # cdf of X evaluated at x = 2


###################################################
### chunk number 101: 
###################################################
plot(X)


###################################################
### chunk number 102: 
###################################################
library(distrEx)
X = Binom(size = 3, prob = 0.45)
E(X)
E(3*X + 4)


###################################################
### chunk number 103: 
###################################################
var(X)
sd(X)


###################################################
### chunk number 104: 
###################################################
x <- c(4, 7, 9, 11, 12)
ecdf(x)


###################################################
### chunk number 105:  eval=FALSE
###################################################
## plot(ecdf(x))


###################################################
### chunk number 106: 
###################################################
plot(ecdf(x))


###################################################
### chunk number 107: 
###################################################
epdf <- function(x) function(t){sum(x %in% t)/length(x)}
x <- c(0,0,1)
epdf(x)(0)       # should be 2/3


###################################################
### chunk number 108: 
###################################################
x <- c(0,0,1)
sample(x, size = 7, replace = TRUE)       # should be 2/3


###################################################
### chunk number 109: 
###################################################
dhyper(3, m = 17, n = 233, k = 5)


###################################################
### chunk number 110: 
###################################################
A <- data.frame(Pr=dhyper(0:4, m = 17, n = 233, k = 5))
rownames(A) <- 0:4 
A


###################################################
### chunk number 111: 
###################################################
dhyper(5, m = 17, n = 233, k = 5)


###################################################
### chunk number 112: 
###################################################
phyper(2, m = 17, n = 233, k = 5)


###################################################
### chunk number 113: 
###################################################
phyper(1, m = 17, n = 233, k = 5, lower.tail = FALSE)


###################################################
### chunk number 114: 
###################################################
rhyper(10, m = 17, n = 233, k = 5)


###################################################
### chunk number 115: 
###################################################
pgeom(4, prob = 0.812, lower.tail = FALSE)


###################################################
### chunk number 116: 
###################################################
dnbinom(5, size = 7, prob = 0.5)


###################################################
### chunk number 117: 
###################################################
diff(ppois(c(47, 50), lambda = 50))


###################################################
### chunk number 118: 
###################################################
xmin <- qbinom(.0005, size=31 , prob=0.447) 
xmax <- qbinom(.9995, size=31 , prob=0.447) 
.x <- xmin:xmax 
plot(.x, dbinom(.x, size=31, prob=0.447), xlab="Number of Successes", ylab="Probability Mass",    main="Binomial Dist'n: Trials = 31, Prob of success = 0.447", type="h") 
points(.x, dbinom(.x, size=31, prob=0.447), pch=16) 
abline( h = 0, lty = 2, col = "grey" ) 
remove(.x, xmin, xmax)


###################################################
### chunk number 119: 
###################################################
xmin <- qbinom(.0005, size=31 , prob=0.447) 
xmax <- qbinom(.9995, size=31 , prob=0.447) 
.x <- xmin:xmax 
plot( stepfun(.x, pbinom((xmin-1):xmax, size=31, prob=0.447)), verticals=FALSE, do.p=FALSE, xlab="Number of Successes", ylab="Cumulative Probability", main="Binomial Dist'n: Trials = 31, Prob of success = 0.447") 
points( .x, pbinom(xmin:xmax, size=31, prob=0.447), pch = 16, cex=1.2 ) 
points( .x, pbinom((xmin-1):(xmax-1), size=31, prob=0.447), pch = 1,    cex=1.2 ) 
abline( h = 1, lty = 2, col = "grey" ) 
abline( h = 0, lty = 2, col = "grey" ) 
remove(.x, xmin, xmax) 


###################################################
### chunk number 120: 
###################################################
dbinom(17, size = 31, prob = 0.447)


###################################################
### chunk number 121: 
###################################################
pbinom(13, size = 31, prob = 0.447)


###################################################
### chunk number 122: 
###################################################
pbinom(11, size = 31, prob = 0.447, lower.tail = FALSE)


###################################################
### chunk number 123: 
###################################################
pbinom(14, size = 31, prob = 0.447, lower.tail = FALSE)


###################################################
### chunk number 124: 
###################################################
sum(dbinom(16:19, size = 31, prob = 0.447))
diff(pbinom(c(19,15), size = 31, prob = 0.447, lower.tail = FALSE))


###################################################
### chunk number 125: 
###################################################
library(distrEx)
X = Binom(size = 31, prob = 0.447)
E(X)


###################################################
### chunk number 126: 
###################################################
var(X)


###################################################
### chunk number 127: 
###################################################
sd(X)


###################################################
### chunk number 128: 
###################################################
E(4*X + 51.324)


###################################################
### chunk number 129: 
###################################################
rnorm(1)


###################################################
### chunk number 130: 
###################################################
rnorm(1)


###################################################
### chunk number 131: 
###################################################
rnorm(1)


###################################################
### chunk number 132: 
###################################################
rnorm(1)


###################################################
### chunk number 133: 
###################################################
rnorm(1)


###################################################
### chunk number 134: 
###################################################
rnorm(1)


###################################################
### chunk number 135: 
###################################################
rnorm(1)


###################################################
### chunk number 136: 
###################################################
rnorm(1)


###################################################
### chunk number 137: 
###################################################
rnorm(1)


###################################################
### chunk number 138: 
###################################################
rnorm(1)


###################################################
### chunk number 139: 
###################################################
pnorm(1:3)-pnorm(-(1:3))


###################################################
### chunk number 140: 
###################################################
library(distr)
X <- Norm(mean = 0, sd = 1)
Y <- 4 - 3*X
Y


###################################################
### chunk number 141: 
###################################################
Z <- exp(X)
Z


###################################################
### chunk number 142: 
###################################################
W <- sin(exp(X) + 27)
W


###################################################
### chunk number 143: 
###################################################
p(W)(0.5)
W <- sin(exp(X) + 27)
p(W)(0.5)


###################################################
### chunk number 144: 
###################################################
qt(0.01, df = 23, lower.tail = FALSE)


###################################################
### chunk number 145: 
###################################################
library(actuar)
mgamma(1:4, shape = 13, rate = 1)


###################################################
### chunk number 146: 
###################################################
plot(function(x){mgfgamma(x, shape = 13, rate = 1)}, from=-0.1, to=0.1, ylab = "gamma mgf")


###################################################
### chunk number 147: 
###################################################
plot(function(x){mgfgamma(x, shape = 13, rate = 1)}, from=-0.1, to=0.1, ylab = "gamma mgf")


###################################################
### chunk number 148: 
###################################################
rnorm(1)


###################################################
### chunk number 149: 
###################################################
rnorm(1)


###################################################
### chunk number 150: 
###################################################
pnorm(2.64, lower.tail = FALSE)


###################################################
### chunk number 151: 
###################################################
pnorm(0.87) - 1/2


###################################################
### chunk number 152: 
###################################################
2 * pnorm(-1.39)


###################################################
### chunk number 153: 
###################################################
rnorm(1)


###################################################
### chunk number 154: 
###################################################
rnorm(1)


###################################################
### chunk number 155: 
###################################################
rnorm(1)


###################################################
### chunk number 156: 
###################################################
rnorm(1)


###################################################
### chunk number 157: 
###################################################
rnorm(1)


###################################################
### chunk number 158: 
###################################################
rnorm(1)


###################################################
### chunk number 159: 
###################################################
rnorm(1)


###################################################
### chunk number 160: 
###################################################
S <- rolldie(2, makespace = TRUE)
S <- addrv(S, FUN = max, invars = c("X1","X2"), name = "U")
S <- addrv(S, FUN = sum, invars = c("X1","X2"), name = "V")
head(S)


###################################################
### chunk number 161: 
###################################################
UV <- marginal(S, vars = c("U", "V"))
head(UV)
xtabs(round(probs,3) ~ V + U, data = UV)


###################################################
### chunk number 162: 
###################################################
marginal(UV, vars = "U")
head(marginal(UV, vars = "V"))


###################################################
### chunk number 163: 
###################################################
Eu <- sum(S$U*S$probs)
Ev <- sum(S$V*S$probs)
sum(S$U*S$V*S$probs)
sum(S$U*S$V*S$probs)-Eu*Ev


###################################################
### chunk number 164: 
###################################################
library(mvtnorm)
x <- y <- seq(from = -3, to = 3, length.out = 30)
f <- function(x,y) dmvnorm(cbind(x,y), mean = c(0,0), sigma = diag(2))
z <- outer(x, y, FUN = f)
persp(x, y, z, theta = -30, phi = 30, ticktype = "detailed")


###################################################
### chunk number 165: 
###################################################
library(combinat)
tmp <- t(xsimplex(3, 6))
p <- apply(tmp, MARGIN = 1, FUN = dmultinom, prob = c(36,27,37))
library(prob)
S <- probspace(tmp, probs = p)
ProbTable <- xtabs(probs ~ X1 + X2, data = S)
round(ProbTable, 3)


###################################################
### chunk number 166: 
###################################################
library(lattice)
print(cloud(probs ~ X1 + X2, data = S, type = c("p","h"), lwd = 2, pch = 16, cex = 1.5), screen = list(z = 15, x = -70))


###################################################
### chunk number 167: 
###################################################
rnorm(1)


###################################################
### chunk number 168: 
###################################################
rnorm(1)


###################################################
### chunk number 169: 
###################################################
rnorm(1)


###################################################
### chunk number 170: 
###################################################
rnorm(1)


###################################################
### chunk number 171: 
###################################################
rnorm(1)


###################################################
### chunk number 172: 
###################################################
rnorm(1)


###################################################
### chunk number 173: 
###################################################
rnorm(1)


###################################################
### chunk number 174: 
###################################################
rnorm(1)


###################################################
### chunk number 175: 
###################################################
rnorm(1)


###################################################
### chunk number 176: 
###################################################
rnorm(1)


###################################################
### chunk number 177: 
###################################################
rnorm(1)


###################################################
### chunk number 178: 
###################################################
iqrs <- replicate(100, IQR(rnorm(100)))


###################################################
### chunk number 179: 
###################################################
mean(iqrs)


###################################################
### chunk number 180: 
###################################################
sd(iqrs)


###################################################
### chunk number 181: 
###################################################
hist(iqrs)


###################################################
### chunk number 182: 
###################################################
mads <- replicate(100, mad(rnorm(100)))


###################################################
### chunk number 183: 
###################################################
mean(mads)


###################################################
### chunk number 184: 
###################################################
sd(mads)


###################################################
### chunk number 185: 
###################################################
hist(mads)


###################################################
### chunk number 186: 
###################################################
k = 1
n = sample(10:30, size=10, replace = TRUE)
mu = round(rnorm(10, mean = 20))


###################################################
### chunk number 187: 
###################################################
rnorm(1)


###################################################
### chunk number 188: 
###################################################
rnorm(1)


###################################################
### chunk number 189: 
###################################################
rnorm(1)


###################################################
### chunk number 190: 
###################################################
rnorm(1)


###################################################
### chunk number 191: 
###################################################
rnorm(1)


###################################################
### chunk number 192: 
###################################################
rnorm(1)


###################################################
### chunk number 193: 
###################################################
pnorm(43.1, mean = 37, sd = 9, lower.tail = FALSE)


###################################################
### chunk number 194: 
###################################################
rnorm(1)


###################################################
### chunk number 195: 
###################################################
rnorm(1)


###################################################
### chunk number 196: 
###################################################
rnorm(1)


###################################################
### chunk number 197: 
###################################################
heights = rep(0, 16)
for (j in 7:15) heights[j] <- dhyper(3, m = 7, n = j - 7, k = 4)
plot(6:15, heights[6:15], pch = 16, cex = 1.5, xlab = "number of fish in pond", ylab = "Likelihood")
abline(h = 0)
lines(6:15, heights[6:15], type = "h", lwd = 2, lty = 3)
text(9, heights[9]/6, bquote(hat(F)==.(9)), cex = 2, pos = 4)
lines(9, heights[9], type = "h", lwd = 2)
points(9, 0, pch = 4, lwd = 3, cex = 2) 


###################################################
### chunk number 198: 
###################################################
dat <- rbinom(27, size = 1, prob = 0.3)
like <- function(x){
r <- 1
for (k in 1:27){ r <- r*dbinom(dat[k], size = 1, prob = x)}
return(r)
}
curve(like, from = 0, to = 1, xlab = "parameter space", ylab = "Likelihood", lwd = 3, col = "blue")
abline(h = 0, lwd = 1, lty = 3, col = "grey")
mle <- mean(dat)
mleobj <- like(mle)
lines(mle, mleobj, type = "h", lwd = 2, lty = 3, col = "red")
points(mle, 0, pch = 4, lwd = 2, cex = 2, col = "red")
text(mle, mleobj/6, substitute(hat(theta)==a, list(a=round(mle, 4))), cex = 2, pos = 4)


###################################################
### chunk number 199: 
###################################################
x <- mtcars$am
L <- function(p,x) prod(dbinom(x, size = 1, prob = p))
optimize(L, interval = c(0,1), x = x, maximum = TRUE)


###################################################
### chunk number 200: 
###################################################
A <- optimize(L, interval = c(0,1), x = x, maximum = TRUE)


###################################################
### chunk number 201: 
###################################################
minuslogL <- function(p,x) -sum(dbinom(x, size = 1, prob = p, log = TRUE))
optimize(minuslogL, interval = c(0,1), x = x)


###################################################
### chunk number 202: 
###################################################
minuslogL <- function(mu, sigma2){
  -sum(dnorm(x, mean = mu, sd = sqrt(sigma2), log = TRUE))
}


###################################################
### chunk number 203: 
###################################################
x <- PlantGrowth$weight
library(stats4)
MaxLikeEst <- mle(minuslogL, start = list(mu = 5, sigma2 = 0.5))
summary(MaxLikeEst)


###################################################
### chunk number 204: 
###################################################
mean(x)
var(x)*29/30
sd(x)/sqrt(30)


###################################################
### chunk number 205: 
###################################################
library(TeachingDemos)
ci.examp()


###################################################
### chunk number 206: 
###################################################
library(Hmisc)
binconf(x = 7, n = 25, method = "asymptotic")
binconf(x = 7, n = 25, method = "wilson")


###################################################
### chunk number 207: 
###################################################
tab <- xtabs(~gender, data = RcmdrTestDrive)
prop.test(rbind(tab), conf.level = 0.95, correct = FALSE)


###################################################
### chunk number 208: 
###################################################
A <- as.data.frame(Titanic)
library(reshape)
B <- with(A, untable(A, Freq))


###################################################
### chunk number 209: 
###################################################
# this is the example from the help file
nheads <- rbinom(1, size = 100, prob = 0.45)
prop.test(x = nheads, n = 100, p = 0.50, alternative = "two.sided", conf.level = 0.95, correct = TRUE)
prop.test(x = nheads, n = 100, p = 0.50, alternative = "two.sided", conf.level = 0.95, correct = FALSE)



###################################################
### chunk number 210: 
###################################################
library(HH)
plot(prop.test(x = nheads, n = 100, p = 0.50, alternative = "two.sided", conf.level = 0.95, correct = FALSE), 'Hypoth')


###################################################
### chunk number 211: 
###################################################
x <- rnorm(37, mean = 2, sd = 3)
library(TeachingDemos)
z.test(x, mu = 1, sd = 3, conf.level = 0.90)


###################################################
### chunk number 212: 
###################################################
x <- rnorm(13, mean = 2, sd = 3)
t.test(x, mu = 0, conf.level = 0.90, alternative = "greater")


###################################################
### chunk number 213: 
###################################################
y1 <- rnorm(300, mean = c(2,8,22))
plot(y1, xlim = c(-1,25), ylim = c(0,0.45) , type = "n")
f <- function(x){dnorm(x, mean = 2)}
curve(f, from = -1, to = 5, add = TRUE, lwd = 2)
f <- function(x){dnorm(x, mean = 8)}
curve(f, from = 5, to = 11, add = TRUE, lwd = 2)
f <- function(x){dnorm(x, mean = 22)}
curve(f, from = 19, to = 25, add = TRUE, lwd = 2)
rug(y1)


###################################################
### chunk number 214: 
###################################################
y2 <- rnorm(300, mean = c(4,4.1,4.3))
hist(y2, 30, prob = TRUE)
f <- function(x){dnorm(x, mean = 4)/3}
curve(f, add = TRUE, lwd = 2)
f <- function(x){dnorm(x, mean = 4.1)/3}
curve(f, add = TRUE, lwd = 2)
f <- function(x){dnorm(x, mean = 4.3)/3}
curve(f, add = TRUE, lwd = 2)


###################################################
### chunk number 215: 
###################################################
library(HH)
old.omd <- par(omd = c(.05,.88, .05,1))
F.setup(df1 = 5, df2 = 30)
F.curve(df1 = 5, df2 = 30, col='blue')
F.observed(3, df1 = 5, df2 = 30)
par(old.omd)


###################################################
### chunk number 216: 
###################################################
library(HH)
old.omd <- par(omd = c(.05,.88, .05,1))
F.setup(df1 = 5, df2 = 30)
F.curve(df1 = 5, df2 = 30, col = 'blue', alpha = c(.05, .05))
par(old.omd)


###################################################
### chunk number 217: 
###################################################
 # open window
plot(c(0,5), c(0,6.5), type = "n", xlab="x", ylab="y")
## the x- and y-axes
abline(h=0, v=0, col = "gray60")
# regression line
abline(a = 2.5, b = 0.5, lwd = 2)
# normal curves
x <- 600:3000/600
y <- dnorm(x, mean = 3, sd = 0.5)
lines(y + 1.0, x)
lines(y + 2.5, x + 0.75)
lines(y + 4.0, x + 1.5)
# pretty it up
abline(v = c(1, 2.5, 4), lty = 2, col = "grey")
segments(1,3, 1+dnorm(0,0,0.5),3, lty = 2, col = "gray")
segments(2.5, 3.75, 2.5+dnorm(0,0,0.5), 3.75, lty = 2, col = "gray")
segments(4,4.5, 4+dnorm(0,0,0.5),4.5, lty = 2, col = "gray")


###################################################
### chunk number 218: 
###################################################
data(cars)
head(cars)


###################################################
### chunk number 219: 
###################################################
plot(dist ~ speed, data = cars)


###################################################
### chunk number 220: 
###################################################
cars.lm <- lm(dist ~ speed, data = cars)


###################################################
### chunk number 221: 
###################################################
coef(cars.lm)


###################################################
### chunk number 222: 
###################################################
plot(dist ~ speed, data = cars)
abline(coef(cars.lm))


###################################################
### chunk number 223:  eval=FALSE
###################################################
## plot(dist ~ speed, data = cars)
## abline(coef(cars))


###################################################
### chunk number 224: 
###################################################
cars[5, ]


###################################################
### chunk number 225: 
###################################################
fitted(cars.lm)[1:5]


###################################################
### chunk number 226: 
###################################################
predict(cars.lm, newdata = data.frame(speed = c(6, 8, 21)))


###################################################
### chunk number 227: 
###################################################
residuals(cars.lm)[1:5]


###################################################
### chunk number 228: 
###################################################
carsumry <- summary(cars.lm)
carsumry$sigma


###################################################
### chunk number 229: 
###################################################
summary(cars.lm)


###################################################
### chunk number 230: 
###################################################
A <- round(summary(cars.lm)$coef, 3)
B <- round(confint(cars.lm), 3)


###################################################
### chunk number 231: 
###################################################
confint(cars.lm)


###################################################
### chunk number 232: 
###################################################
new <- data.frame(speed = c(5, 6, 21))


###################################################
### chunk number 233: 
###################################################
predict(cars.lm, newdata = new, interval = "confidence")


###################################################
### chunk number 234: 
###################################################
carsCI <- round(predict(cars.lm, newdata = new, interval = "confidence"), 2)


###################################################
### chunk number 235: 
###################################################
predict(cars.lm, newdata = new, interval = "prediction")


###################################################
### chunk number 236: 
###################################################
carsPI <- round(predict(cars.lm, newdata = new, interval = "prediction"), 2)


###################################################
### chunk number 237: 
###################################################
library(HH)
print(ci.plot(cars.lm))


###################################################
### chunk number 238:  eval=FALSE
###################################################
## library(HH)
## ci.plot(cars.lm)


###################################################
### chunk number 239: 
###################################################
summary(cars.lm)


###################################################
### chunk number 240: 
###################################################
A <- round(summary(cars.lm)$coef, 3)
B <- round(confint(cars.lm), 3)


###################################################
### chunk number 241: 
###################################################
anova(cars.lm)


###################################################
### chunk number 242: 
###################################################
carsumry$r.squared


###################################################
### chunk number 243: 
###################################################
sqrt(carsumry$r.squared)


###################################################
### chunk number 244: 
###################################################
anova(cars.lm)


###################################################
### chunk number 245: 
###################################################
plot(cars.lm, which = 2)


###################################################
### chunk number 246: 
###################################################
shapiro.test(residuals(cars.lm))


###################################################
### chunk number 247: 
###################################################
plot(cars.lm, which = 3)


###################################################
### chunk number 248: 
###################################################
library(lmtest)
bptest(cars.lm)


###################################################
### chunk number 249: 
###################################################
plot(cars.lm, which = 1)


###################################################
### chunk number 250: 
###################################################
library(lmtest)
dwtest(cars.lm, alternative = "two.sided")


###################################################
### chunk number 251: 
###################################################
sres <- rstandard(cars.lm)
sres[1:5]


###################################################
### chunk number 252: 
###################################################
sres[which(abs(sres) > 2)]


###################################################
### chunk number 253: 
###################################################
sdelres <- rstudent(cars.lm)
sdelres[1:5]


###################################################
### chunk number 254: 
###################################################
t0.005 <- qt(0.005, df = 47, lower.tail = FALSE)
sdelres[which(abs(sdelres) > t0.005)]


###################################################
### chunk number 255: 
###################################################
leverage <- hatvalues(cars.lm)
leverage[1:5]
leverage[which(leverage > 4/50)]


###################################################
### chunk number 256: 
###################################################
dfb <- dfbetas(cars.lm)
head(dfb)


###################################################
### chunk number 257: 
###################################################
dff <- dffits(cars.lm)
dff[1:5]


###################################################
### chunk number 258: 
###################################################
cooksD <- cooks.distance(cars.lm)
cooksD[1:5]


###################################################
### chunk number 259: 
###################################################
plot(cars.lm, which = 4)


###################################################
### chunk number 260: 
###################################################
F0.50 <- qf(0.5, df1 = 2, df2 = 48)
cooksD[which(cooksD > F0.50)]


###################################################
### chunk number 261:  eval=FALSE
###################################################
## influence.measures(cars.lm)


###################################################
### chunk number 262:  eval=FALSE
###################################################
## par(mfrow = c(2,2))
## plot(cars.lm)
## par(mfrow = c(1,1))


###################################################
### chunk number 263: 
###################################################
par(mfrow = c(2,2))
plot(cars.lm)
par(mfrow = c(1,1))


###################################################
### chunk number 264:  eval=FALSE
###################################################
## plot(cars.lm, which = 5)   # std'd resids vs lev plot
## identify(leverage, sres, n = 4)   # identify 4 points


###################################################
### chunk number 265: 
###################################################
data(trees)
head(trees)


###################################################
### chunk number 266: 
###################################################
library(lattice)
print(splom(trees))


###################################################
### chunk number 267:  eval=FALSE
###################################################
## library(lattice)
## splom(trees)


###################################################
### chunk number 268:  eval=FALSE
###################################################
## library(scatterplot3d)
## s3d <- with(trees, scatterplot3d(Girth, Height, Volume, pch = 16, highlight.3d = TRUE, angle = 60))
## fit <- lm(Volume ~ Girth + Height, data = trees)
## s3d$plane3d(fit)


###################################################
### chunk number 269: 
###################################################
library(scatterplot3d)
s3d <- with(trees, scatterplot3d(Girth, Height, Volume, pch = 16, highlight.3d = TRUE, angle = 60))
fit <- lm(Volume ~ Girth + Height, data = trees)
s3d$plane3d(fit)


###################################################
### chunk number 270: 
###################################################
trees.lm <- lm(Volume ~ Girth + Height, data = trees)
trees.lm


###################################################
### chunk number 271: 
###################################################
head(model.matrix(trees.lm))


###################################################
### chunk number 272: 
###################################################
fitted(trees.lm)[1:5]


###################################################
### chunk number 273: 
###################################################
new <- data.frame(Girth = c(9.1, 11.6, 12.5), Height = c(69, 74, 87))


###################################################
### chunk number 274: 
###################################################
new


###################################################
### chunk number 275: 
###################################################
predict(trees.lm, newdata = new)


###################################################
### chunk number 276: 
###################################################
treesFIT <- round(predict(trees.lm, newdata = new), 1)


###################################################
### chunk number 277: 
###################################################
residuals(trees.lm)[1:5]


###################################################
### chunk number 278: 
###################################################
treesumry <- summary(trees.lm)
treesumry$sigma


###################################################
### chunk number 279: 
###################################################
confint(trees.lm)


###################################################
### chunk number 280: 
###################################################
treesPAR <- round(confint(trees.lm), 1)


###################################################
### chunk number 281: 
###################################################
new <- data.frame(Girth = c(9.1, 11.6, 12.5), Height = c(69, 74, 87))


###################################################
### chunk number 282: 
###################################################
predict(trees.lm, newdata = new, interval = "confidence")


###################################################
### chunk number 283: 
###################################################
treesCI <- round(predict(trees.lm, newdata = new, interval = "confidence"), 1)


###################################################
### chunk number 284: 
###################################################
predict(trees.lm, newdata = new, interval = "prediction")


###################################################
### chunk number 285: 
###################################################
treesPI <- round(predict(trees.lm, newdata = new, interval = "prediction"), 1)


###################################################
### chunk number 286: 
###################################################
treesumry$r.squared
treesumry$adj.r.squared


###################################################
### chunk number 287: 
###################################################
treesumry$fstatistic


###################################################
### chunk number 288: 
###################################################
treesumry


###################################################
### chunk number 289: 
###################################################
plot(Volume ~ Girth, data = trees)


###################################################
### chunk number 290: 
###################################################
treesquad.lm <- lm(Volume ~ scale(Girth) + I(scale(Girth)^2), data = trees)
summary(treesquad.lm)


###################################################
### chunk number 291:  eval=FALSE
###################################################
## plot(Volume ~ scale(Girth), data = trees)
## lines(fitted(treesquad.lm) ~ scale(Girth), data = trees)


###################################################
### chunk number 292: 
###################################################
plot(Volume ~ scale(Girth), data = trees)
lines(fitted(treesquad.lm) ~ scale(Girth), data = trees)


###################################################
### chunk number 293: 
###################################################
new <- data.frame(Girth = c(9.1, 11.6, 12.5))
predict(treesquad.lm, newdata = new, interval = "prediction")


###################################################
### chunk number 294: 
###################################################
summary(lm(Volume ~ Girth + I(Girth^2), data = trees))


###################################################
### chunk number 295: 
###################################################
treesint.lm <- lm(Volume ~ Girth + Height + Girth:Height, data = trees)
summary(treesint.lm)


###################################################
### chunk number 296: 
###################################################
confint(treesint.lm)
new <- data.frame(Girth = c(9.1, 11.6, 12.5), Height = c(69, 74, 87))
predict(treesint.lm, newdata = new, interval = "prediction")


###################################################
### chunk number 297: 
###################################################
trees$Tall <- cut(trees$Height, breaks = c(-Inf, 76, Inf), labels = c("no","yes"))
trees$Tall[1:5]


###################################################
### chunk number 298: 
###################################################
class(trees$Tall)


###################################################
### chunk number 299: 
###################################################
treesdummy.lm <- lm(Volume ~ Girth + Tall, data = trees)
summary(treesdummy.lm)


###################################################
### chunk number 300:  eval=FALSE
###################################################
## treesTall <- split(trees, trees$Tall)
## treesTall[["yes"]]$Fit <- predict(treesdummy.lm, treesTall[["yes"]])
## treesTall[["no"]]$Fit <- predict(treesdummy.lm, treesTall[["no"]])
## plot(Volume ~ Girth, data = trees, type = "n")
## points(Volume ~ Girth, data = treesTall[["yes"]], pch = 1)
## points(Volume ~ Girth, data = treesTall[["no"]], pch = 2)
## lines(Fit ~ Girth, data = treesTall[["yes"]])
## lines(Fit ~ Girth, data = treesTall[["no"]])


###################################################
### chunk number 301: 
###################################################
treesTall <- split(trees, trees$Tall)
treesTall[["yes"]]$Fit <- predict(treesdummy.lm, treesTall[["yes"]])
treesTall[["no"]]$Fit <- predict(treesdummy.lm, treesTall[["no"]])
plot(Volume ~ Girth, data = trees, type = "n")
points(Volume ~ Girth, data = treesTall[["yes"]], pch = 1)
points(Volume ~ Girth, data = treesTall[["no"]], pch = 2)
lines(Fit ~ Girth, data = treesTall[["yes"]])
lines(Fit ~ Girth, data = treesTall[["no"]])


###################################################
### chunk number 302: 
###################################################
treesfull.lm <- lm(Volume ~ Girth + I(Girth^2) + Height + I(Height^2), data = trees)
summary(treesfull.lm)


###################################################
### chunk number 303: 
###################################################
treesreduced.lm <- lm(Volume ~ -1 + Girth + I(Girth^2), data = trees)


###################################################
### chunk number 304: 
###################################################
anova(treesreduced.lm, treesfull.lm)


###################################################
### chunk number 305: 
###################################################
treesreduced2.lm <- lm(Volume ~ Girth + I(Girth^2) + Height, data = trees)
anova(treesreduced2.lm, treesfull.lm)


###################################################
### chunk number 306: 
###################################################
treesNonlin.lm <- lm(log(Volume) ~ log(Girth) + log(Height), data = trees)
summary(treesNonlin.lm)


###################################################
### chunk number 307: 
###################################################
exp(confint(treesNonlin.lm))


###################################################
### chunk number 308: 
###################################################
new <- data.frame(Girth = c(9.1, 11.6, 12.5), Height = c(69, 74, 87))
exp(predict(treesNonlin.lm, newdata = new, interval = "confidence"))


###################################################
### chunk number 309: 
###################################################
srs <- rnorm(25, mean = 2)
resamps <- replicate(1000, sample(srs, 25, TRUE), simplify = FALSE)
xbarstar <- sapply(resamps, mean, simplify = TRUE)
mean(xbarstar)
sd(xbarstar)


###################################################
### chunk number 310: 
###################################################
hist(xbarstar, breaks = 40, prob = TRUE)
curve(dnorm(x, 2, 0.2), add = TRUE)


###################################################
### chunk number 311:  eval=FALSE
###################################################
## hist(xbarstar, breaks = 40, prob = TRUE)
## curve(dnorm(x, 2, 0.2), add = TRUE)  # overlay true normal density


###################################################
### chunk number 312: 
###################################################
data(rivers)
stem(rivers)


###################################################
### chunk number 313: 
###################################################
resamps <- replicate(1000, sample(rivers, 141, TRUE), simplify = FALSE)
medstar <- sapply(resamps, median, simplify = TRUE)
mean(medstar)
sd(medstar)


###################################################
### chunk number 314: 
###################################################
hist(medstar, breaks = 40, prob = TRUE)


###################################################
### chunk number 315:  eval=FALSE
###################################################
## hist(medstar, breaks = 40, prob = TRUE)


###################################################
### chunk number 316: 
###################################################
library(boot)
mean_fun <- function(x, indices) mean(x[indices])
boot(data = rnorm(25, mean = 2), statistic = mean_fun, R = 1000)


###################################################
### chunk number 317: 
###################################################
median_fun <- function(x, indices) median(x[indices])
boot(data = rivers, statistic = median_fun, R = 1000)


###################################################
### chunk number 318: 
###################################################
btsamps <- replicate(2000, sample(stack.loss, 21, TRUE), simplify = FALSE)
thetast <- sapply(btsamps, median, simplify = TRUE)
mean(thetast)
median(stack.loss)
quantile(thetast, c(0.025, 0.975))


###################################################
### chunk number 319: 
###################################################
library(boot)
med_fun <- function(x, ind) median(x[ind])
med_boot <- boot(stack.loss, med_fun, R = 2000)
boot.ci(med_boot, type = c("perc", "norm", "bca"))


###################################################
### chunk number 320: 
###################################################
library(coin)
oneway_test(len~supp, data = ToothGrowth)
oneway_test(breaks~wool, data = warpbreaks)
oneway_test(conc~state, data = Puromycin)
oneway_test(rate~state, data = Puromycin)


###################################################
### chunk number 321: 
###################################################
t.test(len ~ supp, data = ToothGrowth, alt = "greater", var.equal = TRUE)


###################################################
### chunk number 322: 
###################################################
A <- as.data.frame(Titanic)
head(A)


###################################################
### chunk number 323: 
###################################################
library(reshape)
B <- with(A, untable(A, Freq))
head(B)


###################################################
### chunk number 324: 
###################################################
tab <- matrix(1:6, nrow = 2, ncol = 3)
rownames(tab) <- c('first', 'second')
colnames(tab) <- c('A', 'B', 'C')
tab  # Counts


###################################################
### chunk number 325: 
###################################################
p <- c("milk","tea")
g <- c("milk","tea")
catgs <- expand.grid(poured = p, guessed = g)
cnts <- c(3, 1, 1, 3)
D <- cbind(catgs, count = cnts)
xtabs(count ~ poured + guessed, data = D)


###################################################
### chunk number 326:  eval=FALSE
###################################################
## library(odfWeave)
## odfWeave(file = "infile.odt", dest = "outfile.odt")


###################################################
### chunk number 327: 
###################################################
library(Hmisc)
summary(cbind(Sepal.Length, Sepal.Width) ~ Species, data = iris)


###################################################
### chunk number 328: 
###################################################
set.seed(095259)


###################################################
### chunk number 329: 
###################################################
options(digits = 16)
runif(1)


###################################################
### chunk number 330: 
###################################################
options(width = 80)
sessionInfo()


###################################################
### chunk number 331: 
###################################################
rm(.Random.seed)
save.image(file = "IPSUR.RData")


###################################################
### chunk number 332: 
###################################################
Stangle(file="IPSUR.Rnw", output="IPSUR.R", annotate=TRUE)


