library(dplyr)
library(tidyverse)
library(readxl)
library(RColorBrewer)

#전처리1 - 필요한 부분만 남기기
Crime <- read_excel("범죄사건처리기간2021.xlsx")
Crime <- Crime[c(4:9,11,12,14:17,20,21,29,30,34,36,39),]
colnames(Crime) <- c("Field", "crime", "Total", "D10", "D20", "M1", "M2", "M3", "M6", "Long")

#전처리2 - 숫자 정수화
Crime$Total <- as.integer(Crime$Total)
Crime$D10 <- as.integer(Crime$D10)
Crime$D20 <- as.integer(Crime$D20)
Crime$M1 <- as.integer(Crime$M1)
Crime$M2 <- as.integer(Crime$M2)
Crime$M3 <- as.integer(Crime$M3)
Crime$M6 <- as.integer(Crime$M6)
Crime$Long <- as.integer(Crime$Long)

#전처리3 - 단기, 장기로 구분
Crime <- Crime %>% mutate(Short_Term = D10+D20+M1, Long_Term = Total - Short_Term) %>% select(Field, crime, Total, Short_Term, Long_Term)

#전처리4 - Field, Crime 이름 수정
Crime$crime[2] <- c("살인미수")
Crime$Field[4:6] <- c("성범죄")
Crime$Field[7] <- c("방화범죄")
Crime$crime[8] <- c("절도")
Crime$Field[9:12] <- c("신체폭력")
Crime$Field[13:14] <- c("재산폭력")
Crime$crime[18] <- c("마약범죄")
Crime$crime[19] <- c("교통범죄")

Crime

#The posterior of single biomial distribution for 살인기수

#1. Uniform prior
n <- 322; y <- 190; a <- 1; b <- 1
grid <- seq(0.001, 0.999, by=0.001)
prior1 <- dbeta(grid, a, b); prior1 <- prior1/sum(prior1)
prob <- dbinom(y, n, grid); prob <- prob/sum(prob)
posterior <- prior1*prob/sum(prior1*prob)

plot(grid, posterior*length(grid), type='l', lwd=2, col="red", xlab="theta", ylab="Density", ylim=c(0, 15))
lines(grid, prior1*length(grid), col="blue")
lines(grid, prob*length(grid), col="black", lty=2)
legend("topright", c("Likelihood", "Prior", "Posterior"), lwd=c(1,1,2), col=c("black","blue","red"))

#posterior mean, mode
(a+y)/(a+b+n); (a+y-1)/(a+b+n-2)
qbeta(0.025, 191, 133); qbeta(0.975, 191, 133)

#2. Hadlane's prior
n <- 322; y <- 190; a <- 0; b <- 0
grid <- seq(0.001, 0.999, by=0.001)
prior1 <- (1/grid)*(1/(1-grid)); prior1 <- prior1/sum(prior1)
prob <- dbinom(y, n, grid); prob <- prob/sum(prob)
posterior <- prior1*prob/sum(prior1*prob)

plot(grid, posterior*length(grid), type='l', lwd=2, col="red", xlab="theta", ylab="Density", ylim=c(0, 15))
lines(grid, prior1*length(grid), col="blue")
lines(grid, prob*length(grid), col="black", lty=2)
legend("topright", c("Likelihood", "Prior", "Posterior"), lwd=c(1,1,2), col=c("black","blue","red"))

#posterior mean, mode
(a+y)/(a+b+n); (a+y-1)/(a+b+n-2)
qbeta(0.025, 190, 132); qbeta(0.975, 190, 132)

#3. 2 conjugate priors of Positive and negative
n <- 322; y <- 190; a_p <- 18; b_p <- 7; a_n <- 5; b_n<-45 
grid <- seq(0.001, 0.999, by=0.001)
prior1 <- dbeta(grid, a_p, b_p); prior1 <- prior1/sum(prior1)
prior2 <- dbeta(grid, a_n, b_n); prior2 <- prior2/sum(prior2)
prob <- dbinom(y, n, grid); prob <- prob/sum(prob)
posterior1 <- prior1*prob/sum(prior1*prob)
posterior2 <- prior2*prob/sum(prior2*prob)

plot(grid, prob*length(grid), type='l', lty=2, col="black", xlab="theta", ylab="Density", ylim=c(0, 16))
lines(grid, prior1*length(grid), col="blue")
lines(grid, posterior1*length(grid), col="red", lwd=2)
legend("topright", c("Likelihood", "Prior(P)", "Posterior"), lwd=c(1,1,2), col=c("black","blue","red"))

plot(grid, prob*length(grid), type='l', lty=2, col="black", xlab="theta", ylab="Density", ylim=c(0, 16))
lines(grid, prior2*length(grid), col="forestgreen")
lines(grid, posterior2*length(grid), col="red", lwd=2)
legend("topright", c("Likelihood", "Prior(N)", "Posterior"), lwd=c(1,1,2), col=c("black","forestgreen","red"))

#posterior mean, mode
(a_p+y)/(a_p+b_p+n); (a_p+y-1)/(a_p+b_p+n-2)
#posterior mean, mode
(a_n+y)/(a_n+b_n+n); (a_n+y-1)/(a_n+b_n+n-2)

qbeta(0.025, 195,177); qbeta(0.975, 195,177)


##각 죄목들의 uniform prior에 따른 posterior들(시간이 너무 오래 걸리고 그래프 비정상적적)
N <- Crime$Total
S <- Crime$Short_Term

?mapply

x <- seq(0, 1, by=0.000001)
df_sep <- mapply(function(a, b, x) dbeta(x, a, b), S+1, N-S+1, MoreArgs = list(x = x)) %>%
  as.data.frame() %>%
  setNames(Crime$crime) %>%
  cbind(x) %>%
  pivot_longer(cols = !x, names_to = "crime", values_to = "P")
head(df_sep,20)
labs1 <- c('Other Groups', '살인기수')
plot_sep <- ggplot(data = df_sep) +
  geom_line(aes(x = x, y = P/N, color = (crime=='살인기수'), group = crime)) +
  labs(x = 'Treatment effect', y = '', title = 'Separate model', color = '') +
  scale_y_continuous(breaks = NULL) +
  scale_color_manual(values = c('blue','red'), labels = labs1) +
  theme(legend.background = element_blank(), legend.position = c(0.8,0.9))

df_sep[100:120,]
warnings()
plot_sep


#n이 적은 강력범죄만 일단 posterior들을 그려본다
library(ggplot2)
N <- Crime$Total[c(1:5,7)]
S <- Crime$Short_Term[c(1:5,7)]

x <- seq(0, 1, by=0.00001)
df_sep <- mapply(function(a, b, x) dbeta(x, a, b), S+1, N-S+1, MoreArgs = list(x = x)) %>%
  as.data.frame() %>%
  setNames(Crime$crime[c(1:5,7)]) %>%
  cbind(x) %>%
  pivot_longer(cols = !x, names_to = "crime", values_to = "P")
head(df_sep,20)
plot_sep <- ggplot(data = df_sep) +
  geom_line(aes(x = x, y = P/N, color = crime, group = crime)) +
  labs(x = 'Short Term Percengate', y = '', title = 'Separate model', color = '') +
  scale_y_continuous(breaks = NULL) +
  theme(legend.background = element_blank(), legend.position = c(0.9,0.8))

plot_sep


#Field로 나눠서
N <- Crime$Total[c(1:5,7)]
S <- Crime$Short_Term[c(1:5,7)]

x <- seq(0, 1, by=0.00001)
df_sep <- mapply(function(a, b, x) dbeta(x, a, b), S+1, N-S+1, MoreArgs = list(x = x)) %>%
  as.data.frame() %>%
  setNames(Crime$crime[c(1:5,7)]) %>%
  cbind(x) %>%
  pivot_longer(cols = !x, names_to = "crime", values_to = "P") %>% mutate(Field = ifelse(crime=="방화", "방화범죄", ifelse(crime=="강간" | crime=="유사강간", "성범죄", "강력범죄")))
head(df_sep,20)
plot_sep <- ggplot(data = df_sep) +
  geom_line(aes(x = x, y = P/N, color = Field, group = crime)) +
  labs(x = 'Short Term Percengate', y = '', title = 'Separate model', color = '') +
  scale_y_continuous(breaks = NULL) +
  theme(legend.background = element_blank(), legend.position = c(0.9,0.8))

plot_sep



Crime
#mean,variance of posterior
#Beta(1,1) prior
a<-1; b<-1; N <- Crime$Total; S <- Crime$Short_Term

Means <- (a+S)/(a+b+N)
Variances <- Means*(1-Means)/(a+b+N+1)
post_mean_var <- cbind(Crime$Field, Crime$crime, Means, Variances)
post_mean_var <- cbind(Means, Variances)

#x축 = 단기 검거 비율, y축 = 사후 분산
post_mean_var %>% as.data.frame() %>% mutate(crime = Crime$crime, Field = Crime$Field, N = Crime$Total) %>% ggplot(aes(x=Means, y=Variances)) + geom_point(aes(col=Field, size=N)) + scale_color_brewer(palette = "Paired")


#x축 = 범죄수, y축 = 단기검거비율
post_mean_var %>% as.data.frame() %>% mutate(crime = Crime$crime, Field = Crime$Field, N = Crime$Total) %>% ggplot(aes(x=N, y=Means)) + geom_point(aes(col=Field, size=3)) + scale_color_brewer(palette = "Paired")

#log10 scale(현재까지 제일 깔끔)
post_mean_var %>% as.data.frame() %>% mutate(crime = Crime$crime, Field = Crime$Field, N = Crime$Total) %>% ggplot(aes(x=log10(N), y=Means)) + geom_point(aes(col=Field, size=7)) + scale_color_brewer(palette = "Paired") + geom_text(aes(label=crime), nudge_y = 0.03)

##Field별 joint posterior(1-강력범죄)(Unif prior)
Field1 <-Crime %>% filter(Field == "강력범죄")
Np <- Field1 %>% select(Total) %>% sum()
Sp <- Field1 %>% select(Short_Term) %>% sum() 

a <- 1; b <- 1
grid <- seq(0, 1, by=0.0001)
prob1 <- dbinom(Field1$Short_Term[1], Field1$Total[1], grid); prob1 <- prob1/sum(prob1)
prob2 <- dbinom(Field1$Short_Term[2], Field1$Total[2], grid); prob2 <- prob2/sum(prob2)
prob3 <- dbinom(Field1$Short_Term[3], Field1$Total[3], grid); prob3 <- prob3/sum(prob3)
posterior1 <- prob1*prob2*prob3/sum(prob1*prob2*prob3)

plot(grid, posterior1*length(grid), type='l', lwd=2, col="red", xlab="theta", ylab="Density", xlim=c(0.4, 0.8), main="강력범죄 Joint Posterior")
lines(grid, prob1*length(grid), col="dodger blue")
lines(grid, prob2*length(grid), col="sky blue")
lines(grid, prob3*length(grid), col="blue")
lines(grid, dunif(grid), col="tan")
legend("topright", c("Unif Prior","살인기수", "살인미수", "강도","강력 Post"), lwd=c(1,1,1,1,2), col=c("tan","dodger blue","sky blue","blue","red"))

#posterior mean, mode
(a+Sp)/(a+b+Np); (a+Sp-1)/(a+b+Np-2)
qbeta(0.025, 867, 644)
Crime

##Field별 joint posterior(1-강력범죄)(positive beta prior)

a <- 71; b <- 31
grid <- seq(0, 1, by=0.0001)
prior <- dbeta(grid, a, b); prior<-prior/sum(prior)
prob1 <- dbinom(Field1$Short_Term[1], Field1$Total[1], grid); prob1 <- prob1/sum(prob1); prob1 <- prior*prob1/sum(prior*prob1)
prob2 <- dbinom(Field1$Short_Term[2], Field1$Total[2], grid); prob2 <- prob2/sum(prob2); prob2 <- prior*prob2/sum(prior*prob2)
prob3 <- dbinom(Field1$Short_Term[3], Field1$Total[3], grid); prob3 <- prob3/sum(prob3); prob3 <- prior*prob3/sum(prior*prob3)
posterior1.p <- prob1*prob2*prob3*(prior^3)/sum(prob1*prob2*prob3*(prior^3))

plot(grid, posterior1.p*length(grid), type='l', lwd=2, col="red", xlab="theta", ylab="Density",xlim=c(0.4, 0.8), ylim=c(0,40),main="강력범죄 Joint Posterior")
lines(grid, prob1*length(grid), col="dodger blue")
lines(grid, prob2*length(grid), col="sky blue")
lines(grid, prob3*length(grid), col="blue")
lines(grid, prior*length(grid), col="tan")
legend("topright", c("Prior(+)","살인기수", "살인미수", "강도","강력 Post"), lwd=c(1,1,1,1,2), col=c("tan","dodger blue","sky blue","blue","red"))

#posterior mean, mode
(a+Sp)/(a+b+Np); (a+Sp-1)/(a+b+Np-2)

##Field별 joint posterior(1-강력범죄)(negative beta prior)

a <- 31; b <- 71
grid <- seq(0, 1, by=0.0001)
prior <- dbeta(grid, a, b); prior<-prior/sum(prior)
prob1 <- dbinom(Field1$Short_Term[1], Field1$Total[1], grid); prob1 <- prob1/sum(prob1); prob1 <- prior*prob1/sum(prior*prob1)
prob2 <- dbinom(Field1$Short_Term[2], Field1$Total[2], grid); prob2 <- prob2/sum(prob2); prob2 <- prior*prob2/sum(prior*prob2)
prob3 <- dbinom(Field1$Short_Term[3], Field1$Total[3], grid); prob3 <- prob3/sum(prob3); prob3 <- prior*prob3/sum(prior*prob3)
posterior1.n <- prob1*prob2*prob3*(prior^3)/sum(prob1*prob2*prob3*(prior^3))

plot(grid, posterior1.n*length(grid), type='l', lwd=2, col="red", xlab="theta", ylab="Density",xlim=c(0.2, 0.7), main="강력범죄 Joint Posterior")
lines(grid, prob1*length(grid), col="dodger blue")
lines(grid, prob2*length(grid), col="sky blue")
lines(grid, prob3*length(grid), col="blue")
lines(grid, prior*length(grid), col="tan")
legend("topright", c("Prior(-)","살인기수", "살인미수", "강도","강력 Post"), lwd=c(1,1,1,1,2), col=c("tan","dodger blue","sky blue","blue","red"))

#posterior mean, mode
(a+Sp)/(a+b+Np); (a+Sp-1)/(a+b+Np-2)

##Field별 joint posterior(2-성범죄)(Unif Prior)
Field2 <-Crime %>% filter(Field == "성범죄")
Np <- Field2 %>% select(Total) %>% sum()
Sp <- Field2 %>% select(Short_Term) %>% sum() 

a <- 1; b <- 1
grid <- seq(0, 1, by=0.0001)
prob1 <- dbinom(Field2$Short_Term[1], Field2$Total[1], grid); prob1 <- prob1/sum(prob1)
prob2 <- dbinom(Field2$Short_Term[2], Field2$Total[2], grid); prob2 <- prob2/sum(prob2)
prob3 <- dbinom(Field2$Short_Term[3], Field2$Total[3], grid); prob3 <- prob3/sum(prob3)
posterior2 <- prob1*prob2*prob3/sum(prob1*prob2*prob3)

plot(grid, posterior2*length(grid), type='l', lwd=2, col="red", xlab="theta", ylab="Density", xlim=c(0.05,0.3), main="성범죄 Joint Posterior")
lines(grid, prob1*length(grid), col="dodger blue")
lines(grid, prob2*length(grid), col="sky blue")
lines(grid, prob3*length(grid), col="blue")
legend("topright", c("강간", "유사강간", "강제추행","성범죄 Post"), lwd=c(1,1,1,2), col=c("dodger blue","sky blue","blue","red"))

##Field별 joint posterior(2-성범죄)(Positive Prior)
a <- 71; b <- 31
grid <- seq(0, 1, by=0.0001)
prior <- dbeta(grid, a, b); prior<-prior/sum(prior)
prob1 <- dbinom(Field2$Short_Term[1], Field2$Total[1], grid); prob1 <- prob1/sum(prob1)
prob2 <- dbinom(Field2$Short_Term[2], Field2$Total[2], grid); prob2 <- prob2/sum(prob2)
prob3 <- dbinom(Field2$Short_Term[3], Field2$Total[3], grid); prob3 <- prob3/sum(prob3)
posterior2.p <- prob1*prob2*prob3*(prior^3)/sum(prob1*prob2*prob3*(prior^3))

plot(grid, posterior2.p*length(grid), type='l', lwd=2, col="red", xlab="theta", ylab="Density", xlim=c(0.1,0.7), main="성범죄 Joint Posterior")
lines(grid, prob1*length(grid), col="dodger blue")
lines(grid, prob2*length(grid), col="sky blue")
lines(grid, prob3*length(grid), col="blue")
lines(grid, prior*length(grid), col="tan")
legend("topright", c("Prior(+)","강간", "유사강간", "강제추행","성범죄 Post"), lwd=c(1,1,1,1,2), col=c("tan","dodger blue","sky blue","blue","red"))

#posterior mean, mode
(a+Sp)/(a+b+Np); (a+Sp-1)/(a+b+Np-2)

##Field별 joint posterior(2-성범죄)(Negative Prior)
a <- 31; b <- 71
grid <- seq(0, 1, by=0.0001)
prior <- dbeta(grid, a, b); prior<-prior/sum(prior)
prob1 <- dbinom(Field2$Short_Term[1], Field2$Total[1], grid); prob1 <- prob1/sum(prob1)
prob2 <- dbinom(Field2$Short_Term[2], Field2$Total[2], grid); prob2 <- prob2/sum(prob2)
prob3 <- dbinom(Field2$Short_Term[3], Field2$Total[3], grid); prob3 <- prob3/sum(prob3)
posterior2.n <- prob1*prob2*prob3*(prior^3)/sum(prob1*prob2*prob3*(prior^3))

plot(grid, posterior2.n*length(grid), type='l', lwd=2, col="red", xlab="theta", ylab="Density", xlim=c(0.1,0.7), main="성범죄 Joint Posterior")
lines(grid, prob1*length(grid), col="dodger blue")
lines(grid, prob2*length(grid), col="sky blue")
lines(grid, prob3*length(grid), col="blue")
lines(grid, prior*length(grid), col="tan")
legend("topright", c("Prior(-)","강간", "유사강간", "강제추행","성범죄 Post"), lwd=c(1,1,1,1,2), col=c("tan","dodger blue","sky blue","blue","red"))

#posterior mean, mode
(a+Sp)/(a+b+Np); (a+Sp-1)/(a+b+Np-2)


##Field별 joint posterior(3-신체폭력범죄)(Unif Prior)
Field3 <-Crime %>% filter(Field == "신체폭력")
Np <- Field3 %>% select(Total) %>% sum()
Sp <- Field3 %>% select(Short_Term) %>% sum() 

a <- 1; b <- 1
grid <- seq(0, 1, by=0.0001)
prob1 <- dbinom(Field3$Short_Term[1], Field3$Total[1], grid); prob1 <- prob1/sum(prob1)
prob2 <- dbinom(Field3$Short_Term[2], Field3$Total[2], grid); prob2 <- prob2/sum(prob2)
prob3 <- dbinom(Field3$Short_Term[3], Field3$Total[3], grid); prob3 <- prob3/sum(prob3)
prob4 <- dbinom(Field3$Short_Term[4], Field3$Total[4], grid); prob4 <- prob4/sum(prob4)
posterior3 <- prob1*prob2*prob3*prob4/sum(prob1*prob2*prob3*prob4)

plot(grid, posterior3*length(grid), type='l', lwd=2, col="red", xlab="theta", ylab="Density", xlim=c(0.25, 0.53), main="신체폭력 Joint Posterior")
lines(grid, prob1*length(grid), col="dodger blue")
lines(grid, prob2*length(grid), col="sky blue")
lines(grid, prob3*length(grid), col="blue")
lines(grid, prob4*length(grid), col="purple")
legend("topright", c("상해", "폭행", "체포/감금","협박","성범죄 Post"), lwd=c(1,1,1,1,2), col=c("dodger blue","sky blue","blue","purple","red"))

#posterior mean, mode
(a+Sp)/(a+b+Np); (a+Sp-1)/(a+b+Np-2)

##Field별 joint posterior(3-신체폭력범죄)(Positive Prior)
a <- 71; b <- 31
grid <- seq(0, 1, by=0.0001)
prior <- dbeta(grid, a, b); prior<-prior/sum(prior)
prob1 <- dbinom(Field3$Short_Term[1], Field3$Total[1], grid); prob1 <- prob1/sum(prob1); prob1 <- prior*prob1/sum(prob1*prior)
prob2 <- dbinom(Field3$Short_Term[2], Field3$Total[2], grid); prob2 <- prob2/sum(prob2); prob2 <- prior*prob2/sum(prob2*prior)
prob3 <- dbinom(Field3$Short_Term[3], Field3$Total[3], grid); prob3 <- prob3/sum(prob3); prob3 <- prior*prob3/sum(prob3*prior)
prob4 <- dbinom(Field3$Short_Term[4], Field3$Total[4], grid); prob4 <- prob4/sum(prob4); prob4 <- prior*prob4/sum(prob4*prior)
posterior3.p <- prob1*prob2*prob3*prob4*(prior^4)/sum(prob1*prob2*prob3*prob4*(prior^4))

plot(grid, posterior3.p*length(grid), type='l', lwd=2, col="red", xlab="theta", ylab="Density", xlim=c(0.25, 0.53), main="신체폭력 Joint Posterior")
lines(grid, prob1*length(grid), col="dodger blue")
lines(grid, prob2*length(grid), col="sky blue")
lines(grid, prob3*length(grid), col="blue")
lines(grid, prob4*length(grid), col="purple")
lines(grid, prior*length(grid), col="tan")
legend("topright", c("Prior(+)","상해", "폭행", "체포/감금","협박","폭력범죄 Post"), lwd=c(1,1,1,1,1,2), col=c("tan","dodger blue","sky blue","blue","purple","red"))

#posterior mean, mode
(a+Sp)/(a+b+Np); (a+Sp-1)/(a+b+Np-2)
m <- (a+Sp)/(a+b+Np)
m*(1-m)/(a+b+Np+1)

##Field별 joint posterior(3-신체폭력범죄)(Negative Prior)
a <- 31; b <- 71
grid <- seq(0, 1, by=0.0001)
prior <- dbeta(grid, a, b); prior<-prior/sum(prior)
prob1 <- dbinom(Field3$Short_Term[1], Field3$Total[1], grid); prob1 <- prob1/sum(prob1); prob1 <- prior*prob1/sum(prob1*prior)
prob2 <- dbinom(Field3$Short_Term[2], Field3$Total[2], grid); prob2 <- prob2/sum(prob2); prob2 <- prior*prob2/sum(prob2*prior)
prob3 <- dbinom(Field3$Short_Term[3], Field3$Total[3], grid); prob3 <- prob3/sum(prob3); prob3 <- prior*prob3/sum(prob3*prior)
prob4 <- dbinom(Field3$Short_Term[4], Field3$Total[4], grid); prob4 <- prob4/sum(prob4); prob4 <- prior*prob4/sum(prob4*prior)
posterior3.n <- prob1*prob2*prob3*prob4*(prior^4)/sum(prob1*prob2*prob3*prob4*(prior^4))

plot(grid, posterior3.n*length(grid), type='l', lwd=2, col="red", xlab="theta", ylab="Density", xlim=c(0.25, 0.53), main="신체폭력 Joint Posterior")
lines(grid, prob1*length(grid), col="dodger blue")
lines(grid, prob2*length(grid), col="sky blue")
lines(grid, prob3*length(grid), col="blue")
lines(grid, prob4*length(grid), col="purple")
lines(grid, prior*length(grid), col="orange")
legend("topright", c("Prior(-)","상해", "폭행", "체포/감금","협박","폭력범죄 Post"), lwd=c(1,1,1,1,1,2), col=c("orange","dodger blue","sky blue","blue","purple","red"))

#posterior mean, mode
(a+Sp)/(a+b+Np); (a+Sp-1)/(a+b+Np-2)
m <- (a+Sp)/(a+b+Np)
m*(1-m)/(a+b+Np+1)

##Field별 joint posterior(4-재산범죄)
Field4 <-Crime %>% filter(Field == "재산폭력")
Np <- Field4 %>% select(Total) %>% sum()
Sp <- Field4 %>% select(Short_Term) %>% sum() 

a <- 1; b <- 1
grid <- seq(0, 1, by=0.0001)
prob1 <- dbinom(Field4$Short_Term[1], Field4$Total[1], grid); prob1 <- prob1/sum(prob1)
prob2 <- dbinom(Field4$Short_Term[2], Field4$Total[2], grid); prob2 <- prob2/sum(prob2)
posterior4 <- dbeta(grid, Sp+1, Np-Sp+1)

Crime

plot(grid, posterior4, type='l', lwd=2, col="red", xlab="theta", ylab="Density", xlim=c(0.1, 0.5),main="재산범죄 Joint Posterior")
lines(grid, prob1*length(grid), col="dodger blue")
lines(grid, prob2*length(grid), col="sky blue")
legend("topright", c("공갈", "손괴" ,"재산범죄 Post"), lwd=c(1,1,2), col=c("dodger blue","sky blue","red"))

#posterior mean, mode
(a+Sp)/(a+b+Np); (a+Sp-1)/(a+b+Np-2)

##Field별 joint posterior(4-재산범죄)(Positive Prior)
a <- 71; b <- 31
grid <- seq(0, 1, by=0.0001)
prior <- dbeta(grid, a, b); prior<-prior/sum(prior)
prob1 <- dbinom(Field4$Short_Term[1], Field4$Total[1], grid); prob1 <- prob1/sum(prob1); prob1 <- prior*prob1/sum(prob1*prior)
prob2 <- dbinom(Field4$Short_Term[2], Field4$Total[2], grid); prob2 <- prob2/sum(prob2); prob2 <- prior*prob2/sum(prob2*prior)
posterior4.p <- dbeta(grid, Sp+2*a-1, Np-Sp+2*b-1)

plot(grid, posterior4.p, type='l', lwd=2, col="red", xlab="theta", ylab="Density", xlim=c(0.15, 0.73), main="재산범죄 Joint Posterior")
lines(grid, prob1*length(grid), col="dodger blue")
lines(grid, prob2*length(grid), col="sky blue")
lines(grid, prior*length(grid), col="tan")
legend("topright", c("Prior(+)","공갈","손괴","재산범죄 Post"), lwd=c(1,1,1,2), col=c("tan","dodger blue","sky blue","red"))

##Field별 joint posterior(4-재산범죄)(Negative Prior)
a <- 31; b <- 71
grid <- seq(0, 1, by=0.0001)
prior <- dbeta(grid, a, b); prior<-prior/sum(prior)
prob1 <- dbinom(Field4$Short_Term[1], Field4$Total[1], grid); prob1 <- prob1/sum(prob1); prob1 <- prior*prob1/sum(prob1*prior)
prob2 <- dbinom(Field4$Short_Term[2], Field4$Total[2], grid); prob2 <- prob2/sum(prob2); prob2 <- prior*prob2/sum(prob2*prior)
posterior4.n <- dbeta(grid, Sp+2*a-1, Np-Sp+2*b-1)

plot(grid, posterior4.n, type='l', lwd=2, col="red", xlab="theta", ylab="Density", xlim=c(0.15, 0.73), main="재산범죄 Joint Posterior")
lines(grid, prob1*length(grid), col="dodger blue")
lines(grid, prob2*length(grid), col="sky blue")
lines(grid, prior*length(grid), col="tan")
legend("topright", c("Prior(-)","공갈","손괴","재산범죄 Post"), lwd=c(1,1,1,2), col=c("tan","dodger blue","sky blue","red"))

Crime
##Field별 joint posterior(5-지능범죄)(Unif Prior)
Field5 <-Crime %>% filter(Field == "지능범죄")
Np <- Field5 %>% select(Total) %>% sum()
Sp <- Field5 %>% select(Short_Term) %>% sum() 

a <- 1; b <- 1
grid <- seq(0, 1, by=0.0001)
prob1 <- dbinom(Field5$Short_Term[1], Field5$Total[1], grid); prob1 <- prob1/sum(prob1)
prob2 <- dbinom(Field5$Short_Term[2], Field5$Total[2], grid); prob2 <- prob2/sum(prob2)
posterior5 <- dbeta(grid, Sp+1, Np-Sp+1)

plot(grid, posterior5, type='l', lwd=2, col="red", xlab="theta", ylab="Density", xlim=c(0.145, 0.285),main="지능범죄 Joint Posterior")
lines(grid, prob1*length(grid), col="dodger blue")
lines(grid, prob2*length(grid), col="sky blue")
legend("topright", c("사기", "횡령" ,"지능범죄 Post"), lwd=c(1,1,2), col=c("dodger blue","sky blue","red"))

#posterior mean, mode
(a+Sp)/(a+b+Np); (a+Sp-1)/(a+b+Np-2)

##Field별 joint posterior(5-지능범죄)(Positive Prior)
a <- 71; b <- 31
grid <- seq(0, 1, by=0.0001)
prob1 <- dbinom(Field5$Short_Term[1], Field5$Total[1], grid); prob1 <- prob1/sum(prob1)
prob2 <- dbinom(Field5$Short_Term[2], Field5$Total[2], grid); prob2 <- prob2/sum(prob2)
posterior5.p <- dbeta(grid, Sp+2*a-1, Np-Sp+2*b-1)

plot(grid, posterior5.p, type='l', lwd=2, col="red", xlab="theta", ylab="Density", xlim=c(0.145, 0.285),main="지능범죄 Joint Posterior")
lines(grid, prob1*length(grid), col="dodger blue")
lines(grid, prob2*length(grid), col="sky blue")
legend("topright", c("Prior(+)","사기", "횡령" ,"지능범죄 Post"), lwd=c(1,1,1,2), col=c("tan","dodger blue","sky blue","red"))

##Field별 joint posterior(5-지능범죄)(Negative Prior)
a <- 31; b <- 71
grid <- seq(0, 1, by=0.0001)
prob1 <- dbinom(Field5$Short_Term[1], Field5$Total[1], grid); prob1 <- prob1/sum(prob1)
prob2 <- dbinom(Field5$Short_Term[2], Field5$Total[2], grid); prob2 <- prob2/sum(prob2)
posterior5.n <- dbeta(grid, Sp+2*a-1, Np-Sp+2*b-1)

plot(grid, posterior5.n, type='l', lwd=2, col="red", xlab="theta", ylab="Density", xlim=c(0.145, 0.285),main="지능범죄 Joint Posterior")
lines(grid, prob1*length(grid), col="dodger blue")
lines(grid, prob2*length(grid), col="sky blue")
legend("topright", c("Prior(-)","사기", "횡령" ,"지능범죄 Post"), lwd=c(1,1,1,2), col=c("tan","dodger blue","sky blue","red"))


#5개 field joint post
plot(grid, posterior5, type='l', lwd=2, col="red", xlab="theta", ylab="Density", xlim=c(0.16,0.61),main="5 plots of Field Joint Posterior")
lines(grid, posterior1*length(grid), col="orange")
lines(grid, posterior2*length(grid), col="forestgreen")
lines(grid, posterior3*length(grid), col="blue")
lines(grid, posterior4, col="purple")
legend("topright", c("지능범죄","강력범죄", "성범죄" ,"폭력범죄", "재산범죄"), lwd=c(1,1,1,1,1), col=c("red","orange","forestgreen","blue","purple"))


#10개 field joint post
ap<-71; bp<-31; an<-31; bn<-71
Field6 <-Crime %>% filter(Field == "방화범죄")
Np <- Field6 %>% select(Total) %>% sum()
Sp <- Field6 %>% select(Short_Term) %>% sum() 
posterior6 <- dbeta(grid, Sp+1, Np-Sp+1)
posterior6.p <- dbeta(grid, Sp+ap, Np-Sp+bp)
posterior6.n <- dbeta(grid, Sp+an, Np-Sp+bn)

Field7 <-Crime %>% filter(Field == "절도범죄")
Np <- Field7 %>% select(Total) %>% sum()
Sp <- Field7 %>% select(Short_Term) %>% sum() 
posterior7 <- dbeta(grid, Sp+1, Np-Sp+1)
posterior7.p <- dbeta(grid, Sp+ap, Np-Sp+bp)
posterior7.n <- dbeta(grid, Sp+an, Np-Sp+bn)

Field8 <-Crime %>% filter(Field == "풍속범죄")
Np <- Field8 %>% select(Total) %>% sum()
Sp <- Field8 %>% select(Short_Term) %>% sum() 
posterior8 <- dbeta(grid, Sp+1, Np-Sp+1)
posterior8.p <- dbeta(grid, Sp+ap, Np-Sp+bp)
posterior8.n <- dbeta(grid, Sp+an, Np-Sp+bn)

Field9 <-Crime %>% filter(Field == "마약범죄")
Np <- Field9 %>% select(Total) %>% sum()
Sp <- Field9 %>% select(Short_Term) %>% sum() 
posterior9 <- dbeta(grid, Sp+1, Np-Sp+1)
posterior9.p <- dbeta(grid, Sp+ap, Np-Sp+bp)
posterior9.n <- dbeta(grid, Sp+an, Np-Sp+bn)

Field10 <-Crime %>% filter(Field == "교통범죄")
Np <- Field10 %>% select(Total) %>% sum()
Sp <- Field10 %>% select(Short_Term) %>% sum() 
posterior10 <- dbeta(grid, Sp+1, Np-Sp+1)
posterior10.p <- dbeta(grid, Sp+ap, Np-Sp+bp)
posterior10.n <- dbeta(grid, Sp+an, Np-Sp+bn)

plot(grid, posterior10, type='l', lwd=2, col="red", xlab="theta", ylab="Density", xlim=c(0.16, 0.75), ylim=c(0,600),main="10 plots of Field Joint Posterior")
lines(grid, posterior1*length(grid), col="coral", lwd=2)
lines(grid, posterior2*length(grid), col="tan", lwd=2)
lines(grid, posterior3*length(grid), col="green", lwd=2)
lines(grid, posterior4, col="forestgreen", lwd=2)
lines(grid, posterior5, col="skyblue", lwd=2)
lines(grid, posterior6, col="blue", lwd=2)
lines(grid, posterior7, col="purple", lwd=2)
lines(grid, posterior8, col="gray", lwd=2)
lines(grid, posterior9, col="black", lwd=2)
legend("topright", c("강력범죄","성범죄","폭력범죄","재산범죄","지능범죄","방화범죄","절도범죄","도박범죄","마약범죄",
"교통범죄"), lwd=c(rep(1,10)), col=c("coral", "tan","green","forestgreen","skyblue","blue","purple","gray","magenta","red"))

#p
prior.p <- dbeta(grid, ap, bp); prior.p<- prior.p/sum(prior.p)
plot(grid, posterior10, type='l', lwd=2, col="red", xlab="theta", ylab="Density", xlim=c(0.16, 0.75), ylim=c(0,600),main="10 plots of Field Joint Posterior")
lines(grid, posterior1.p*length(grid), col="coral", lwd=2)
lines(grid, posterior2.p*length(grid), col="green", lwd=2)
lines(grid, posterior3.p*length(grid), col="forestgreen", lwd=2)
lines(grid, posterior4.p, col="skyblue", lwd=2)
lines(grid, posterior5.p, col="blue", lwd=2)
lines(grid, posterior6.p, col="purple", lwd=2)
lines(grid, posterior7.p, col="gray", lwd=2)
lines(grid, posterior8.p, col="magenta", lwd=2)
lines(grid, posterior9.p, col="black", lwd=2)
lines(grid, prior.p*length(grid), col="tan", lwd=2)
legend("topright", c("강력범죄","성범죄","폭력범죄","재산범죄","지능범죄","방화범죄","절도범죄","도박범죄","마약범죄",
                     "교통범죄", "Prior(+)"), lwd=c(rep(1,10)), col=c("coral","green","forestgreen","skyblue","blue","purple","gray","magenta","black","red", "tan"))

#n
prior.n <- dbeta(grid, an, bn); prior.n<- prior.n/sum(prior.n)
plot(grid, posterior10, type='l', lwd=2, col="red", xlab="theta", ylab="Density", xlim=c(0.16, 0.75), ylim=c(0,600),main="10 plots of Field Joint Posterior")
lines(grid, posterior1.n*length(grid), col="coral", lwd=2)
lines(grid, posterior2.n*length(grid), col="green", lwd=2)
lines(grid, posterior3.n*length(grid), col="forestgreen", lwd=2)
lines(grid, posterior4.n, col="skyblue", lwd=2)
lines(grid, posterior5.n, col="blue", lwd=2)
lines(grid, posterior6.n, col="purple", lwd=2)
lines(grid, posterior7.n, col="gray", lwd=2)
lines(grid, posterior8.n, col="magenta", lwd=2)
lines(grid, posterior9.n, col="black", lwd=2)
lines(grid, prior.n*length(grid), col="tan", lwd=2)
legend("topright", c("강력범죄","성범죄","폭력범죄","재산범죄","지능범죄","방화범죄","절도범죄","도박범죄","마약범죄",
                     "교통범죄", "Prior(-)"), lwd=c(rep(1,10)), col=c("coral","green","forestgreen","skyblue","blue","purple","gray","magenta","black","red", "tan"))


## 전체 Joint Posterior
grid <- seq(0, 1, by=0.0001)
Np <- Crime %>% select(Total) %>% sum()
Sp <- Crime %>% select(Short_Term) %>% sum() 
posterior.J <- dbeta(grid, Sp+1, Np-Sp+1)

m <- (1+Sp)/(2+Np)
m*(1-m)/(2+Np+1)
(Sp+(an-1)*19+1)/(Sp+(an-1)*19+1+Np-Sp+(bn-1)*19+1)
#hyperprior, prior
hyperprior <- dunif(grid, 0, 1)
prior <- dunif(grid, 0, 1)
prior.p <- dbeta(grid, 71, 31)
prior.n <- dbeta(grid, 31, 71)

#likelihoods
prob <- matrix(rep(0, 19*length(grid)), nrow=19, ncol=length(grid), byrow=T)
for (i in 1:19){
  for (j in 1:length(grid)){
    prob[i,j] <- dbinom(Crime$Short_Term[i], Crime$Total[i], grid[j])
  }
    prob[i,] <- prob[i,]/sum(prob[i,])
}

plot(grid, hyperprior, type='l', lwd=1, col="tan", xlab="theta", ylab="Density", xlim=c(0.1, 0.75), ylim=c(0,800),main="All plots")
lines(grid, prior, col="tan", lwd=2)
for (i in 1:19){
  lines(grid, prob[i,]*length(grid), col="black", lwd=1)
}
lines(grid, posterior1*length(grid), col="blue", lwd=2)
lines(grid, posterior2*length(grid), col="blue", lwd=2)
lines(grid, posterior3*length(grid), col="blue", lwd=2)
lines(grid, posterior4, col="blue", lwd=2)
lines(grid, posterior5, col="blue", lwd=2)
lines(grid, posterior6, col="blue", lwd=2)
lines(grid, posterior7, col="blue", lwd=2)
lines(grid, posterior8, col="blue", lwd=2)
lines(grid, posterior9, col="blue", lwd=2)
lines(grid, posterior10, col="blue", lwd=2)
lines(grid, posterior.J, col="red", lwd=2)

legend("topright", c("Full Joint Post", "Joint of each Field", "Likelihood", "Prior", "Hyperprior"), lwd=c(1,1,1,1,1), col=c("red", "blue","black","tan","tan"))

#total joint posterior with positive priors
posterior.J.p <- dbeta(grid, Sp+(ap-1)*19+1, Np-Sp+(bp-1)*19+1)
plot(grid, hyperprior, type='l', lwd=1, col="tan", xlab="theta", ylab="Density", xlim=c(0.1, 0.75), ylim=c(0,800),main="All plots")
lines(grid, prior.p, col="forestgreen", lwd=2)
for (i in 1:19){
  lines(grid, prob[i,]*length(grid), col="black", lwd=1)
}
lines(grid, posterior1.p*length(grid), col="blue", lwd=2)
lines(grid, posterior2.p*length(grid), col="blue", lwd=2)
lines(grid, posterior3.p*length(grid), col="blue", lwd=2)
lines(grid, posterior4.p, col="blue", lwd=2)
lines(grid, posterior5.p, col="blue", lwd=2)
lines(grid, posterior6.p, col="blue", lwd=2)
lines(grid, posterior7.p, col="blue", lwd=2)
lines(grid, posterior8.p, col="blue", lwd=2)
lines(grid, posterior9.p, col="blue", lwd=2)
lines(grid, posterior10.p, col="blue", lwd=2)
lines(grid, posterior.J.p, col="red", lwd=2)

legend("topright", c("Full Joint Post", "Joint of each Field", "Likelihood", "Prior(+)", "Hyperprior"), lwd=c(1,1,1,1,1), col=c("red", "blue","black","forestgreen","tan"))

#total joint posterior with negative priors
posterior.J.n <- dbeta(grid, Sp+(an-1)*19+1, Np-Sp+(bn-1)*19+1)
plot(grid, hyperprior, type='l', lwd=1, col="tan", xlab="theta", ylab="Density", xlim=c(0.1, 0.75), ylim=c(0,800),main="All plots")
lines(grid, prior.n, col="forestgreen", lwd=2)
for (i in 1:19){
  lines(grid, prob[i,]*length(grid), col="black", lwd=1)
}
lines(grid, posterior1.p*length(grid), col="blue", lwd=2)
lines(grid, posterior2.p*length(grid), col="blue", lwd=2)
lines(grid, posterior3.p*length(grid), col="blue", lwd=2)
lines(grid, posterior4.p, col="blue", lwd=2)
lines(grid, posterior5.p, col="blue", lwd=2)
lines(grid, posterior6.p, col="blue", lwd=2)
lines(grid, posterior7.p, col="blue", lwd=2)
lines(grid, posterior8.p, col="blue", lwd=2)
lines(grid, posterior9.p, col="blue", lwd=2)
lines(grid, posterior10.p, col="blue", lwd=2)
lines(grid, posterior.J.p, col="red", lwd=2)

legend("topright", c("Full Joint Post", "Joint of each Field", "Likelihood", "Prior(-)", "Hyperprior"), lwd=c(1,1,1,1,1), col=c("red", "blue","black","forestgreen","tan"))
