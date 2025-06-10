rm(list = ls())
# define a function 
Classification <- function(x, breakpoint = c(14999,24999,34999,49999,74999,99999,149999,199999,200000)){
  if(x <= breakpoint[1]){
    return(1)
  }else if(breakpoint[1] < x & x <= breakpoint[2]){
    return(2)
  }else if(breakpoint[2] < x & x <= breakpoint[3]){
    return(3)
  }else if(breakpoint[3] < x & x <= breakpoint[4]){
    return(4)
  }else if(breakpoint[4] < x & x <= breakpoint[5]){
    return(5)
  }else if(breakpoint[5] < x & x <= breakpoint[6]){
    return(6)
  }else if(breakpoint[6] < x & x <= breakpoint[7]){
    return(7)
  }else if(breakpoint[7] < x & x <= breakpoint[8]){
    return(8)
  }else if(breakpoint[9] <= x){
    return(9)
  }
}
# load data
df <- read.csv('C:/Users/yilin/Dropbox/My PC (DESKTOP-BH77LRG)/Documents/Fachsemester6/Bachelorarbeit/Data/public use dara 2021 usa/pppub21.csv')

income <- df$PTOTVAL
income
max(income)
#remove negative value
idx <- income < 0
income2 <- df[!idx,]$PTOTVAL
income2 <- income2
income2 <- data.frame(income2)
min(income2)
max(income2)
idx <- income < 0
income3 <- df[!idx,]$PTOTVAL


# apply classification function
pclass <- apply(income2, 1, Classification)
pclass
typeof(pclass)
###Datain list-form
result <- list()
for (i in 1:9) {
  idx <- pclass==i
  df1 <- data.frame(income2[idx,])
  colnames(df1) <- paste0('class',i)
  result[[i]] <- df1
}

# example for class9
ak9 <- result[[9]]
ak9
sum(result[[2]])
total_1 <- nrow(result[[1]])
#75398
total_2 <- nrow(result[[2]])
#16381
total_3 <- nrow(result[[3]])
#13735
total_4 <- nrow(result[[4]])
#16025
total_5 <- nrow(result[[5]])
#18178
total_6 <- nrow(result[[6]])
#8929
total_7 <- nrow(result[[7]])
# 8152
total_8 <- nrow(result[[8]])
#3257
total_9 <- nrow(result[[9]])
#3335
total_1/length(unlist(result)) * 2000##923






counts <- c(total_1, total_2, total_3, total_4, total_5, total_6, total_7, total_8, total_9)
income_class <- c("class1", "class2", "class3", "class4", "class5", "class6", "class7", "class8", "class9")
income_amount <- c(sum(result[[1]]), sum(result[[2]]), sum(result[[3]]), sum(result[[4]]), sum(result[[5]]), sum(result[[6]]), sum(result[[7]]), sum(result[[8]]), sum(result[[9]]))
income_amount
income_df <- data.frame(income_class, income_amount, counts)
income_df

income_df$income_class <- factor(income_df$income_class) 
income_class
hist(as.numeric(income_df$amount))
ggplot(dk, aes(x=pclass)) + geom_histogram()
pclass
hist(c(income_df$counts))
income_matrix <- matrix(1:18, nrow = 9, ncol=2)
income_matrix
rnames <- c("class1", "class2", "class3", "class4", "class5", "class6", "class7", "class8", "class9")
cnames <- c("total_income")
cnames2 <- c("counts")
cnames3 <- c("Proportion of income classes")

min(income3)


income_matrix <- matrix(income_amount, nrow = 9, ncol = 1, byrow = T, dimnames = list(rnames, cnames))
income_matrixatrix2 <- matrix(counts, nrow = 9, ncol = 1, byrow = T, dimnames = list(rnames, cnames2))
income_matrix2
income_matrix <- cbind(income_matrix, income_matrix2)
income_matrix
income_matrix_df <- as.data.frame(income_matrix)
income_matrix[1,1]

typeof(L1)
prop_income <- c(L_percentage)
prop_population <- c(p_percentage)
income_matrix3 <- matrix(prop_income, nrow = 9, ncol = 1, byrow = TRUE, dimnames = list(rnames, cnames3))
income_matrix3
income_df3<- data.frame(income_class, prop_income, prop_income)
income_df3
##bar of proportion of each income classes
library(ggplot2)
ggplot(data = income_df3, aes(x = income_class, y = prop_population))+theme_bw()+geom_bar(stat ="identity")+
  labs(y = "propotion of income classes", x = "income classes")+ theme(axis.title = element_text(size = 15, face = "bold"))+
  theme(panel.background = element_blank(), panel.grid.minor = element_blank())+theme_classic()


ggplot(data = income_df3, aes(x = income_class, y = prop_population))+theme_bw()+geom_bar(stat ="identity")+
  labs(y = "propotion of income classes", x = "income classes")+ theme(axis.title = element_text(size = 15, face = "bold"))+
  theme(panel.background = element_blank(), panel.grid.minor = element_blank())+theme_classic()




###Data in Dataframe form
len <- NULL
result <- list()
for (i in 1:9) {
  idx <- pclass == i
  df1 <- data.frame(income2[idx,])
  colnames(df1) <- paste0('class',i)
  result[[i]] <- df1
  len[i] <- sum(idx)
}
len <- max(len)
dk <- data.frame(1:len)
for (i in 1:9) {
  dff <- result[[i]]
  dff <- as.matrix(dff)
  a <- rep(NA,len-nrow(dff))
  dff <- c(dff,a)
  dk <- cbind(dk,data.frame(dff))
  colnames(dk)[i+1] <- paste0('class',i)
}
classtype <- paste0('class',1:9)
dk <- dk[, -1]
dk$class1
sum(dk$class2)
##???
length(dk$class2)

dk$class1
dk

###############################################POVCAL
n <- nrow(income2) # total population
sum_income <- sum(income2)# total income
sum_income
p_percentage <- NULL
L_percentage <- NULL
mean_class <- NULL
for (i in 1:9) {
  idx <- pclass == i
  p_percentage[i] <- sum(idx)/n
  print(sum(idx)/n)
  
  L_percentage[i] <- sum(income2[idx,])/sum_income
  print(sum(income2[idx,]/sum_income))
  mean_class[i] <- mean(income2[idx,])
}
p <- cumsum(p_percentage)
p
L <- cumsum(L_percentage)
L
typeof(p)
p1 <- p[-9]
L1 <- L[-9]
sum(L_percentage)
mean_class
p_percentage


df1 <- data.frame(L1*(1-L1),p1^2-L1,L1*(p1-1),p1-L1)
colnames(df1) <- paste0('V',1:4)
fit <- lm(V1~.+0,df1)
summary(fit) 

df2 <- data.frame(L1*(1-L1),p1^2-L1,L1*(p1-1),p1-L1)
colnames(df2) <- c("y","a","b","c")
fit2 <- lm(y ~.-1, df2)
summary(fit2)


##determine estimated value of parameters a,b,c
coefs <- fit2$coefficients
coefs
a <- coefs[1]
a
#a:1.9381515
b <- coefs[2]
b
#b:0.1620688
c <- coefs[3]
c
#c:-0.7682067 
a_value <- as.numeric(a)
a_value
b_value <- as.numeric(b)
b_value
c_value <- as.numeric(c)

##
a_value+c_value
#1.169945>0

#######summary
dk2 <- data.frame(cbind(classtype,p,L,mean_class))
dk3 <- data.frame(cbind(classtype,p,L,mean_class))
dk2
dk3
typeof(idx)

#####Values of e,m,n,r,s1,s2
e <- -(a + b + c + 1)
e_value <- as.numeric(e)
e_value
#-2.332014

m <- (b^2) - (4*a)
m_value <- as.numeric(m)
#-7.72634  

n <- (2*b*e) - (4*c)
n_value <- as.numeric(n)
#2.316933 

r <- ((n^2) - (4*m*(e^2)))^(1/2)
r_value <- as.numeric(r)
#13.16968

s1 <- (r-n)/(2*m)
s1_value <- as.numeric(s1)
#-0.7023212 

s2 <- -((r + n)/(2 * m))
s2_value <- as.numeric(s2)
#1.002196


str(income2)##163390 observations
#mean income at units level
mean_incom_units <- sum_income/163390
mean_incom_units#37370.23



##first and second derivatives of the LC and Gini index
#first derivative of GQ-LC
L1_function <- function(p) {(-b_value/2) - ((2*m_value*p + n_value) * (m_value*(p^2) + n_value*p + e_value^2)^(-1/2))/4}

#Second derivative of GQ-LC
L2_function <- function(p) {r_value^2 * ((m_value*(p^2) + n_value*p + e_value^2)^(-3/2))/8} 


##Gini index of GQ-LC
m #-7.72634 < 0: use seconde formula
library(SciViews)
Gini_GQ_function <- function(n,m,r){e/2 - (n*(b+2)/(4*m)) + r^2/(8*m*sqrt(-m)) * (asin((2*m+n)/r)- asin(n/r))}
Gini_GQ <- e/2 - (n*(b+2)/(4*m)) + r^2/(8*m*sqrt(-m)) * (asin((2*m+n)/r)- asin(n/r))
Gini_GQ <- as.numeric(Gini_GQ)
Gini_GQ
#0.6878213

##check if the gini index is valid 
a+c
#1.242514 
e_value# -2.332014<0
#=> Valid Gini formula


##range of poverty line
help("range")
range(mean_incom_units * L1_function(b = b_value, m = m_value, p = 0.001, n = n_value, e = e_value), 
      mean_incom_units * L1_function(b = b_value, m = m_value, p = 0.999, n = n_value, e = e_value))
# -12246.55 595020.74

## Poverty line z 
z <- 13790
z_value <- z

##Lorenz curve of each income class
LC_GQ_function <- function(p) {-1/2 * (b_value*p + e_value + (m_value*p^2 + n_value*p + e_value^2)^(1/2))}
LC_GQ <- (-1/2) * (b*p + e + (m*p^2 + n*p + e^2)^(1/2))
LC_GQ
#[1] 0.02609620 0.08343907 0.15029863 0.25623182 0.43152882 0.55578904 0.71869173
#[8] 0.81561917 1.00000000




##Head-count index
H <- (-1/(2*m_value)) * (n_value + r_value*(b_value+2*z_value/mean_incom_units) * (b_value+((2*z_value/mean_incom_units)^2)-m_value)^(-1/2))
H_value <- H
H_value
#Head-count index = 0.4140953



##Poverty gap index
PG <- H_value - (mean_incom_units/z_value)*LC_GQ_function(p = H_value)
PG_value <- PG
PG_value
#Poverty gap index =  0.3970173


##value of p2
p2_value <- 2*PG_value - H_value - (((mean_incom_units/z_value)^2)
                                    *(a_value*H_value 
                                       + b_value*LC_GQ_function(p = H_value)
                                       -(r_value/16)*log((1 - H_value/s1_value)/(1 - H_value/s2_value))))

p2_value
#0.5022546



###Elasticities of poverty measures with respects to the mean and the Gini index
##1.mean
#1.1 Headcount index
Elast_mean_H <- -z_value/(mean_incom_units*H_value*L2_function(p = H_value))
Elast_mean_H
#-0.4696294

#1.2 poverty Gap
Elast_mean_PG <- 1-(H_value/PG_value)
Elast_mean_PG
#-0.04301584

#1.3 Foster-Greer-Thorbecke measure
Elast_mean_SPG <- 2*(1-PG_value/p2_value)
Elast_mean_SPG
#0.4190595

##2.Gini index
#1.1 Headcount index
Elast_gini_H <- ((1-z_value/mean_incom_units)/(H_value*L2_function(p = H_value)))
Elast_gini_H
#0.8030435

#1.2 Poverty Gap
Elast_gini_PG <- 1+((mean_incom_units/z_value - 1)*H_value/PG_value)
Elast_gini_PG
#2.783507

#1.3 Foster-Greer-Thorbecke measure
Elast_gini_SPG <- 2*(1+((mean_incom_units/z_value -1)*PG_value/p2_value))
Elast_gini_SPG
#4.703332





######Beta Lorenz curve
df3 <- data.frame(log(p1-L1), 1, log(p1), log(1-p1))
colnames(df3) <- c("Y", "theta", "alpha", "beta")
fit3 <- lm(Y ~. -1, df3)
summary(fit3)
coefs_beta <- fit3$coefficients
coefs_beta
theta <- coefs_beta[1]
alpha <- coefs_beta[2]
beta <- coefs_beta[3]
exp(alpha)
theta_value <- as.numeric(theta)
alpha_value <- as.numeric(alpha)
beta_value <- as.numeric(beta)

##Lorenzcurve beta
L_beta <- function(p, theta, alpha, beta) {p - (theta*(p^alpha)*((1-p)^beta))}


##Headcount index beta
H_beta <- 1- z/mean_incom_units
H_beta
# 0.6309957


##Poverty gap index beta
PG_beta <- H_beta - (mean_incom_units/z) * L_beta(p = H_beta, theta = theta_value, alpha = alpha_value, beta = beta_value)
PG_beta
# -0.7652165

##value of parameter s1 and s2 
##Beta function
B_function <- function(alpha, beta) {(p^alpha) * ((1-p)^beta)}
##Gini of beta Lorenz curve
Gini_beta <- 2*theta*integrate(B_function(alpha = 1+alpha_value, beta = 1+beta_value),
                               lower = 0, upper = 1)



##first derivatives of the beta Lorenz curve
L1_beta_function <- function(p, theta, alpha, beta) {1- (theta*(p^alpha)*((1-p)^beta) * ((alpha/p) - (beta/(1-p))))}
##Check if the beta Lorenz curve valid:
L1_beta_function(p = 0.001, theta = theta_value, alpha = alpha_value, beta = beta_value)
#0.7963818 >= 0: Valid



##second derivatives of the beta Lorenz curve
L2_beta_function <- function(theta, p, alpha, beta) {(theta*(p^alpha)*((1-p)^beta)) * 
    (((alpha*(1-alpha))/(p^2)) + ((2*alpha*beta)/(p/(1-p))) + ((beta*(1-beta))/((1-p)^2)))}
##Check if the beta Lorenz curve valid:
L2_beta_function(p = 0.08, theta = theta_value, alpha = alpha_value, beta = beta_value)
##problem: p = 0.01-> -1.605299, p =0.02-> -0.7111067... from p = 8-> value >= 0



##Gini index of beta Lorenz curve
Gini_beta <- 2*theta*B_function



##true gini
library(DescTools)
Gini_true <- Gini(income3, unbiased = FALSE)
Gini_true



###################################################Synthetic sample
n <- nrow(income2) # total population
sum_income <- sum(income2)# total income
sum_income


dk2 <- data.frame(cbind(classtype,p,L,mean))
dk3 <- data.frame(cbind(classtype,p,L,mean))
dk2

##mean income of each income group
new_row <- data.frame(classtype = c("class0"), p = c("0"), L = c("0"), mean_class= c("0"))
new_df <- rbind(new_row, dk2)
new_df
new_df$classtype

m_incomeclass1 <- (as.numeric(new_df[2,3])- as.numeric(new_df[1,3]))/(as.numeric(new_df[2,2])-as.numeric(new_df[1,2]))
m_incomeclass1
m_incomeclass2 <- (as.numeric(new_df[3,3])- as.numeric(new_df[2,3]))/(as.numeric(new_df[3,2])-as.numeric(new_df[2,2]))
m_incomeclass2
m_incomeclass3 <- (as.numeric(new_df[4,3])- as.numeric(new_df[3,3]))/(as.numeric(new_df[4,2])-as.numeric(new_df[3,2]))
m_incomeclass3
m_incomeclass4 <- (as.numeric(new_df[5,3])- as.numeric(new_df[4,3]))/(as.numeric(new_df[5,2])-as.numeric(new_df[4,2]))
m_incomeclass4
m_incomeclass5 <- (as.numeric(new_df[6,3])- as.numeric(new_df[5,3]))/(as.numeric(new_df[6,2])-as.numeric(new_df[5,2]))
m_incomeclass5
m_incomeclass6 <- (as.numeric(new_df[7,3])- as.numeric(new_df[6,3]))/(as.numeric(new_df[7,2])-as.numeric(new_df[6,2]))
m_incomeclass6
m_incomeclass7 <- (as.numeric(new_df[8,3])- as.numeric(new_df[7,3]))/(as.numeric(new_df[8,2])-as.numeric(new_df[7,2]))
m_incomeclass7
m_incomeclass8 <- (as.numeric(new_df[9,3])- as.numeric(new_df[8,3]))/(as.numeric(new_df[9,2])-as.numeric(new_df[8,2]))
m_incomeclass8
m_incomeclass9 <- (as.numeric(new_df[10,3])- as.numeric(new_df[9,3]))/(as.numeric(new_df[10,2])-as.numeric(new_df[9,2]))
m_incomeclass9
name_df <- data.frame()
income_df1 <- data.frame(class = c("class1", "class2", "class3", "class4", "class5", "class6", "class7", "class8", "class9"), 
                        mean_income = c("0.06277711", "0.5249549", "0.7832868", "1.106375","1.608593",
                                        "2.272759", "3.173795", "4.474408", "9.471947") )


m_indome <- NULL
for (i in 2:10) {
  m_indome[i-1] <- (as.numeric(new_df$L[i])-as.numeric(new_df$L[i-1]))/(as.numeric(new_df$p[i])-as.numeric(new_df$p[i-1]))
}
m_indome
mean(m_indome)
df_m_indome <- as.data.frame(m_indome)

new_income_df <- cbind(dk2, df_m_indome)
names(new_income_df) <- c("classtype", "p", "L", "mean", "class_mincome")
new_income_df
new_df$L

##sigma
phi_pk <- qnorm(p1)
phi_pk
phi_Lk <- qnorm(L1)
phi_Lk
sigma_k <- phi_pk - phi_Lk
mean_sigma_k <- mean(sigma_k)
income2
#mean sigma_k: 1.355713

1/462
##generating of synthetic sample lognorm distribution
#sample size 1000
synthetic_smaple_1000 <- qlnorm(seq(0.0005, 0.9995, by = 0.001), sdlog = mean_sigma_k, meanlog = mean(m_indome))
synthetic_smaple_1000
qlnorm(seq(0.0005, 0.9995, by = 0.001), sdlog = mean_sigma_k, meanlog = mean_class/10000)
max(synthetic_smaple_1000)
length(synthetic_smaple_1000)
?qlnorm
#sample size 2000 
synthetic_smaple_2000 <- qlnorm(seq(0.00025, 0.99975, by = 0.0005),sdlog = mean_sigma_k, meanlog = mean(m_indome))
synthetic_smaple_2000
max(synthetic_smaple_2000)
length(synthetic_smaple_2000)
qlnorm

##population of each income class
total_1 <- nrow(result[[1]])
#75398
total_2 <- nrow(result[[2]])
#16381
total_3 <- nrow(result[[3]])
#13735
total_4 <- nrow(result[[4]])
#16025
total_5 <- nrow(result[[5]])
#18178
total_6 <- nrow(result[[6]])
#8929
total_7 <- nrow(result[[7]])
# 8152
total_8 <- nrow(result[[8]])
#3257
total_9 <- nrow(result[[9]])
#3335



##proportion of population of each income class * synthetic sample size
syn_class1 <- total_1/length(unlist(result)) * 1000
syn_c1 <- 462
#461.4603; 462
syn_class2 <- total_2/length(unlist(result)) * 1000
syn_c2 <- 100
#100.2571; 100
syn_class3 <- total_3/length(unlist(result)) * 1000
syn_c3 <- 84
#84.06267; 84
syn_class4 <- total_4/length(unlist(result)) * 1000
syn_c4 <- 98
#98.07822; 98
syn_class5 <- total_5/length(unlist(result)) * 1000
syn_c5 <- 111
#111.2553; 111
syn_class6 <- total_6/length(unlist(result)) * 1000
syn_c6 <- 55
#54.64839; 55
syn_class7 <- total_7/length(unlist(result)) * 1000
syn_c7 <- 50
#49.89289; 50
syn_class8 <- total_8/length(unlist(result)) * 1000
syn_c8 <- 20
#19.9339; 20
syn_class9 <- total_9/length(unlist(result)) * 1000
syn_c9 <- 20
#20.41129; 20

##dataframe
syn_pop_class <- c(syn_c1,syn_c2,syn_c3,syn_c4,syn_c5,syn_c6,syn_c7,syn_c8, syn_c9)
syn_class <- data.frame(syn_pop_class)
syn_class

##matrix
cnames5 <- c("population of each class")
syn_cpop_matrix <- matrix(syn_pop_class, nrow = 9, ncol = 1, byrow = T, dimnames = list(rnames, cnames5))
syn_cpop_matrix  
syn_cpop_list <- list(Syn_cpop = c(syn_c1,syn_c2,syn_c3,syn_c4,syn_c5,syn_c6,syn_c7,syn_c8, syn_c9))
syn_cpop_list  
  
##total income of each income group 
sum_syn_class1 <- sum(synthetic_smaple_1000[1:462])
#2428.046
sum_syn_class2 <- sum(synthetic_smaple_1000[463:562])
#1361.21
sum_syn_class3 <- sum(synthetic_smaple_1000[563:646])
#1559.186
sum_syn_class4 <- sum(synthetic_smaple_1000[647:744])
#2533.891
sum_syn_class5 <- sum(synthetic_smaple_1000[745:855])
#4500.115
sum_syn_class6 <- sum(synthetic_smaple_1000[856:910])
#4500.115
sum_syn_class7 <- sum(synthetic_smaple_1000[911:960])
#5001.422
sum_syn_class8 <- sum(synthetic_smaple_1000[961:980])
#3225.9
sum_syn_class9 <- sum(synthetic_smaple_1000[981:1000])
#7180.064


##dataframe form
sum_syn_class <- c(sum_syn_class1, sum_syn_class2, sum_syn_class3, sum_syn_class4,
                            sum_syn_class5, sum_syn_class6, sum_syn_class7, sum_syn_class8,
                            sum_syn_class9)
sum_syn_df <- data.frame(sum_syn_class)
sum_syn_df
##matrix
cnames4 <- c("total income of each class")
sum_syn_matrix <- matrix(sum_syn_class, nrow = 9, ncol = 1, byrow = T, dimnames = list(rnames, cnames4))
sum_syn_matrix
sum_syn_list <- list(SumSyn_class = c(sum_syn_class1, sum_syn_class2, sum_syn_class3, sum_syn_class4,
                                      sum_syn_class5, sum_syn_class6, sum_syn_class7, sum_syn_class8,
                                      sum_syn_class9))

sum_syn_list



##Mean income of each group(synthetic sample)

msyn_class1 <- sum_syn_df$sum_syn_class[1]/syn_class$syn_pop_class[1]
msyn_class2 <- sum_syn_df$sum_syn_class[2]/syn_class$syn_pop_class[2]
msyn_class3 <- sum_syn_df$sum_syn_class[3]/syn_class$syn_pop_class[3]
msyn_class4 <- sum_syn_df$sum_syn_class[4]/syn_class$syn_pop_class[4]
msyn_class5 <- sum_syn_df$sum_syn_class[5]/syn_class$syn_pop_class[5]
msyn_class6 <- sum_syn_df$sum_syn_class[6]/syn_class$syn_pop_class[6]
msyn_class7 <- sum_syn_df$sum_syn_class[7]/syn_class$syn_pop_class[7]
msyn_class8 <- sum_syn_df$sum_syn_class[8]/syn_class$syn_pop_class[8]
msyn_class9 <- sum_syn_df$sum_syn_class[9]/syn_class$syn_pop_class[9]



 
typeof(mean_class)
msyn_class <- c(msyn_class1, msyn_class2,msyn_class3,msyn_class4,msyn_class5,msyn_class6,msyn_class7,msyn_class8,msyn_class9)
msyn_class[2]
range(msyn_class)


syn_sum <- 1000
syn_cpopulation <- c(syn_class1, syn_class2, syn_class3,syn_class4, syn_class5, 
                     syn_class6, syn_class7, syn_class8,syn_class9)
syn_cpopulation
sincome_class <- c("class1", "class2", "class3", "class4", "class5", "class6", "class7", "class8", "class9")
syn_popu_prop <- c(syn_class1/syn_sum, syn_class2/syn_sum, syn_class3/syn_sum, syn_class4/syn_sum, syn_class5/syn_sum,
                   syn_class6/syn_sum, syn_class7/syn_sum, syn_class8/syn_sum, syn_class9/syn_sum)
syn_popu_prop
syn_df <- data.frame(sincome_class, syn_cpopulation, syn_popu_prop)
syn_df

ggplot(data = syn_df, aes(x = sincome_class, y = syn_popu_prop))+theme_bw()+geom_bar(stat ="identity")+
  labs(y = "propotion of income classes(synthetic sample)", x = " income classes of synthetic sample")+ theme(axis.title = element_text(size = 15, face = "bold"))+
  theme(panel.background = element_blank(), panel.grid.minor = element_blank())+theme_classic()


ggplot(data = syn_df, aes(x = syn_cpopulation)) + geom_density(alpha = 0.5)
p_percentage





##Adjustment step1:

S1000_sample <- rep(NA,1000)

for (i in 463:980) {
  if (synthetic_smaple_1000[i] < msyn_class[2] && synthetic_smaple_1000[i] > msyn_class[1]) {
    S1000_sample[i] <- m_indome[2] + ((m_indome[3]-m_indome[2])/(msyn_class[3]-msyn_class[2]))*(synthetic_smaple_1000[i]-msyn_class[2])
    
  } else if (synthetic_smaple_1000[i] < msyn_class[3] && synthetic_smaple_1000[i] > msyn_class[2]) {
    S1000_sample[i] <- m_indome[3] + ((m_indome[4]-m_indome[3])/(msyn_class[4]-msyn_class[3]))*(synthetic_smaple_1000[i]-msyn_class[3])
    
  } else if(synthetic_smaple_1000[i] < msyn_class[4] && synthetic_smaple_1000[i] > msyn_class[3]) {
    S1000_sample[i] <- m_indome[4] + ((m_indome[5]-m_indome[4])/(msyn_class[5]-msyn_class[4]))*(synthetic_smaple_1000[i]-msyn_class[4])
    
  } else if(synthetic_smaple_1000[i] < msyn_class[5] && synthetic_smaple_1000[i] > msyn_class[4]) {
    S1000_sample[i] <- m_indome[5] + ((m_indome[6]-m_indome[5])/(mean_class[6]-mean_class[5]))*(synthetic_smaple_1000[i]-msyn_class[5])
    
  } else if(synthetic_smaple_1000[i] < msyn_class[6] && synthetic_smaple_1000[i] > msyn_class[5]) {
    S1000_sample[i] <- m_indome[6] + ((m_indome[7]-m_indome[6])/(msyn_class[7]-msyn_class[6]))*(synthetic_smaple_1000[i]-msyn_class[6])
    
  } else if(synthetic_smaple_1000[i] < msyn_class[7] && synthetic_smaple_1000[i] > msyn_class[6]) {
    S1000_sample[i] <- m_indome[7] + ((m_indome[8]-m_indome[7])/(msyn_class[8]-msyn_class[7]))*(synthetic_smaple_1000[i]-msyn_class[7]) 
    
  } else if(synthetic_smaple_1000[i] < msyn_class[8] && synthetic_smaple_1000[i] > msyn_class[7]) {
    S1000_sample[i] <- m_indome[8] + ((m_indome[9]-m_indome[8])/(msyn_class[9]-msyn_class[8]))*(synthetic_smaple_1000[i]-msyn_class[8]) 

  } else if(synthetic_smaple_1000[i] < msyn_class[9] && synthetic_smaple_1000[i] > msyn_class[8]) {
    S1000_sample[i] <- m_indome[9] + ((m_indome[8]-m_indome[9])/(msyn_class[8]-msyn_class[9]))*(synthetic_smaple_1000[i]-msyn_class[9]) 
  }
}
S1000_sample

#Adjustment for bottom and top
for (i in 1:462) {
  S1000_sample[i] <- (m_indome[1]/msyn_class[1])* synthetic_smaple_1000[i]
}


S1000_upper <-rep(NA, length(synthetic_smaple_1000[]))

for (i in 981:1000) {
  S1000_sample[i] <- (m_indome[9]/msyn_class[9])* synthetic_smaple_1000[i]
}

S1000_sample



##adjustment2:


c1 <- 0
c2 <- 1/2 * (max(S1000_sample[1:462]) + min(S1000_sample[463:562]))
c3 <- 1/2 * (max(S1000_sample[463:562]) + min(S1000_sample[563:646]))
c4 <- 1/2 * (max(S1000_sample[563:646]) + min(S1000_sample[647:744]))
c5 <- 1/2 * (max(S1000_sample[647:744]) + min(S1000_sample[745:855]))
c6 <- 1/2 * (max(S1000_sample[745:855]) + min(S1000_sample[856:910]))
c7 <- 1/2 * (max(S1000_sample[856:910]) + min(S1000_sample[911:960]))
c8 <- 1/2 * (max(S1000_sample[911:960]) + min(S1000_sample[961:980]))
c9 <- 1/2 * (max(S1000_sample[961:980]) + min(S1000_sample[981:1000]))

c_k <- c(c1, c2, c3, c4, c5, c6, c7, c8, c9)
c_k

x_star <- rep(NA, 1000)
m <- 9

for (i in 1:462) {
  if (m_indome[1] > msyn_class[1]) {
    x_star[i] <- c_k[2] - (c_k[2]-m_indome[1])/(c_k[2]-msyn_class[1])*(c_k[2]- S1000_sample[i])
  } else {
    x_star[i] <- c_k[1] + (m_indome[1]-c_k[1])/(msyn_class[1]-c_k[1])*(S1000_sample[i]-c_k[1])
  }
}

for(i in 463:562) {
  if(m_indome[2] > msyn_class[2]) {
    x_star[i] <- c_k[3] - (c_k[3]-m_indome[2])/(c_k[3]-msyn_class[2])*(c_k[3]- S1000_sample[i])
  } else {
    x_star[i] <- c_k[2] + (m_indome[2]-c_k[2])/(msyn_class[2]-c_k[2])*(S1000_sample[i]-c_k[2])
  }
  
} 
for(i in 563:646) {
  if(m_indome[3] > msyn_class[3]) {
    x_star[i] <- c_k[4] - (c_k[4]-m_indome[3])/(c_k[4]-msyn_class[3])*(c_k[4]- S1000_sample[i])
  } else {
    x_star[i] <- c_k[3] + (m_indome[3]-c_k[3])/(msyn_class[3]-c_k[3])*(S1000_sample[i]-c_k[3])
  }
  
} 
for (i in 647:744) {
  if(m_indome[4] > msyn_class[4]) {
    x_star[i] <- c_k[5] - (c_k[5]-m_indome[4])/(c_k[5]-msyn_class[4])*(c_k[5]- S1000_sample[i])
  } else {
    x_star[i] <- c_k[4] + (m_indome[4]-c_k[4])/(msyn_class[4]-c_k[4])*(S1000_sample[i]-c_k[4])
  }
}
for (i in 745:855) {
  if(m_indome[5] > msyn_class[5]) {
    x_star[i] <- c_k[6] - (c_k[6]-m_indome[5])/(c_k[6]-msyn_class[5])*(c_k[6]- S1000_sample[i])
  } else {
    x_star[i] <- c_k[5] + (m_indome[5]-c_k[5])/(msyn_class[5]-c_k[5])*(S1000_sample[i]-c_k[5])
  }
}
for (i in 856:910) {
  if(m_indome[6] > msyn_class[6]) {
    x_star[i] <- c_k[7] - (c_k[7]-m_indome[6])/(c_k[7]-msyn_class[6])*(c_k[7]- S1000_sample[i])
  } else {
    x_star[i] <- c_k[6] + (m_indome[6]-c_k[6])/(msyn_class[6]-c_k[6])*(S1000_sample[i]-c_k[6])
  }
}
for (i in 911:960) {
  if(m_indome[7] > msyn_class[7]) {
    x_star[i] <- c_k[8] - (c_k[8]-m_indome[7])/(c_k[8]-msyn_class[7])*(c_k[8]- S1000_sample[i])
  } else {
    x_star[i] <- c_k[7] + (m_indome[7]-c_k[7])/(msyn_class[7]-c_k[7])*(S1000_sample[i]-c_k[7])
  }
}
for (i in 961:980) {
  if(m_indome[8] > msyn_class[8]) {
    x_star[i] <- c_k[9] - (c_k[9]-m_indome[8])/(c_k[9]-msyn_class[8])*(c_k[9]- S1000_sample[i])
  } else {
    x_star[i] <- c_k[8] + (m_indome[8]-c_k[8])/(msyn_class[8]-c_k[8])*(S1000_sample[i]-c_k[8])
  }
}
for (i in 981:1000) {
  if(m_indome[9] > msyn_class[9]) {
    x_star[i] <- c_k[9+1] - (c_k[9+1]-m_indome[9])/(c_k[9+1]-msyn_class[9])*(c_k[9+1]- S1000_sample[i])
  } else {
    x_star[i] <- c_k[9] + (m_indome[9]-c_k[9])/(msyn_class[9]-c_k[9])*(S1000_sample[i]-c_k[9])
  }
  
}

x_star

library(ineq)
ineq(synthetic_smaple_1000, type = "Gini")#0.6597737
ineq(x_star, type = "Gini") ##0.6797753
ineq(income3, type = "Gini")##0.670094
min(income3)##

##Synthetic sample generated by using Singh-Maddala distribution
library(VGAM)
qsinmad(seq(0.0005, 0.9995, by = 0.001))
?qsinmad






#################Evaluation
sample(x = income3, size = 1000, replace = F)



