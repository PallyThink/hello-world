y=c(2*165,2*198)
n=c(200*2,2*250)
prop.test(y,n,alternative = "two.sided",conf.level = 0.95)
# chua du co so de bac bo H0
# khong co su khac biet ve viec su dung dai an toan o HCM va HN
prop.test(y,n,alternative = "two.sided",conf.level = 0.99)
# p-value = 0.4468 > 0.01
# chua du co so de bac bo H0
# suy ra khong co su khac biet ve viec su dung dai an toan o HCM va HN
prop.test(y,n,alternative = "two.sided",conf.level = 0.99)
prop.test(y,n,alternative = "two.sided",conf.level = 0.95)
# p-value = 0.2457 > alpha(0.01,0.05) -> chua du co so de bac bo H0



#bai 1 KD1
df1=read.csv(file.choose())
df1
# H0 : duong kinh thep tao ra boi hai may la nhu nhau
# H1 : duong kinh thep tao ra boi hai may la khac nhau
result=t.test(df1$extru.ma.1,df1$extru.ma.2,alternative = "two.sided",conf.level = 0.95)
# p-value < 2.2*10^-16 < 0.05 -> bac bo H0
#b
result$p.value
#c
result$conf.int


#bai 3 KD1
df2=read.csv(file.choose())
df2
# H0 thu nhap cua nhan vien giua 2 thanh pho nhu nhau
# H1 thu nhap cua nhan vien giua 2 thanh pho khac nhau
t.test(df2$HCM,df2$HaNoi,alternative = "two.sided",conf.level = 0.9)
# p_value = 0.1692 > 0.1 = alpha -> vay chua du co so de bac bo H0 
#b
result2=t.test(df2$HCM,df2$HaNoi,alternative = "two.sided", conf.level = 0.96)
# khoang tin cay 96% cho su sai khac ve muc luong trung binh cua nhan vien trong nganh cntt tai 2 thanh pho
result2$conf.int

hcm=df2$HCM[df2$HCM>11.5]
hn=df2$HaNoi[df2$HaNoi>11.5]
# H0 : thu nhap cua nhan vien o HCM va HN la nhu nhau
# H1 : thu nhap cua nhan vien o HCM lon hon HN
result3=t.test(hcm,hn,conf.level = 1-0.025,alternative = "greater")
result3$p.value
# p_value = 1.157*10^-10 < 0.025 -> bac bo H0. Vay thu nhap cua nhan vien o HCM lon hon HN


#############################################################
# File tensile-strength.txt chua cac gia tri ve do dan hoi cua cac bao bi giay
# khi duoc xu ly them vao bot go cung trong nguyen lieu voi 4 muc khac nhau

# Doc file du lieu
dat <- read.table(file.choose(), header = T) # dat la file du lieu dinh dang data.frame

# y: bien chua do dan hoi cua cac bao bi
# group: bien nhom gom 4 nhan to tuong ung voi cac muc xu ly 5% (= 1), 10% (= 2), 15% (= 3) va 15% (= 4)
#dat$group <- factor(dat$group)

# CHU Y / ATTENTION !: truoc khi chay phan tich phuong sai
# ta can kiem tra xem bien group da duoc dinh danh theo kieu factor hay chua? 

is.factor(dat$group) # Ket qua = False tuc la bien group khong phai la kieu nhan to (factor)

dat$group <- factor(dat$group)

#Graphical and check the assumptions: 
boxplot(y ~ group, col = c(2,3,4,5), data = dat)


#Kiem tra phan phoi chuan
qqnorm(dat$y, col = 'red', pch = 16)
qqline(dat$y)

library(gplots)
plotmeans(y ~ group, data = dat)

#Run ANOVA
fit1 <- aov(y ~ group, data = dat)
fit1
summary(fit1)
#layout(matrix(c(1,2,3,4),2,2)) # optional layout 
#plot(fit1) # diagnostic plots

#Multiple comparison
pairwise.t.test(dat$y, dat$group, 'none')
TukeyHSD(fit1)

#Comparison function
p.table = function(x, g, p.adjust.method = "none", ..., level = 0.05) {
  ## fill out p-value table
  p = pairwise.t.test(x, g, p.adjust.method, ...)$p.value
  p[is.na(p)] = 0
  p = rbind(0, cbind(p, 0))
  p = p + t(p)
  ## 0 = no difference, 1 = difference
  p = 1 * (p <= level)
  diag(p) = 0
  ## get means and find their order
  m = tapply(x, g, mean)
  o = order(m)
  p = p[o, o]
  dimnames(p) = list(names(m[o]), names(m[o]))
  cbind(mean = m[o], p)
}

p.table(dat$y,dat$group,"none",0.05)
