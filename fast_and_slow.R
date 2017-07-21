library(MASS)

# ch.17  平均への回帰 ####
## 頭のいい女性は頭の悪い男性と結婚しがち？
r <- 0.7; n <- 500
mu <- c(0,0)  #標準得点に変換済み
mysigma <- matrix(c(1, r, r, 1), 2, 2)
#共分散行列の定義. 平均は0，分散は1，共分散は相関rに等しい
pair <- mvrnorm(n, mu, mysigma)
# 2次元正規分布に従う確率変数の実現値をn個生成．相関はr
# pairはc(女性得点,男性得点)を要素とするn次元ベクトル
plot(pair) #散布図

hist(x) #女性の得点分布確認．
x <- pair[,1];y <- pair[,2]
data <- data.frame(x=x,y=y)#回帰用にdata.frameにキャスト
data$x[data$x>1]#得点1以上の女性
data[data[,1]> 1,] #ペアの様子
data[data[,1]> 1,2] #女性の上位15%とペアの男性データを抽出

print(  c("女性上位15%平均得点=", mean(data$x[data$x>1])   )    )
print(  c("女性上位15%のパートナーの平均得点=", mean(data$y[data$x>1])   )    )

# 回帰確認.P(X>1)=1-pnorm(1)=約0.158655
out <- lm(y ~ x, data=data)
summary(out)

## 体重とピアノの疑似相関　
n <- 100
age <- rnorm(n)
practice <- rnorm(n)
ice <- rnorm(n)

weight <- age+ice
piano <- age+practice

cor(weight,piano)

data <- data.frame(weight=weight,piano=piano,age=age)
summary(lm(piano~weight+age,data=data))

  
n <- 1000
age <- rnorm(n)
practice <- rnorm(n)
ice <- rnorm(n)

p <- 0.7
weight <- p*age + (1-p)*ice
piano <- p*age + (1-p)*practice

cor(weight,piano)







#  ch.19 ceo and company ####

library(MASS)
# for mvrnorm

# 以下のコードは，テキストとは若干異なるが本質的には同じ指数を計算している
d100<-function(r,m1=50,m2=50,s1=5,s2=5,n=2000){
	mu = c(m1, m2)  #平均ベクトルの定義
	sigma = matrix(c(s1^2, r*s1*s2, r*s1*s2, s2^2), 2, 2)#分散共分散行列の定義
	#r=(r*s1*s2)/(s1*s2)
	data = mvrnorm(n, mu, sigma)
	#2次元正規乱数(ceo能力,企業成績)をn個を生成． 
	y=data[data[,1]> m1, 2]	#data行列からceo能力が平均以上の企業を抽出.
	y2=subset(y, y>m2) 
	#さらに業績が平均以上の企業を抽出
	length(y2)/length(y) # 業績が平均以上の企業数/CEO能力が平均以上の企業数
}

mean(rep(d100(0),10))
mean(rep(d100(0.2),10))
mean(rep(d100(0.3),10))
mean(rep(d100(0.4),10))
mean(rep(d100(0.6),10))
mean(rep(d100(0.9999),10))


# 以下なるべくテキストを再現したコードで確率を計算する
d201<-function(r,n=2000){
	mu = c(0, 0)  #平均ベクトルの定義
	sigma = matrix(c(1, r, r, 1), 2, 2)#分散共分散行列の定義
	# 平均0,分散1,相関rに基準化
	data = mvrnorm(n, mu, sigma)
	#2次元正規乱数乱数sn個を生成  相関cor(exv[,1],exv[,2]) 
	#より正確な推定には，乱数発生を反復する必要がある
	#print(data)
	y=data[order(data[,1]),]
	# ceo能力順(data[,1])にdataをsort
	#print(y)
	d <- n/2 #上位下位分岐点
	y1=y[1:d,2]#COE能力下位半分の企業をセレクト
	y2=y[(d+1):n,2]#COE能力上位半分の企業をセレクト
	# print(y1)
	# print(y2)
	out <- c()
	for(i in 1:100){
		result <- sample(y1)<sample(y2)
		out <- append(out,mean(result))# y1,y2をランダムに並びかえて比較
	}
	print(out)
	mean(out)
}

d201(0.2)
d201(0.3)
d201(0.4)
