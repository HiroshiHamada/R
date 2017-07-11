#install.packages("rjags", dependencies=TRUE)
#install.packages("runjags", dependencies=TRUE)
#install.packages("ggplot2", dependencies=TRUE)
#install.packages("R2jags", dependencies=TRUE)


# ver.110 runjags で並列化してみる
# runjagsだとjagsmodelのoutputがないため，現時点ではWAICを計算できない．

graphics.off()
rm(list=ls(all=TRUE))


###################################################
# THE DATA.
source("ssp2015data.R")#データのリコードはこのファイルに集約
#source("ssp2015data_male.R")#男性のみデータ

# head(data) 確認

##### Run JAGS #################################

# The model 
# modelstrings,parametersの指定はsource("gmi-jags-model***.R")で読み込む
# switch関数の引数numberでモデルを切り替える

number="22" #この数字でjagsモデルを選択

switch(number,
       "0"=source("gmi-jags-model0.R"),##base用  \mu,\sigmaをあてはめ推定．
       "1"=source("gmi-jags-model1.R"),##GMI　による "p","b","n","m","s"
       "2"=source("gmi-jags-model2.R"),#年齢jでnを階層化．p,bを逆ロジットで教育年数に回帰
       "20"=source("gmi-jags-model20.R"),# model20: mを教育年数と年齢に回帰
       "21"=source("gmi-jags-model21.R"),#nj,pi,bi,b0,b1,b2,b3 年齢jでnを階層化．p,b逆ロジット教育年数
       "22"=source("gmi-jags-model22.R"),#ni,pi,bi,b1,b3:p,b逆ロジットで教育年数,n:age
       "3"=source("gmi-jags-model3.R"),#age,eduyが説明変数のあてはめglm
       "31"=source("gmi-jags-model3.R"),#age,eduyが説明変数のあてはめglm
       "40"=source("gmi-jags-model40.R"),#年齢別にp,b,n,giniを推定
       "4"=source("gmi-jags-model4.R"),#年齢別にp[45],b[45],n[45]を推定
       "5"=source("gmi-jags-model.R"))

adaptSteps = 500         # 初期値1000 Number of steps to "tune" the samplers.
burnInSteps = 500        # 初期値1000 Number of steps to "burn-in" the samplers.
nChains = 3               # 初期値3 Number of chains to run.
numSavedSteps=5000       # 初期値20000 Total number of steps in chains to save.
thinSteps=1               # Number of steps to "thin" (1=keep every step).
nPerChain = ceiling( ( numSavedSteps * thinSteps ) / nChains ) # Steps per chain.
# Create, initialize, and adapt the model:

require(rjags)

jagsModel = jags.model( "model.txt" , data=dataList ,
                        n.chains=nChains , n.adapt=adaptSteps )
# Burn-in:
  cat( "Burning in the MCMC chain...\n" )
  update( jagsModel , n.iter=burnInSteps )
# The saved MCMC chain:
  cat( "Sampling final MCMC chain...\n" )
  mcmcCoda = coda.samples( jagsModel , variable.names=parameters ,
                            n.iter=nPerChain , thin=thinSteps )

codamenu()  #いろいろ診断結果をみる

mcmcCoda

summary(mcmcCoda)#coda.samplesの中身確認
plot(mcmcCoda)
gelman.diag(mcmcCoda)
  

gelman.diag(mcmcCoda$beta1)




#結果に関するメモ
# model1：n,b,pが収束してない
# Point est. Upper C.I.
# b       3.20       5.96
# m       1.00       1.00
# n       3.67       7.40　
# p       2.93       5.35
# s       1.00       1.01


#########################################################
###  gini index boxplot
mcmcChain = as.matrix( mcmcCoda )
labels.gini <- c()
for(i in 1:45)labels.gini[i] <- paste("gini[",i+19,"]")
labels.gini
boxplot.matrix(mcmcChain[,46:90],names=labels.gini)
dev.copy2pdf(file="gini20-64.pdf")



################################################
# ジニ係数に対する教育年数の限界効果
# 
# Mean       SD  Naive SE Time-series SE
# beta0  1.63696 0.132885 1.879e-03      0.0245135
# beta1  0.05727 0.010010 1.415e-04      0.0018624
# beta2 -0.68907 0.093833 1.327e-03      0.0181981
# beta3  0.04026 0.005528 7.817e-05      0.0008156

beta0 <- 1.63696 
beta1 <- 0.05727
beta2 <- -0.68907
beta3 <- 0.04026 



s[i] <- 1/(n[age[i]]*p[i]*(1-p[i])*(log( (1+b[i])/(1-b[i]) ))^2)
p[i] <- ilogit(beta0+ beta1*eduy[i])
b[i] <- ilogit(beta2+ beta3*eduy[i]) 

2*pnorm(s[i]/sqrt(2), mean = 0, sd = 1)-1





######### 事後分布のboxplot　#################################
############### model 4 output ###############################
labels.b <- c()
for(i in 1:45)labels.b[i] <- paste("b[",i+19,"]")
labels.b
boxplot.matrix(mcmcChain[,1:45],names=labels.b)
dev.copy2pdf(file=paste0(fileNameRoot,"b20-64.pdf"))

labels.p <- c()
for(i in 1:45)labels.p[i] <- paste("p[",i+19,"]")#labels.p
boxplot.matrix(mcmcChain[,91:135],names=labels.p)
dev.copy2pdf(file=paste0(fileNameRoot,"p20-64.pdf"))




mcmcChain[1,91]

46+45

mcmcChain[,46]  

fileNameRoot

############################
# R2jags を使ってR-hatを算出する
library(R2jags)
post.jags <- jags(data = dataList, NULL, parameters,
									n.iter = nPerChain, model.file = "model.txt", n.chains = 3, n.thin = 3, n.burnin = 1000)
print(post.jags) 



##### 事後分布のチェック
# Convert coda-object codaSamples to matrix object for easier handling.
graphics.off()
varnames(mcmcCoda)

parameterNames = varnames(mcmcCoda) # get all parameter names
mcmcChain = as.matrix( mcmcCoda )



######### 事後分布のboxplot　#################################
############### model 4 output ###############################
labels.b <- c()
for(i in 1:45)labels.b[i] <- paste("b[",i+19,"]")
labels.b
boxplot.matrix(mcmcChain[,1:45],names=labels.b)
dev.copy2pdf(file=paste0(fileNameRoot,"b20-64.pdf"))

labels.p <- c()
for(i in 1:45)labels.p[i] <- paste("p[",i+19,"]")#labels.p
boxplot.matrix(mcmcChain[,91:135],names=labels.p)
dev.copy2pdf(file=paste0(fileNameRoot,"p20-64.pdf"))


boxplot.matrix(mcmcChain[,46:90])
dev.copy2pdf(file=paste0(fileNameRoot,"n20-64.pdf"))


############### model 4 output 男性有職者のみ###############################
labels.b <- c()
for(i in 1:45)labels.b[i] <- paste("b[",i+19,"]")
labels.b
boxplot.matrix(mcmcChain[,1:45],names=labels.b)
dev.copy2pdf(file=paste0(fileNameRoot,"b20-64male.pdf"))

labels.p <- c()
for(i in 1:45)labels.p[i] <- paste("p[",i+19,"]")#labels.p
boxplot.matrix(mcmcChain[,91:135],names=labels.p)
dev.copy2pdf(file=paste0(fileNameRoot,"p20-64male.pdf"))

############### model 4 output workerのみ###############################
labels.b <- c()
for(i in 1:45)labels.b[i] <- paste("b[",i+19,"]")
labels.b
boxplot.matrix(mcmcChain[,1:45],names=labels.b)
dev.copy2pdf(file=paste0(fileNameRoot,"b20-64worker.pdf"))

labels.p <- c()
for(i in 1:45)labels.p[i] <- paste("p[",i+19,"]")#labels.p
boxplot.matrix(mcmcChain[,91:135],names=labels.p)
dev.copy2pdf(file=paste0(fileNameRoot,"p20-64worker.pdf"))













########  ggplotでお化粧　　###############################
require(ggplot2)

g<-boxplot.matrix(mcmcChain[,46:90])
g <- g + geom_boxplot(aes(colour = type))
plot(g)

df<-data.frame(mcmcChain[,46:90])
g <- ggplot(df)
g <- g + geom_boxplot(outlier.shape = NA)
g

head(df)


library(reshape2)
library(ggplot2)

# データの読み込み
x <- read.table("https://stat.biopapyrus.net/data/rat.food.txt", header = TRUE)

# データを ggplot2 用に整形します
df <- melt(x)
head(df)
# ggplot2 描画レイヤー
g <- ggplot(df, aes(x = variable, y = value))


# 描画
plot(g)








#パラメータ1--15
for ( parName in parameterNames[1:15] ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName ,
            saveName=fileNameRoot , saveType=graphFileType )}

#mcmcChain[,1]#parameter 1個目

#boxplot.matrix(as.matrix(mcmcChain[,1:15]))






mcmcChain[,1:2]



########### EXAMINE THE RESULTS
# compute DIC
dic.pD <- dic.samples(jagsModel, 1000, "pD") # Deviance Information Criterion
dic.pD 

# compute WAIC  
require(loo)
loglik1 <- sapply(1:N, function(i)
  unlist(mcmcCoda[, paste("loglikelihood[", i, "]", sep = "")]))
waic(loglik1)



#各パラメタの事後分布を1枚のパネルに並べる
openGraph(width=10,height=6)
layout(matrix(1:6,nrow=2,byrow=TRUE))
# b[20代--60代)]までの事後分布
for(i in 1:5){
  plotPost( mcmcChain[,i] , xlab=paste0("b[", i*10+10,"]"))	}
dev.copy2pdf(file=paste0(fileNameRoot,"b20-60.pdf"))

# n[20代--60代)]までの事後分布
openGraph(width=10,height=6)
layout(matrix(1:6,nrow=2,byrow=TRUE))
for(i in 1:5){
  plotPost( mcmcChain[,i+5] , xlab=paste0("n[", i*10+10,"]"))	}
dev.copy2pdf(file=paste0(fileNameRoot,"n20-60.pdf"))

# p[20代--60代)]までの事後分布
openGraph(width=10,height=6)
layout(matrix(1:6,nrow=2,byrow=TRUE))
for(i in 1:5){
  plotPost( mcmcChain[,i+10] , xlab=paste0("p[", i*10+10,"]"))	}
dev.copy2pdf(file=paste0(fileNameRoot,"p20-60.pdf"))











for(i in 16:30){
  plotPost( mcmcChain[,i] , xlab=paste0("b[", i+19,"]"))}
dev.copy2pdf(file=paste0(fileNameRoot,"b16-30-2.pdf"))
for(i in 31:45){
  plotPost( mcmcChain[,i] , xlab=paste0("b[", i+19,"]"))}#15枚ずつ出力
dev.copy2pdf(file=paste0(fileNameRoot,"b31-45-2.pdf"))





hist(unlist(mcmcCoda[, paste("b[", 1, "]", sep = "")]))

# 確認用loo(loglik1)
# unlist(mcmcCoda[, paste("loglikelihood[", 1, "]", sep = "")])
# unlist(mcmcCoda[, paste("loglikelihood[", 8, "]", sep = "")])

# Display diagnostics of chain, for specified parameters:

mcmcCoda[1]

#1-45 b



if(modelnumber==1){
	for ( parName in parameterNames[1:5] ) {
		diagMCMC( codaObject=mcmcCoda , parName=parName ,
							saveName=fileNameRoot , saveType=graphFileType )}}

if(modelnumber==2){
	for ( parName in parameterNames[46:49] ) {#modelnumber2の処理
		diagMCMC( codaObject=mcmcCoda , parName=parName ,
							saveName=fileNameRoot , saveType=graphFileType )}}
# 1-45 までは年齢別の反復回数，46-49がロジットのパラメータ


if(modelnumber==3){
	for ( parName in parameterNames[1:4] ) {#modelnumber3の処理
		diagMCMC( codaObject=mcmcCoda , parName=parName ,
							saveName=fileNameRoot , saveType=graphFileType )}}

if(modelnumber==4){
	for ( parName in parameterNames[1:6] ) {#modelnumber4の処理 数が多いので部分指定
		diagMCMC( codaObject=mcmcCoda , parName=parName ,
							saveName=fileNameRoot , saveType=graphFileType )}#for
	}#if

if(modelnumber==6){
	for ( parName in parameterNames[1:3] ) {#modelnumber4の処理 数が多いので部分指定
		diagMCMC( codaObject=mcmcCoda , parName=parName ,
							saveName=paste0(fileNameRoot,"model",modelnumber,"-"),
							saveType=graphFileType )}#for
}#if


if(modelnumber==7){
	for ( parName in parameterNames[1:5] ) {#modelnumber4の処理 数が多いので部分指定
		diagMCMC( codaObject=mcmcCoda , parName=parName ,
							saveName=fileNameRoot , saveType=graphFileType )}#for
}#if




#diag mcmc を実行すると，plotpostがPANEに出力されない
#diagMCMC内でopenGrpahを呼び出しているため，別windowsが出ているため
#右下には出ないが，下のコマンドでpdf保存される


# Convert coda-object codaSamples to matrix object for easier handling.
mcmcChain = as.matrix( mcmcCoda )
chainLength = NROW(mcmcChain) # 10000個の事後分布サンプル

# for model 1   base model による所得分布の予測と各パラメータ事後分布
if(modelnumber==1){
openGraph(width=10,height=6)
layout(matrix(1:6,nrow=2,byrow=TRUE))
# posterior predictive
hist( dataList$y , xlab="y" , main="Data w. Post. Pred." , breaks=30 ,
      col="pink" , border="white" , prob=TRUE , cex.lab=1.5)
pltIdx = floor(seq(1,chainLength,length=20))
xComb = seq( min(dataList$y) , max(dataList$y) , length=501 )
for ( chnIdx in pltIdx ) {
  lines( xComb ,
         dlnorm( xComb, mcmcChain[chnIdx,"m"], mcmcChain[chnIdx,"s"] ),
         col="skyblue" )}
# param's of log(y)
postInfo = plotPost( mcmcChain[,"m"] , xlab="mu of log(y)" )
postInfo = plotPost( mcmcChain[,"s"] , xlab="sigma of log(y)" )
# param's of y
postInfo = plotPost( mcmcChain[,"modeOfY"] , xlab="mode of y" )
postInfo = plotPost( mcmcChain[,"muOfY"] , xlab="mu of y" )
postInfo = plotPost( mcmcChain[,"sigmaOfY"] , xlab="sigma of y" , cenTend="mode")
saveGraph(file=fileNameRoot,type=graphFileType)
}

#head(mcmcChain[,91:135])#n;#head(mcmcChain[,136:181])#p
#head(mcmcChain[,10])#n;#mcmcChain[,10]
# mcmcChain は1列目から45列目まで順にb[1]~b[45] がならぶ
# 46~90:m 91~135:n

openGraph(width=10,height=6)
plotPost(mcmcChain[,1])# b(20才)の事後分布


#各パラメタを15枚ずつ1枚のパネルに並べる
openGraph(width=10,height=6)
layout(matrix(1:15,nrow=3,byrow=TRUE))
# b[1(20)]-b[45(64)]までの事後分布
for(i in 1:15){
	plotPost( mcmcChain[,i] , xlab=paste0("b[", i+19,"]"))	}
dev.copy2pdf(file=paste0(fileNameRoot,"b1-15-2.pdf"))
for(i in 16:30){
	plotPost( mcmcChain[,i] , xlab=paste0("b[", i+19,"]"))}
dev.copy2pdf(file=paste0(fileNameRoot,"b16-30-2.pdf"))
for(i in 31:45){
	plotPost( mcmcChain[,i] , xlab=paste0("b[", i+19,"]"))}#15枚ずつ出力
dev.copy2pdf(file=paste0(fileNameRoot,"b31-45-2.pdf"))

# p[20]~p[64] の事後分布　matrixの中で
openGraph(width=10,height=6)
layout(matrix(1:15,nrow=3,byrow=TRUE))
for(i in 1:15){
	plotPost( mcmcChain[,i+90] , xlab=paste0("p[", i+19,"]"))	}
dev.copy2pdf(file=paste0(fileNameRoot,"p20-34-2.pdf"))
for(i in 16:30){
	plotPost( mcmcChain[,i+90] , xlab=paste0("p[", i+19,"]"))}
dev.copy2pdf(file=paste0(fileNameRoot,"p35-49-2.pdf"))
for(i in 31:45){
	plotPost( mcmcChain[,i+90] , xlab=paste0("p[", i+19,"]"))}#15枚ずつ出力
dev.copy2pdf(file=paste0(fileNameRoot,"p50-64-2.pdf"))

# p[20]~p[64] の事後分布
openGraph(width=10,height=6)
layout(matrix(1:15,nrow=3,byrow=TRUE))
for(i in 1:15){
	plotPost( mcmcChain[,i+135] , xlab=paste0("p[", i+19,"]"))	}
dev.copy2pdf(file=paste0(fileNameRoot,"p20-34-2.pdf"))
for(i in 16:30){
	plotPost( mcmcChain[,i+135] , xlab=paste0("p[", i+19,"]"))}
dev.copy2pdf(file=paste0(fileNameRoot,"p35-49-2.pdf"))
for(i in 31:45){
	plotPost( mcmcChain[,i+135] , xlab=paste0("p[", i+19,"]"))}#15枚ずつ出力
dev.copy2pdf(file=paste0(fileNameRoot,"p50-64.pdf"))



#年齢グループ毎に45の所得分布を予測する

#table(income_ind,age);length(income_ind[age==1])
openGraph(width=10,height=6)
layout(matrix(1:15,nrow=3,byrow=TRUE))

for(i in 1:15){
y.age<-dataList$y[dataList$age==i]
#所得分布の予測
hist( y.age , xlab="y" , main=paste0("age group ", i), breaks=20,
			col="pink" , border="white" , prob=TRUE , cex.lab=1.5)
pltIdx = floor(seq(1,chainLength,length=20))
#MCMC sampleからの代表点
xComb = seq( min(na.omit(y.age)) , max(na.omit(y.age)) , length=51 )
for ( x in pltIdx ) {
	lines( xComb,
				 dlnorm( xComb, mcmcChain[x,paste0("m[",i,"]")], mcmcChain[x,paste0("tau[",i,"]")]),col="skyblue")
				 }}
dev.copy2pdf(file=paste0(fileNameRoot,"age-grouped-income-1.pdf"))

for(i in 16:30){
	y.age<-dataList$y[dataList$age==i]
	#所得分布の予測
	hist( y.age , xlab="y" , main=paste0("age group ", i), breaks=20,
				col="pink" , border="white" , prob=TRUE , cex.lab=1.5)
	pltIdx = floor(seq(1,chainLength,length=20))
	#MCMC sampleからの代表点
	xComb = seq( min(na.omit(y.age)) , max(na.omit(y.age)) , length=51 )
	for ( x in pltIdx ) {
		lines( xComb,
					 dlnorm( xComb, mcmcChain[x,paste0("m[",i,"]")], mcmcChain[x,paste0("tau[",i,"]")]),col="skyblue")
	}
}
dev.copy2pdf(file=paste0(fileNameRoot,"age-grouped-income-2.pdf"))

for(i in 31:45){
	y.age<-dataList$y[dataList$age==i]
	#所得分布の予測
	hist( y.age , xlab="y" , main=paste0("age group ", i), breaks=20,
				col="pink" , border="white" , prob=TRUE , cex.lab=1.5)
	pltIdx = floor(seq(1,chainLength,length=20))
	#MCMC sampleからの代表点
	xComb = seq( min(na.omit(y.age)) , max(na.omit(y.age)) , length=51 )
	for ( x in pltIdx ) {
		lines( xComb,
					 dlnorm( xComb, mcmcChain[x,paste0("m[",i,"]")], mcmcChain[x,paste0("tau[",i,"]")]),col="skyblue")
	}
}
dev.copy2pdf(file=paste0(fileNameRoot,"age-grouped-income-3.pdf"))

###########################################
model 4
Mean deviance:  38707 
penalty 88.53 
Penalized deviance: 38796 




graphics.off()




