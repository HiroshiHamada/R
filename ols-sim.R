library(MASS)

#説明変数が2個の場合
#説明変数は基準化
omit2<-function(n,sd,r,b1,b2){
  mu  <- c(0,0)  #平均ベクトルの定義
  Sigma <- matrix(c(1, r, r, 1), 2, 2)#分散共分散行列の定義
  exv<-mvrnorm(n, mu, Sigma)#説明変数用 乱数n個を生成
  er<-rnorm(n, mean = 0, sd)#攪乱項の生成
  y<- b1*exv[,1]+ b2*exv[,2]+er #真関数によるデータ生成
  data1<-data.frame(x1=exv[,1],x2=exv[,2],y)#data.frameを使って人工データを定義
  out1<-lm(y~x1,data=data1)#欠落変数x2によるols
  out2<-lm(y~x2,data=data1)#欠落変数x1によるols
  out12<-lm(y~x1+x2,data=data1)#欠落変数がない推定
  print(summary(out1))#強制出力
  print(summary(out2))
  print(summary(out12))
  print(cor(exv[,1],exv[,2]))#相関係数確認
}


# 欠落変数バイアスの確認
# 引数 
# omit2(n,sd,r,b1,b2)
omit2(1000,5,0.3,10,10)




#関数truef*は真のデータ分布を発生させるための関数
#攪乱項はdata.frameの定義の際追加

truef1<-function(x1,x2,x3){20 + 10*x1 + 5*x2 + (-3)*x3   }

truef2<-function(x1,x2,x3){20+10*x1*x2*x3}

truef3<-function(x1,x2,x3){
  20+10*x1*sqrt(x2)*(x3^3)  }

truef4<-function(x1,x2,x3){
  20 + 10*x1 + 5*x2 + 5*x3  -10*x1*x3  }#交互作用項

truef5<-function(x1,x2,x3){ 
  20 + 5*x2 -10*x1*x3}#実は主効果なし


#確認用
truef1(1,1,1)
truef2(1,1,1)
truef3(1,1,1)



#人工データ格納から強制olsを実行する関数
# ndはサンプルサイズ，kは真関数の番号 
dgene<-function(nd,k){
  v1<-ceiling(10*runif(nd))#1から10の値をランダムに発生
  v2<-ceiling(10*runif(nd))#最小値を0，最大値を1にするため10倍
  v3<-ceiling(10*runif(nd))
  #optionで真の関数を選択,相互排反なのでif並列
  if(k==1) {y1<-truef1(v1,v2,v3)}#関数truef*の引数にvector v1_3を渡す
  if(k==2) {y1<-truef2(v1,v2,v3)}# 関数はlistableなのでOK
  if(k==3) {y1<-truef3(v1,v2,v3)}#k==*で条件分岐
  if(k==4) {y1<-truef4(v1,v2,v3)}#y1は説明すべき人工データ
  if(k==5) {y1<-truef5(v1,v2,v3)}
  er<-rnorm(nd, mean = 0, sd = 1)#攪乱項の生成
  y1<-y1+er #攪乱項の追加
  data1<-data.frame(x1=v1,x2=v2,x3=v3,y=y1)#data.frameを使って人工データを定義
  out<-lm(y~x1+x2+x3+x1*x3,data=data1)#強制的にols
  summary(out)
}

dgene(1000,5)



