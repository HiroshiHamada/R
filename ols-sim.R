library(MASS)

#説明変数が2個の場合
#説明変数は基準化
omit<-function(n,sd,r,b1,b2){
  mu  <- c(0,0)  #平均ベクトルの定義
  Sigma <- matrix(c(1, r, r, 1), 2, 2)#分散共分散行列の定義
  exv<-mvrnorm(n, mu, Sigma)#説明変数用 乱数n個を生成
  er<-rnorm(n, mean = 0, sd)#攪乱項の生成
  y<- b1*exv[,1]+ b2*exv[,2]+er #真関数によるデータ生成
  data1<-data.frame(x1=exv[,1],x2=exv[,2],y)#data.frameを使って人工データを定義
  out1<-lm(y~x1,data=data1)#欠落変数x2によるols
  out2<-lm(y~x2,data=data1)#欠落変数x1によるols
  out12<-lm(y~x1+x2,data=data1)#欠落変数がない推定
  print("説明変数x2が欠落した場合の推定")
  print(summary(out1))#強制出力
  print("説明変数x1が欠落した場合の推定")
  print(summary(out2))
  print("欠落のない推定")
    print(summary(out12))
  print(cor(exv[,1],exv[,2]))#相関係数確認
}


# 欠落変数バイアスの確認
# 引数 
# omit2(n,sd,r,b1,b2)
omit(1000,5,0.3,10,10)




#関数truef*は人工データを発生させるための関数
#攪乱項はdata.frameの定義の際に追加する

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




