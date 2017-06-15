library(MASS)#2次元正規分布用

#関数generator*は人工データを発生させるための関数
#攪乱項はdata.frameの定義の際に追加する

generator1<-function(x){5+6*x}
generator2<-function(x){20+10*x}
generator3<-function(x){20+10*x1*sqrt(x2)*(x3^3)  }
generator4<-function(x){20 + 10*x1 + 5*x2 + 5*x3  -10*x1*x3  }#交互作用項
generator5<-function(x){20 + 5*x2 -10*x1*x3}#実は主効果なし
generator6<-function(x){10 + 5*x1 + 5*x2 - 5*x3+3*x2^2}

# 関数misspecificationの概要：
#人工データをgenerator関数を使って生成．
#その後generator関数と異なる関数で推定する
# ndはサンプルサイズ，generatortypeでswitchを切り替え，
# generator関数を指定する． 

misspecification<-function(nd,generatortype,statmodel){
  v1<-ceiling(10*runif(nd))#1から10の値をランダムに発生
  v2<-ceiling(10*runif(nd))#最小値を0，最大値を1にするため10倍
  v3<-ceiling(10*runif(nd))
  #optionで真の関数を選択,相互排反なのでif並列
  switch(generatortype,
         y1<-generator1(v1,v2,v3),# 関数はlistable
         y1<-generator2(v1,v2,v3),
         y1<-generator3(v1,v2,v3),
         y1<-generator4(v1,v2,v3),
         y1<-generator5(v1,v2,v3),
         y1<-generator6(v1,v2,v3),
  )
  error<-rnorm(nd, mean = 0, sd = 1)#攪乱項の生成
  y1<-y1+error #攪乱項の追加
  data1<-data.frame(x1=v1,x2=v2,x3=v3,x2square=v2^2,y=y1)#data.frameを使って人工データを定義
  switch(statmodel,#推定用の統計モデルを指定
         out<-lm(y~x1+x2+x3,data=data1),
         out<-lm(y~x1+x2+x3+x2square,data=data1),
  )
  switch(generatortype,
         print("generator1 is 20 + 10*x1 + 5*x2 + (-3)*x3"),
         print("generator2 is 20 + 10*x1*x2*x3"),
         print("generator3 is 20 + 10*x1*sqrt(x2)*(x3^3)"),
         print("generator4 is 20 + 10*x1 + 5*x2 + 5*x3 - 10*x1*x3"),
         print("generator5 is 20 + 5*x2 -10*x1*x3"),
         print("generator6 is 10+5*x1+5*x2-5*x3+3*x2^2"),)
  switch(statmodel,
         print("model is y~x1+x2+x3"),
         print("model is y~x1+x2+x3+x2square"),
         print("generator3 is 20 + 10*x1*sqrt(x2)*(x3^3)"),
         print("generator4 is 20 + 10*x1 + 5*x2 + 5*x3 - 10*x1*x3"),
         print("generator5 is 20 + 5*x2 -10*x1*x3"),
         print("generator6 is 10+5*x1+5*x2-5*x3+3*x2^2"),)
  summary(out)
}


misspecification(1000,6,1)





# 説明変数を2つ以上に拡張


generator1<-function(x1,x2,x3){20 + 10*x1 + 5*x2 + (-3)*x3   }
generator2<-function(x1,x2,x3){20+10*x1*x2*x3}
generator3<-function(x1,x2,x3){
  20+10*x1*sqrt(x2)*(x3^3)  }
generator4<-function(x1,x2,x3){
  20 + 10*x1 + 5*x2 + 5*x3  -10*x1*x3  }#交互作用項
generator5<-function(x1,x2,x3){ 
  20 + 5*x2 -10*x1*x3}#実は主効果なし
generator6<-function(x1,x2,x3){ 
  10 + 5*x1 + 5*x2 - 5*x3+3*x2^2}


#確認用
# generator1(1,1,1)
# generator2(1,1,1)
# generator3(1,1,1)


# 関数misspecificationの概要：
#人工データをgenerator関数を使って生成．
#その後generator関数と異なる関数で推定する
# ndはサンプルサイズ，generatortypeでswitchを切り替え，
# generator関数を指定する． 

#testcode

misspecification<-function(nd,generatortype,statmodel){
  v1<-ceiling(10*runif(nd))#1から10の値をランダムに発生
  v2<-ceiling(10*runif(nd))#最小値を0，最大値を1にするため10倍
  v3<-ceiling(10*runif(nd))
  #optionで真の関数を選択,相互排反なのでif並列
  switch(generatortype,
         y1<-generator1(v1,v2,v3),# 関数はlistable
         y1<-generator2(v1,v2,v3),
         y1<-generator3(v1,v2,v3),
         y1<-generator4(v1,v2,v3),
         y1<-generator5(v1,v2,v3),
         y1<-generator6(v1,v2,v3),
  )
  error<-rnorm(nd, mean = 0, sd = 1)#攪乱項の生成
  y1<-y1+error #攪乱項の追加
  data1<-data.frame(x1=v1,x2=v2,x3=v3,x2square=v2^2,y=y1)#data.frameを使って人工データを定義
  switch(statmodel,#推定用の統計モデルを指定
         out<-lm(y~x1+x2+x3,data=data1),
         out<-lm(y~x1+x2+x3+x2square,data=data1),
  )
  switch(generatortype,
         print("generator1 is 20 + 10*x1 + 5*x2 + (-3)*x3"),
         print("generator2 is 20 + 10*x1*x2*x3"),
         print("generator3 is 20 + 10*x1*sqrt(x2)*(x3^3)"),
         print("generator4 is 20 + 10*x1 + 5*x2 + 5*x3 - 10*x1*x3"),
         print("generator5 is 20 + 5*x2 -10*x1*x3"),
         print("generator6 is 10+5*x1+5*x2-5*x3+3*x2^2"),)
  switch(statmodel,
         print("model is y~x1+x2+x3"),
         print("model is y~x1+x2+x3+x2square"),
         print("generator3 is 20 + 10*x1*sqrt(x2)*(x3^3)"),
         print("generator4 is 20 + 10*x1 + 5*x2 + 5*x3 - 10*x1*x3"),
         print("generator5 is 20 + 5*x2 -10*x1*x3"),
         print("generator6 is 10+5*x1+5*x2-5*x3+3*x2^2"),)
  summary(out)
}


misspecification(1000,6,1)












misspecification<-function(nd,generatortype,statmodel){
  v1<-ceiling(10*runif(nd))#1から10の値をランダムに発生
  v2<-ceiling(10*runif(nd))#最小値を0，最大値を1にするため10倍
  v3<-ceiling(10*runif(nd))
  #optionで真の関数を選択,相互排反なのでif並列
  switch(generatortype,
         y1<-generator1(v1,v2,v3),# 関数はlistable
         y1<-generator2(v1,v2,v3),
         y1<-generator3(v1,v2,v3),
         y1<-generator4(v1,v2,v3),
         y1<-generator5(v1,v2,v3),
         y1<-generator6(v1,v2,v3),
  )
  error<-rnorm(nd, mean = 0, sd = 1)#攪乱項の生成
  y1<-y1+error #攪乱項の追加
  data1<-data.frame(x1=v1,x2=v2,x3=v3,x2square=v2^2,y=y1)#data.frameを使って人工データを定義
  switch(statmodel,#推定用の統計モデルを指定
         out<-lm(y~x1+x2+x3,data=data1),
         out<-lm(y~x1+x2+x3+x2square,data=data1),
         )
  switch(generatortype,
         print("generator1 is 20 + 10*x1 + 5*x2 + (-3)*x3"),
         print("generator2 is 20 + 10*x1*x2*x3"),
         print("generator3 is 20 + 10*x1*sqrt(x2)*(x3^3)"),
         print("generator4 is 20 + 10*x1 + 5*x2 + 5*x3 - 10*x1*x3"),
         print("generator5 is 20 + 5*x2 -10*x1*x3"),
         print("generator6 is 10+5*x1+5*x2-5*x3+3*x2^2"),)
  switch(statmodel,
         print("model is y~x1+x2+x3"),
         print("model is y~x1+x2+x3+x2square"),
         print("generator3 is 20 + 10*x1*sqrt(x2)*(x3^3)"),
         print("generator4 is 20 + 10*x1 + 5*x2 + 5*x3 - 10*x1*x3"),
         print("generator5 is 20 + 5*x2 -10*x1*x3"),
         print("generator6 is 10+5*x1+5*x2-5*x3+3*x2^2"),)
  summary(out)
}


misspecification(1000,6,1)


# misspecification<-function(nd,generatortype){
#   v1<-ceiling(10*runif(nd))#1から10の値をランダムに発生
#   v2<-ceiling(10*runif(nd))#最小値を0，最大値を1にするため10倍
#   v3<-ceiling(10*runif(nd))
#   #optionで真の関数を選択,相互排反なのでif並列
#   switch(generatortype,
#          y1<-generator1(v1,v2,v3),# 関数はlistable
#          y1<-generator2(v1,v2,v3),
#          y1<-generator3(v1,v2,v3),
#          y1<-generator4(v1,v2,v3),
#          y1<-generator5(v1,v2,v3),
#          y1<-generator6(v1,v2,v3),
#          )
#   error<-rnorm(nd, mean = 0, sd = 1)#攪乱項の生成
#   y1<-y1+error #攪乱項の追加
#   data1<-data.frame(x1=v1,x2=v2,x3=v3,x2square=v2^2,y=y1)#data.frameを使って人工データを定義
#   out<-lm(y~x1+x2+x3+x2square,data=data1)#わざとgeneratorと異なる関数で推定
#   summary(out)
# }
# 
# misspecification(1000,6)

#欠落変数バイアスの計算．説明変数が2個の場合
#説明変数は基準化

omit<-function(n,sd,r,b1,b2){
  mu  <- c(0,0)  #平均ベクトルの定義
  Sigma <- matrix(c(1, r, r, 1), 2, 2)#分散共分散行列の定義
  exv<-mvrnorm(n, mu, Sigma)#説明変数用 乱数n個を生成
  error<-rnorm(n, mean = 0, sd)#攪乱項の生成
  y<- b1*exv[,1]+ b2*exv[,2]+error #真関数によるデータ生成
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

