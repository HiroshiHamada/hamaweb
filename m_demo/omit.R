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

omit2(1000,5,0.3,10,10)

#バイアスが大きくなる引数を探せ

omit2(1000,15,0.3,10,10)
omit2(1000,100,0.3,10,10)


#3次元正規分布乱数を説明変数とするols
#説明変数は基準化
omit3<-function(n,sd,r12,r13,r23,b1,b2,b3){
  mu <- c(0,0,0)  #平均ベクトルの定義
  Sigma <- matrix(c(1,r12,r13,r12,1,r23,r13,r23,1),3,3)
  #分散共分散行列の定義
  exv<-mvrnorm(n, mu, Sigma) #説明変数用 乱数n個を生成
  er<-rnorm(n, mean = 0, sd)#攪乱項の生成
  y<- b1*exv[,1]+ b2*exv[,2]+ b3*exv[,3]+er
  data1<-data.frame(x1=exv[,1],x2=exv[,2],x3=exv[,3],y)#data.frameを使って人工データを定義
  out1<-lm(y~x1,data=data1);out2<-lm(y~x2,data=data1);out3<-lm(y~x3,data=data1)
  out12<-lm(y~x1+x2,data=data1);out13<-lm(y~x1+x3,data=data1)
  out23<-lm(y~x2+x3,data=data1);out123<-lm(y~x1+x2+x3,data=data1)
  print(summary(out1))#推定結果の強制出力
  print(summary(out2));print(summary(out3))
  print(summary(out12));print(summary(out13))
  print(summary(out23));print(summary(out123))
  cor12<-cor(exv[,1],exv[,2]);cor13<-cor(exv[,1],exv[,3])
  cor23<-cor(exv[,2],exv[,3])
  c(cor12,cor13,cor23)#相関係数確認
}

#   n,sd,m1,m2,m3,s1,s2,s3,r12,r23,r13,b1,b2,b3
omit3(1000,10,0.3,0.3,0.3,2,3,5)

omit3(1000,10,0.3,0.3,0.3,2,3,150)







#--------基準化しない場合----------------
  omit<-function(n,sd,m1,m2,s1,s2,r,b1,b2){
    mu    <- c(m1, m2)  #平均ベクトルの定義
    s12 <- r*s1*s2 #共分散を相関から定義
    Sigma <- matrix(c(s1^2, s12, s12, s2^2), 2, 2)#分散共分散行列の定義
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

# n,sd,m1,m2,s1,s2,r,b1,b2
omit(1000,10,5,5,1,1,0.3,5,5)  

# パラメータだけ変化
omit(1000,10,5,5,1,1,0.4,5,-10)  

# 攪乱項の分散だけが変化
omit(1000,100,5,5,1,1,0.4,10,10)  
omit(1000,200,5,5,1,1,0.4,10,10)  

#y,x1が5points Likert scale, x2が所得の場合
# b1>b2となる
omit(1000,1,2.5,10,1,20,0.5,3,0.0005)  

#負の相関がある場合,欠落変数によるバイアスはより深刻
omit(1000,1,2.5,10,1,20,-0.5,3,0.0005)  


omit3<-function(n,sd,m1,m2,m3,s1,s2,s3,r12,r13,r23,b1,b2,b3){
  mu <- c(m1,m2,m3)  #平均ベクトルの定義
  s12 <- r12*s1*s2;s23 <- r23*s2*s3;s13 <- r13*s1*s3 #共分散の定義
  Sigma <- matrix(c(s1^2,s12,s13,s12,s2^2,s23,s13,s23,s3^2),3,3)#分散共分散行列の定義
  exv<-mvrnorm(n, mu, Sigma) #説明変数用 乱数n個を生成
  er<-rnorm(n, mean = 0, sd)#攪乱項の生成
  y<- b1*exv[,1]+ b2*exv[,2]+ b3*exv[,3]+er
  data1<-data.frame(x1=exv[,1],x2=exv[,2],x3=exv[,3],y)#data.frameを使って人工データを定義
  out1<-lm(y~x1,data=data1);out2<-lm(y~x2,data=data1);out3<-lm(y~x3,data=data1)
  out12<-lm(y~x1+x2,data=data1);out13<-lm(y~x1+x3,data=data1)
  out23<-lm(y~x2+x3,data=data1)
  out123<-lm(y~x1+x2+x3,data=data1)
  print(summary(out1))#推定結果の強制出力
  print(summary(out2));print(summary(out3))
  print(summary(out12));print(summary(out13))
  print(summary(out23));print(summary(out123))
  cor12<-cor(exv[,1],exv[,2]);cor13<-cor(exv[,1],exv[,3])
  cor23<-cor(exv[,2],exv[,3])
  c(cor12,cor13,cor23)#相関係数確認
}

