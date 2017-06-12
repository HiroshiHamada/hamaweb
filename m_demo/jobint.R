#x1,x2の二次元正規分布，
#x1は生産能力，x2語学力

library(MASS)#多変量正規分布用パッケージの読み込み

g1<-function(n,x,m1,m2,s11,s22,p){
  mu    <- c(m1, m2)  #平均ベクトル定義
  s12<-p*sqrt(s11)*sqrt(s22)#共分散s12を相関p,分散s11,s22で定義
  Sigma <- matrix(c(s11,s12,s12,s22), 2, 2)#分散共分散行列の定義
  #p=(s12)/(sqrt(s11)*sqrt(s_2))
  exv<-mvrnorm(n, mu, Sigma)#二次元正規乱数n個を生成
  print(cor(exv[,1],exv[,2]))#サンプルの相関確認
  d1<-data.frame(x1=exv[,1],x2=exv[,2])#並替用にデータフレームに格納
  print(head(d1))#並替前のデータ確認
  d2<-d1[order(d1$x2, decreasing = TRUE), ]  #語学成績順で並び替え
  print(head(d2))#並替後のデータ確認
  d2<-head(d2$x1, n=floor(n*x)) #上位x%の抽出
  print(sort(d2))#選抜後のデータ確認
  d1<-d1[order(d1$x1, decreasing = TRUE), ] #生産能力順並び替え
  d1<-head(d1$x1, n=floor(n*x)) #上位x%の抽出
  print(sort(d1))#選抜後のデータ確認
  t.test(d1,d2,paired=T)#平均値の差の検定
}
  
g1(300,0.1,50,50,5,5,0.7)
