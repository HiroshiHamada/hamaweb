#Marloff2012

#############################################
# ch1
#############################################

#関数は複数同時に適応できる
mean(abs(rnorm(100)))

#上と動作は一緒
x=rnorm(100)
y=abs(x)
mean(y)


mean(rnorm(1000))
hist(rnorm(1000))

#関数や変数を別のファイルに定義しておき，source()で呼び出せる．

source("rnorm1000.R")
x1()
x

#変数を空にする
remove(x)
remove(x1)

#data呼び出し　Nileは1変数ベクトル
data(Nile)

head(Nile)

#breaks でbinの数を調整
hist(Nile,breaks=20)


#########################################
### 関数

# counts the number of odd integers in x

oddcount <- function(x) {
   k <- 0 # assign 0 to k
   for (n in x) {
   if (n %% 2 == 1) k <- k+1 # %% is the modulo operator
   }
   return(k)
}

#実行例
oddcount(c(1,3,5))
oddcount(1:50)


#グローバルとローカル変数
#It’s very important to note that the formal parameters in an R function
#are local variables. Suppose we make the following function call:

z <- c(2,6,7)
oddcount(z)

#関数oddcount内のローカル変数xはzで置き換わる


#関数の外で定義されるとグローバル変数として，参照される  
 f <- function(x) return(x+y)
 y <- 3　# Here y is a global variable.
 f(5)

#A global variable can be written to from within a function by using R’s
#superassignment operator, <<-. This is also discussed in Chapter 7.

#関数定義時に初期値を設定することもできる
#R also makes frequent use of default arguments. Consider a function definition

  g <- function(x,y=2,z=T) { ... }

#Here y will be initialized to 2 if the programmer does not specify y in the call.
#Similarly, z will have the default value TRUE. Now consider this call:

  g(12,z=FALSE)

#Here, the value 12 is the actual argument for x, and we accept the default
#value of 2 for y, but we override the default for z, setting its value to FALSE.
#The preceding example also demonstrates that, like many programming
#languages, R has a Boolean type; that is, logical values TRUE and FALSE.

#TRUE and FALSEは文字modeではない．論理modeである.
#  modeはいわゆる変数の型である
  mode(TRUE)

#文字列

#行列

#リスト. 異なる型を入れることができる
  x <- list(u=2, v="abc")
  x
  
  #listのmodeは当然listである
  mode(x)  

  #一方，ただのベクトルの型はnumericである．
  x=c(1:5)
  mode(x)
  
  #hist関数のoutputを確認
  print(hist(Nile))
  
  #strはオブジェクトの内部ストラクチャを返す
  str(hist(Nile))
    

  #data.frame はlistの一種である.　modeはlistである
    d <- data.frame(list(kids=c("Jack","Jill"),ages=c(12,10)))
  
  str(d)
  mode(d)
  
  #クラス. とりあえずそういう属性があることを知る．
  print(hist(Nile))
  summary(hist(Nile))
  
  examsquiz <- read.table("ExamsQuiz.txt",header=FALSE) 
  
  dat0=data.frame(swiss)#data行列の定義
  head(dat0)#中身確認
  
  lma <- lm(dat0$Fertility ~ dat0$Education)
  attributes(lma)
  
  str(lma) #lmクラスの成分を表示
  
  lma$coefficients #係数だけとりだす

    lma$coeff #省略可　
  
  print(lma)
  
  summary(lma) #よく使うジェネリック関数．
  
  
  #example関数 heplで呼び出すより，これを使った方がよい．

#関数seqの内容を知りたい時
  
  ?seq
  
  example(seq)
    
  
  
  
  
  #############################################
  # ch2
  #############################################
  
  
  