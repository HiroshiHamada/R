# select with while ################
select <- function(n, r){
  applicants <- runif(n) # n人を数値化
  candidate <- max(applicants[1:r]) #r人までの暫定1位
  if(candidate == max(applicants)){
    selected <- 0
  }#最初のr人に1位が入ればreturnが0になる
  s <- r + 1 #whileループ用カウンタ
  if(applicants[s] > candidate) {
    selected <- applicants[s] #サーチ最初で超えたら代入
  } else {
    selected <- 0
    while(candidate != max(applicants) & applicants[s] < candidate & s<=n){
      s <- s+1
      selected <- applicants[s]# candidateよりもいい人をselectedに記録
    }#while ends
  }#if else ends
  if(selected == max(applicants)){1}else{0}# selectedの最初が1位か判定
}#return is 0 or 1


select(10,3)

mean(replicate(1000,select(10,3)))




















# select with while ##########################
#一応動くが可読性が悪い
select100 <- function(n, r){
  applicants <- runif(n) # n人を数値化
  candidate <- max(applicants[1:r]) #r人までの暫定1位
  if(candidate == max(applicants)){
    selected <- 0
    0 #最初のr人に1位が入れば終了
  }else{
    s <- r + 1 #サーチ開始位置
    if(applicants[s] > candidate) {
      selected <- applicants[s] 
    }else{
      selected <- 0
      while(applicants[s] < candidate & s<=n){
        s <- s+1
        selected <- applicants[s]# candidateよりもいい人をselectedに記録
      }
    }#else ends
  }
  if(selected[1] == max(applicants)){1}else{0}# selectedの最初が1位か判定
}#return is 0 or 1

select(10,3)

mean(replicate(1000,select(100,37)))

hist(
  replicate(100, mean(replicate(1000,select(1000,368))))
)

# select with for  #######
select <- function(n, r){
	applicants <- runif(n) # n人を数値化
	candidate <- max(applicants[1:r]) #r人までの暫定1位
	if(candidate == max(applicants)){
		0 #最初のr人に1位が入れば終了
	}else{
		selected <- c()
		s <- r + 1 #サーチ開始位置
		for(i in s:n){
			if(candidate < applicants[i]){
				selected <- append(selected, applicants[i])
			}# candidateよりもいい人をselectedに記録
		}# for roop ends here 
		if(selected[1] == max(applicants)){1}else{0}
		# selectedの最初が1位か判定
	}
} #return is 0 or 1

hist(
	replicate(100, mean(replicate(1000,select(100,37))))
)




# select.print with while  ################
select.print <- function(n, r){
  applicants <- runif(n) # n人を数値化
  print(c("applicants=", applicants))
  candidate <- max(applicants[1:r]) #r人までの暫定1位
  print(paste("candidate=", candidate))
    if(candidate == max(applicants)){
      selected <- 0
    }#最初のr人に1位が入ればreturnが0になる
  s <- r + 1 #whileループ用カウンタ
  if(applicants[s] > candidate) {
      selected <- applicants[s] #サーチ最初で超えたら代入
    } else {
      selected <- 0
      while(candidate != max(applicants) & applicants[s] < candidate & s<=n){
          s <- s+1
          selected <- applicants[s]# candidateよりもいい人をselectedに記録
      }#while ends
  }#if else ends
  print(paste0("selected=",selected))
  if(selected == max(applicants)){1}else{0}# selectedの最初が1位か判定
  }#return is 0 or 1

select.print(10,3)



# while proto type ##################

a <- c(1,3,5,7,1,2,3,2,1,6,8,2,4)
b <- 5
i <- 4
if(a[i] > b) {selected <- a[i] }else{selected <- 0}
while(a[i] < b){
  i <- i+1
  selected <- a[i]
  print(selected)
  }
selected



select002 <- function(n, r){
	applicants <- runif(n) # n人を数値化
	#print(c("applicants=",applicants))
	candidate <- max(applicants[1:r]) #r人までの暫定1位
	#print(paste0("candidate= ",candidate))
	if(candidate == max(applicants)){
		0 #最初のr人に1位が入れば終了
	}else{
		selected <- c()
		s <- r+1 
		for(i in s:n){
			if(candidate < applicants[i]) selected <- append(selected, applicants[i])
		}
		#print(paste0("selected= ", selected))
		if(selected[1] == max(applicants)){1}else{0}
	}
}

select002(10,3)


mean(replicate(1000,select002(10,3)))


# prototype #######
select002.print <- function(n, r){
	applicants <- runif(n) # n人の候補者を数値化
	print(c("applicants=",applicants))
	candidate <- max(applicants[1:r]) #r人までのベストを記録
	print(paste0("candidate= ",candidate))
	if(candidate == max(applicants)){
		0
	}else{
		selected <- c()
		k <- r+1
		for(i in k:n){
			if(candidate < applicants[i]) selected <- append(selected, applicants[i])
		}
		print(paste0("selected= ", selected))
		if(selected[1] == max(applicants)){1}else{0}
	}
}

select002.print(10,3)


# applicants 固定#
select001.print <- function(n, r){
	applicants <- c(1,8,6,10,4,5,2,3,7,9) # n人の候補者を数値化
	print(c("applicants=",applicants))
	candidate <- max(applicants[1:r]) #r人までのベストを記録
	print(paste0("candidate= ",candidate))
	if(candidate == max(applicants)){
		0
		}else{
	selected <- c()
	k <- r+1
	for(i in k:n){
		if(candidate < applicants[i]) selected <- append(selected, applicants[i])
	}
	print(paste0("selected= ", selected))
	if(selected[1] == max(applicants)){1}else{0}
		}
}

select001.print(10,3)







select001.print <- function(n, r){
	applicants <- c(1,8,2,5,4,9,10,3,7,6) # n人の候補者を数値化
	print(c("applicants=",applicants))
	candidate <- max(applicants[1:r]) #r人までのベストを記録
	print(paste0("candidate= ",candidate))
	selected <- 0
	s = r + 1
	while(applicants[s] < candidate &  s <= n ){
			selected <- applicants[s]
			print(paste("selected= ", selected))
			s <- s + 1
	}
	print(paste("after.selected= ", selected))
  if(selected == max(applicants)){1}else{0}
  }

select001.print(10,3)


	
	
	selected <- 0 #r人以降で candidate を超えた数値を格納する変数. 初期値は0
	s <- r + 1 #カウンタ用変数の定義．
	if(applicants[s] > candidate){
		selected <- applicants[s]
	}else{
		while() {   #条件を満たす限りループ
			selected <- applicants[s]
			print(paste0("selected= ",selected, "s=", s))
			s <- s + 1
		}# applicants[s]>candidate でループ終了. 
	}# selectedには最初のapplicants[s]>candidateを満たすapplicants[s] が格納される
	print(paste0("after.while.selected= ",selected))
	if(selected == max(applicants)) {1} else {0}  #selectedがmax(applicants)に一致するかどうか判定
} #関数定義終了


select001.print(10,3)





select001.print <- function(n, r){
	applicants <- c(1,10,2,9,8,3,7,5,6,4) # n人の候補者を数値化
	print(c("applicants=",applicants))
	candidate <- max(applicants[1:r]) #r人までのベストを記録
	print(paste0("candidate= ",candidate))
	selected <- 0 #r人以降で candidate を超えた数値を格納する変数. 初期値は0
	s <- r + 1 #カウンタ用変数の定義．
	if(applicants[s] > candidate){
		selected <- applicants[s]
	}else{
	while(applicants[s] < candidate &  s<n ) {   #条件を満たす限りループ
		selected <- applicants[s]
		print(paste0("selected= ",selected, "s=", s))
		s <- s + 1
	}# applicants[s]>candidate でループ終了. 
}# selectedには最初のapplicants[s]>candidateを満たすapplicants[s] が格納される
print(paste0("after.while.selected= ",selected))
if(selected == max(applicants)) {1} else {0}  #selectedがmax(applicants)に一致するかどうか判定
} #関数定義終了


select001.print(10,3)





select0.print <- function(n, r){
  applicants = runif(n); # n人の候補者を数値化
  print(c("applicants=",applicants))
  candidate = max(applicants[1:r]) #r人までのベストを記録
  print(paste0("candidate= ",candidate))
  selected=0    #r人以降で candidate を超えた数値を格納する変数. 初期値は0
  s=r+1 ; #カウンタ用変数の定義．
  if(applicants[s] < candidate &  s<n ) {   #条件を満たす限りループ
    selected=applicants[s]
  }else{
    selected=applicants[s]
  }
    s = s+1;  # r+1人目からサーチ
    print(paste0("selected= ",selected))
  } # applicants[s]>candidate でループ終了. 
  # selectedには最初のapplicants[s]>candidateを満たすapplicants[s] が格納される
  selected=applicants[s]
  print(paste0("selected= ",selected))
  if(selected == max(applicants)) {1} else {0}  #selectedがmax(applicants)に一致するかどうか判定
}; #関数定義終了


select0.print(10,3)







select.print <- function(n, r){
  applicants = runif(n); # n人の候補者を数値化
  print(c("applicants=",applicants))
  candidate = max(applicants[1:r]) #r人までのベストを記録
  print(paste0("candidate= ",candidate))
  selected=0    #r人以降で candidate を超えた数値を格納する変数. 初期値は0
  s=r ; #カウンタ用変数の定義．
  while(applicants[s] < candidate &  s<n ) {   #条件を満たす限りループ
    s = s+1;  # r+1人目からサーチ
    selected=applicants[s]
    print(paste0("selected= ",selected))
  } # applicants[s]>candidate でループ終了. 
  # selectedには最初のapplicants[s]>candidateを満たすapplicants[s] が格納される
  print(paste0("selected= ",selected))
  if(selected == max(applicants)) {1} else {0}  #selectedがmax(applicants)に一致するかどうか判定
}; #関数定義終了


select.print(10,3)





mean(replicate(1000,select(10,3)))
               
          

# prototype  ##################
sec0.print<-function(n,k){
  x=runif(n)# n人の得点ベクトル
  print(x)
  b=max(x[1:k]);#k人までのベストを記録
  print(paste0("暫定1位",b))
  x1=0; #k+1人以降にbを超える人がいたら格納する変数
  #いない場合もあるので初期値を0に設定する
  s=k+1; #カウンタ用変数の定義
  while(x[s] < b &  s<n ) {
    x1=x[s]
    s=s+1
    }
  if(x1==max(x)){1}else{0}#x1が真のベストに一致するかどうか判定
}


sec0.print(10,3)

mean(replicate(1000,sec0(10,3)))


sec0<-function(n, r-1){
  candidates=runif(n);# n 人の得点ベクトル
  no1_temp=max(candidates[1:r-1]);#r-1 人までのベストを記録
  selected=0; #r-1+1 人以降にno1_temp を超える人がいたら格納する変数. 初期値は0
  s=r-1+1; #カウンタ用変数の定義．r-1+1 人目からサーチ
  while(candidates[s]<no1_temp & s<n ) { #条件を満たす限りループ
  s=s+1;selected=candidates[s]
  } # candidates[s]>no1_temp でループ終了.
  # ここでカウンタs は代入の前に進める．
  # selected には最初のcandidates[s]>no1_temp を満たすcandidates[s] が格納
  print(c("no1_temp=","selected=","max(candidates)="))
  print(c(no1_temp, selected, max(candidates)))
  if(selected==max(candidates)){1}else{0} #selectedがmax(candidates)に一致するかどうか判定
};

sec0(10,4)

