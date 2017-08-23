# select with while ################

# nは候補者全体数．rは観察数
# アルゴリズムは以下の通り
# r人までは選ばず最大値だけ記憶する，
# r+1人目以降にその最大値を超えた最初の人を選ぶ


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


#動作確認
select(10,3)

# 1000回繰り返して確率を計算
mean(replicate(1000,select(10,3)))

mean(replicate(1000,select(100,37)))

# 平均（成功確率）の分布
hist(
  replicate(100, mean(replicate(1000,select(1000,368))))
)



# select with for  #######
# while ではなくfor文を使うときこちらを使う．
# candidateを超えた2人目などの変則的なアルゴリズムの確認用

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
		# selected[2] などとすれば2人目を選ぶ
		#　ただし2人目以降が空の場合もあるので要調整
	}
} #return is 0 or 1

hist(
	replicate(100, mean(replicate(1000,select(100,37))))
)



# select.print with while  ################
# printデバッグ用

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
# while の動作確認

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
