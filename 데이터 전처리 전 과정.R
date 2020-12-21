# 전처리 과정 정리 #
# 편의를 위해 변수표현을 심판,날짜,우승 등등을 공통변수라 표현
# 나이, 키, 팔길이 등을 B또는 R선수의 인적사항
# avg, opp등 누적으로 측정되는 변수들을 공격변수라고 정의 >> 각 선수별로 49개의 변수 존재 > 98개 차지 

setwd('C:/Users/danan/Desktop/Projects/UFC')

df <- read.csv('ufcdata/data.csv')  # 2001년 이후의 데이터 
# 4887행 145열 

col_na <- function(y){
  return(sum(is.na(y)))
}    # 각 열의 결측치 개수를 반환해주는 함수 col_na 만듬 
# 각 열의 결측치 분포를 확인하기 위해서 

df_col_na <- sapply(df,FUN=col_na)
df_col_na
View(df_col_na)  # B선수 공격변수 49개에서 모두 결측치 형태로 나타남 > 1136개의 행에서 
# R선수 공격변수 49개에서 모두 결측치 형태로 나타남 >  554개의 행에서 

# Q. 위와 같은 결측치 형태는 왜 나타난걸까?? #
# A. 공격변수들 특성상 누적되는 형태라서 total_rounds_fought ==0 인 선수들은 그 경기가 처음인 선수임을 알 수 있다. 
#    처음 출전하는 선수들은 누적될게 없으니 공격변수49개가 모두 결측값이다. 
# 객관적인 근거(지표)를 위해 다음 아래의 코드를 실행해보면 확인 가능 
nrow(df[df$B_total_rounds_fought==0,])  # B팀에서 처음 출전한 선수들이 1136명 
nrow(df[df$R_total_rounds_fought==0,])  # R팀에서 처음 출전한 선수들이 554명 
# 1136개의 행과 554개의 행이 모두 처음 출전한 선수 수와 일치 !!
# 나머지 age, height 등등은 단순누락 결측치로 판단 




#------------------------------------------------------------------------------------------------------------------------#
#------------------------------------------------------------------------------------------------------------------------#

# ------------------- 결측값 채우는 과정 --------------------- #

#1. 처음 출전하는 선수들의 데이터를 채우기 
# B팀 R팀 선수들을 팀 무시하고 하나의 열로 만듬 >> 한 선수가 한팀에 속하는게 아니고 R팀도 B팀에도 속하는 경우가 있어서 

df_B <- read.csv('ufcdata/data_B.csv', stringsAsFactors = F, na.strings = c(""," ",NA)) #2001년 이후 B선수들 데이터만 순서대로 
df_R <- read.csv('ufcdata/data_R.csv', stringsAsFactors = F, na.strings = c(""," ",NA)) #2001년 이후 R선수들 데이터만 순서대로

df <- rbind(df_B,df_R)  # 9774행 74열  > ID변수 추가함 > 결측치 처리 후 B팀과 R팀 적절히 분류하기 위해 
head(df)

freq <- data.frame(table(df$fighter))  # 각 선수별로 2001년 이후 몇번 경기에 출전했는지 알려줌 
names(freq) <- c("fighter","total")
freq$fighter <- as.character(freq$fighter)
head(freq)
library(dplyr)
df <- left_join(df,freq, by='fighter')  # df데이터에 각 선수 출전 횟수 나타낸 열을 추가 > 행변화없음 , 열만 +1개 

df_col_na <- sapply(df, FUN=col_na)
View(df_col_na)
sum(!!df_col_na==1690)   # 49개의 공격변수가 결측값 >> 9774행 중 1690개의 행이 결측값 
nrow(df[df$total_rounds_fought==0,])  # total_rounds_fought=0 인 행이 1690개 존재 
# 위의 두개의 코드를 통해 49개의 공격변수가 결측값인 이유는 처음 출전한 선수들이기 때문임을 다시 한번 확인 


# Q. 우리가 지금 해야 하는 것은?
# A. 자신의 첫 경기가 2001년 이후인 선수들의 첫경기 데이터를 그선수의 다른 경기 데이터들을 이용해서 채워넣어야 한다.
# 이때, 아예 2001년 이후 경기를 1번만 한 선수는 참고할 데이터가 없으니 삭제
# 이때, 2001년 이후 경기를 2번한 선수들도 참고할 데이터가 하나밖에 없으니 삭제
# 결론적으로, 2001년 이후 경기를 3번 이상한 선수들 중 첫 경기와 가장 가까운 시기에 한 두개의 경기의 평균으로 채워넣음 

# 주의!! 첫경기가 즉, total==1 또는2인 행들을 모두 제거해야 한다? NONO
# WHY? total변수는 2001년 이후 출전한 경기횟수이다. 즉, total이 1인 모든 선수는 49개의 공격변수가 결측값이다 라고 생각하면 NONO
# 2001년 이전에 이미 경기를 한 선수라면 2001년 이후 total=1이라도 공격변수들이 모두 채워져있을 것이다. 

# ---------------------------------------------------------------------------------------------------------------------- #
# ---------------------------------------------------------------------------------------------------------------------- #
# ------------ 이제 채워보자 !!--------------#
################################## 여기까지 함 ##############################################
# 1. total==1인 선수들 결측값 처리 과정
# total 1인 선수는 총 285명이니 285행을 저장해서 불러와보자
total_1 <- df[df$total==1,]
dim(total_1)    #285행 확인 > 잘 불러옴 
View(sapply(total_1,FUN=col_na))   # 285개 행에서  277행이 공격변수들 결측 
# 진짜 277명인가 한번 더 확인해보자
nrow(total_1[total_1$total_rounds_fought==0,])  #total_rounds_fought==0 > 277행과 동일함 
# 결론 : total이 1인 행을 모두 지우는게 아니고 285명의 선수들 중 277명의 데이터만 지우면 됨 

total_1 <- total_1[total_1$total_rounds_fought!=0,]
dim(total_1)  #8명의 선수는 2001년 이전에 이미 경기를 한 선수이기 때문에 결측치가 없어서 살려둬야함



# 1. total==2인 선수들 결측값 처리 과정
# total 2인 선수는 총 321명이니 (321X2 = ) 642행을 저장해서 불러와보자

total_2 <- df[df$total==2,]
dim(total_2)  #642행 확인 > 잘 불러옴  
View(sapply(total_2,FUN=col_na))   # 2001년 이후 처음 출전하는 선수들은 315명 
# 진짜 315명인지 한번 더 확인해보자
nrow(total_2[total_2$total_rounds_fought==0,])  #total_rounds_fought==0 > 315행과 동일함 
# 즉, total이 2인 행을 모두 지우는게 아니고 321명의 선수들 중 315명의 데이터만 지우면 됨 
# 즉, 642행에서 315행만 삭제하면 된다!! 

total_2 <- total_2[total_2$total_rounds_fought!=0,]
dim(total_2)  # 642-315 = 327 
View(sapply(total_2,FUN=col_na))

# total 3 ~ 32 까지는 제거하는 행이 없기 때문에 for문으로 채움! for문 돌리기전 어떤 흐름으로 for문 돌아가는지 확인 과정 

# total 3인 사람을 보자 
# 위와 같은 이유면 total이 3인 선수는 232명이지만 232명 모두가 2001년 이후에 처음 출전한게 아니고
# 2001년 이전에 이미 출천을 한 선수가 있을 수 있고 이 선수 같은 경우는
# 자기가 가지고 있는 세경기의 데이터의 결측치가 모두 없을 수 도 있다. 

total_3 <- df[df$total==3,]
dim(total_3)  #696행 확인 > 잘 불러옴  > 232명 선수 
View(sapply(total_3,FUN=col_na))   # 2001년 이후 처음 출전하는 선수들은 223명 > 즉, 9명의 선수는 결측값 아예 없음 
name_3 <- unique(total_3$fighter)  # 232명의 선수들 이름 
index_3 <- data.frame(name_3,1:length(name_3)) # 각 선수별로 인덱스 숫자를 부여함 
names(index_3) <- c('fighter','fighter_ID')
index_3$fighter <- as.character(index_3$fighter)
head(index_3)
total_3 <- left_join(total_3,index_3, by='fighter') %>% arrange(fighter_ID,desc(date))
head(total_3)  # 각 선수들의 3번의 경기를 한번에 볼 수 있음 

# 우선 첫번째 선수의 데이터 3개를 이용해 처음 출전했던 경기의 결측값을 채워보자 #
data1 <- total_3[total_3$fighter_ID==1,]  # 1자리에는 i 가 와서 for 문 돌릴거 
# 만약 저장한 data1에 total_fought == 0 인게 있으면 data1안에서 함수가 실행되고 
# 만약 저장한 data1에 total_fount 가 0인게 없으면 data1를 빠져나와서 data2를 검사하라 

# 만약 total_fought==0인게 있으면 다음 아래의 과정을 실행 (다음 과정을 함수로 일반화 시켜야됨)
data1[nrow(data1),10] <- mean(data1[c(nrow(data1)-2,nrow(data1)-1),10])  # 공격변수49개중 첫번째 변수가 10번째 컬럼에 위치 
# 10번째 컬럼을 최근 2개의 데이터의 평균으로 채움 


# 이제 실전 for문으로 모든 결측값을 채워보자 #

ex <- c()

for (i in c(3:30,32)) {               # i 는 total 이 몇인지 의미 > 31번 출전한 선수는 없어서 31은 pass
  
  total_number <- df[df$total==i,]  #i번 출전한 선수들의 데이터만 따로 뽑아서 total_number 에 저장 
  name <- unique(total_number$fighter)
  index <- data.frame(name,1:length(name))
  names(index) <- c('fighter','fighter_ID')
  index$fighter <- as.character(index$fighter)
  total_number <- left_join(total_number,index, by='fighter') 
  total_number<- total_number %>% arrange(fighter_ID,desc(date))  # > 한 선수당 i번째 데이터를 묶어서 정렬 
  print(i)  # 잘 돌아가는지 확인하기 위해 
  
  for (j in 1:length(name)){
    data <- total_number[total_number$fighter_ID==j,]  # j가 의미하는 것은? 각 선수ID
    if (sum(!!data$total_rounds_fought==0)==1) {     # 한선수의 데이터에서 첫 출전 행이 있다면 for문으로 들어가서 채워라 
      for (k in c(10:33,36:59,61)){
        data[nrow(data),k] <- mean(data[c(nrow(data)-2,nrow(data)-1),k])
        # k가 의미하는 것은? > 우리가 채워야 할 변수의 위치 인덱스
      }
    }
    ex <- rbind(data,ex)
  } 
}

ex <- ex[-76] # total_1과 total_2 와 rbind 해주려면 ID 변수 빼줘야함 > 열 개수 동일해야 하니깐 
ex <- rbind(total_1,total_2,ex)
dim(ex)   #최종적으로 9774개의 행에서 9182행으로 감소 
View(sapply(ex,FUN=col_na)) # 결측값이 없어진걸 확인 가능
write.csv(ex,"결측값채운데이터.csv", row.names=F)


ex <- ex %>% arrange(ID)
head(ex)

# 1. B와 R팀 데이터를 분리 
ex_B<- ex[substr(ex$ID,nchar(ex$ID),nchar(ex$ID))=='B',]  # 결측치 처리 후  4887명에서 4428명으로 줄어듬
ex_R<- ex[substr(ex$ID,nchar(ex$ID),nchar(ex$ID))=='R',]  # 결측치 처리 후  4887명에서 4754명으로 줄어듬 


# 2. 분리했으니 ID에 B,R 표시 의미 없어짐 > 숫자만 남겨주기 
ex_B$ID <-substr(ex_B$ID,1,nchar(ex_B$ID)-1)
ex_R$ID <-substr(ex_R$ID,1,nchar(ex_R$ID)-1)

# 3. ID가 숫자의 의미를 갖도록 변환해주기 > 숫자여야 정렬 가능 
ex_B$ID <- as.numeric(ex_B$ID)
ex_R$ID <- as.numeric(ex_R$ID)

ex_B <- ex_B %>% arrange(ID)
ex_R <- ex_R %>% arrange(ID)

# ex_B데이터와 ex_R 데이터는 결측값 다 채운 각 팀들의 데이터 
write.csv(ex_B,'ex_B.csv', row.names = F)
write.csv(ex_R,'ex_R.csv', row.names = F)


# 위의 데이터 저장후 내가 직접 엑셀에서 공격변수들 앞에 R_, B_ 붙여줌 >> 원래 초기 데이터 형태로 
ex_B <- read.csv('ex_B.csv', stringsAsFactors = F)
ex_R <- read.csv('ex_R.csv', stringsAsFactors = F)

df <- full_join(ex_B,ex_R, by='ID')

dim(df)  #4887행에서 4851행으로 줄어듬 
View(sapply(df,FUN=col_na))  # 결측값 형태 확인
# 처음 출전 선수들을 삭제했기 때문에 결측값 존재
# 공격변수 49개가 결측치 패턴이  B팀은 423개, R팀은 97개 관찰 >> 모두 제거 

df <- df[!is.na(df$B_current_lose_streak),]
dim(df)  # 4851행에서 -423 = 4428 행으로 줄어듬
df <- df[!is.na(df$R_current_lose_streak),]
dim(df)  # 4428행에서 -97 = 4331 행으로 줄어듬 

write.csv(df,"df.csv", row.names=F)  # 최종 결측치 처리 (age, height 등 제외) df.csv로 저장 






