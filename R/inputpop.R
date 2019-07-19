#'inputpop함수 사용법
#'
#'inputDB함수는 계수DB를 입력하는 함수입니다
#'
#'@param DB_VA, DB_DA
#'@return 표면적인 결과물은 없지만, 이 함수를 실행하지 않으면 분석이 시행되지 않습니다
#'@examples inputpopperc("pop_perc.csv")
#'@export


inputpop <- function(popDB) {

  pop <- read.csv(popDB, stringsAsFactors=FALSE)
  pop$pop<-as.numeric(gsub(",","",pop$pop))
  pop$age<-as.character(pop$age)
  pop$age_min<-as.character(pop$age_min)
  pop$age_max<-as.character(pop$age_max)

  c=1
  i=1
  k=1
  gender=1

  ## 1. 남자별,여자별 모수생성
  for(gender in 1:2){
    for(i in 1:10){
      for(k in 1:(11-i)){
        pop[22+c,]$pop<-sum(pop[(i+11*(gender-1)):(i+11*(gender-1)+k),5])
        pop[22+c,]$age_min<-pop[i+11*(gender-1),]$age_min
        pop[22+c,]$age_max<-pop[i+11*(gender-1)+k,]$age_max
        pop[22+c,]$age<-paste(pop[22+c,]$age_min,pop[22+c,]$age_max,sep="")
        pop[22+c,]$gender<-pop[i+11*(gender-1),]$gender
        c<-c+1
      }
    }
  }

  ## 2. 남녀 모수 생성

  temp_M<-pop%>%
    filter(gender=="M")
  temp_F<-pop%>%
    filter(gender=="F")

  temp_MF<-data.frame(gender="MF",age=temp_M$age,age_min=temp_M$age_min,age_max=temp_M$age_max,pop=temp_M$pop+temp_F$pop)

  pop<-rbind(pop,temp_MF)

  rownames(pop)<-NULL


  ##임시 객체 삭제
  rm(list="temp_F")
  rm(list="temp_M")
  rm(list="temp_MF")
  rm(list="c")
  rm(list="gender")
  rm(list="i")
  rm(list="k")

  save(pop, file="pop_DB.RData")


}
