#'mix_Reach함수 사용법
#'
#'제작 : DMC미디어 데이터RnD팀
#'version : 1.190719
#'
#'@param mixfile, 양식에 맞게 작성된 믹스안 파일명을 확장자까지 함께하여 적어주세요 ex) "temp1.csv"
#'@param output, 분석기준 타겟을 벡터형식으로 입력해주세요. 성별,연령 순서로 입력하며, 성별의 경우 M/F/MF중 하나를 입력해주세요. 성별에 input을 입력하시면 mix안의 라인별 타겟 기준 예측을 시행합니다 ex) c("MF",3069)
#'@return data.frame형식으로 reach 예측값들이 저장되어 반환됩니다.
#'@examples mix_reach("MF",1969) / mix_reach("input",1969)
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


