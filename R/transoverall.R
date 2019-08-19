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





##광고상품 overall로 변환 함수 정의

transoverall<-function(mix){

  load("parameter_DB.RData")
  load("pop_DB.RData")
  load("popperc_DB.RData")

  n<-nrow(mix)

  temp_mix<-data.frame(number=1:7)

  for( count in 1:n ) {
    calcond <- c(mix[count,1],mix[count,3],mix[count,2],mix[count,4],mix[count,6],mix[count,5],mix[count,7])
    cond_logic<-grepl(calcond[4],bind_DB[,4])
    if(sum(cond_logic)>0){
      calcond[4]<-calcond[4]
    }else{
      calcond[4]<-"overall"
    }
    temp_mix[,count+1]<-calcond
  }

  temp_mix<-temp_mix[,-1]
  names(temp_mix)<-1:n
  temp_mix<-t(temp_mix)
  temp_mix<-data.frame(temp_mix)
  names(temp_mix)<-c("type","Device","Channel","Vehicle","gender","target","impression")
  temp_mix$impression<-as.numeric(gsub(",","",temp_mix$impression))
  return(temp_mix)
  rm(list="temp_mix")
}
