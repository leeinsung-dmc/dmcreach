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



####필요한 함수정의################

inputpopperc <-function(DB_pop_perc) {

  pop_perc<-read.csv(DB_pop_perc,stringsAsFactors=FALSE)
  save(pop_perc, file="popperc_DB.RData")
}

