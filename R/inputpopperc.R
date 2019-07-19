#'inputpopperc함수 사용법
#'
#'inputpopperc함수는 타겟별 Impression 배분비율을 적용하는 함수입니다
#'
#'@param DB_pop_perc 사전에 준비된 pop_perc 엑셀파일명 입력변수입니다
#'@return 표면적인 결과물은 없지만, 이 함수를 실행하지 않으면 분석이 시행되지 않습니다
#'@examples inputpopperc("pop_perc.csv")
#'@export

inputpopperc <-function(DB_pop_perc) {

  pop_perc<-read.csv(DB_pop_perc,stringsAsFactors=FALSE)
  save(pop_perc, file="popperc_DB.RData")
}
