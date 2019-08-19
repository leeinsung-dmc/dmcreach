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




inputDB <- function(DB_VA,DB_DA) {

  parameter_DB<-read.csv(DB_VA, stringsAsFactors=FALSE)
  cname<-c("Level","Device","Channel","Vehicle","target","gender")
  parameter_DB[,cname] <- lapply(parameter_DB[,cname],as.factor)

  names(parameter_DB)[names(parameter_DB)=="계산용.limit"]<-c("limit_c")

  parameter_DB2<-read.csv(DB_DA, stringsAsFactors=FALSE)
  parameter_DB2[,cname] <- lapply(parameter_DB2[,cname],as.factor)


  names(parameter_DB2)[names(parameter_DB2)=="계산용.limit"]<-c("limit_c")
  names(parameter_DB2)[names(parameter_DB2)=="가중치"]<-c("weight")

  VA<-parameter_DB
  DA<-parameter_DB2
  VA[,23] <- 1
  DA[,24] <- "DA"
  VA[,24] <- "VA"
  names(VA)<-names(DA)
  bind_DB<-rbind(VA,DA)

  names(bind_DB)[names(bind_DB)=="V24"]<-c("type")

  bind_DB_check<-bind_DB

  bind_DB[,cname] <- lapply(bind_DB[,cname],as.character)

  rm(list="DA")
  rm(list="VA")
  rm(list="parameter_DB")
  rm(list="parameter_DB2")
  rm(list="cname")

  save(bind_DB, bind_DB_check, file="parameter_DB.RData")

}


