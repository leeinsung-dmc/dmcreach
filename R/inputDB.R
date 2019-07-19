#'inputDB함수 사용법
#'
#'inputDB함수는 계수DB를 입력하는 함수입니다
#'
#'@param DB_VA, DB_DA
#'@return 표면적인 결과물은 없지만, 이 함수를 실행하지 않으면 분석이 시행되지 않습니다
#'@examples inputpopperc("pop_perc.csv")
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
