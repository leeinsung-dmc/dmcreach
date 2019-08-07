#'mix_Reach_channel함수 사용법
#'
#'제작 : DMC미디어 데이터RnD팀
#'version : 1.190731
#'
#'@param mixfile, 양식에 맞게 작성된 믹스안 파일명을 확장자까지 함께하여 적어주세요 ex) "temp1.csv"
#'@param output, 분석기준 타겟을 벡터형식으로 입력해주세요. 성별,연령 순서로 입력하며, 성별의 경우 M/F/MF중 하나를 입력해주세요. 성별에 input을 입력하시면 mix안의 라인별 타겟 기준 예측을 시행합니다 ex) c("MF",3069)
#'@return data.frame형식으로 reach 예측값들이 저장되어 반환됩니다.
#'@examples mix_reach_channel(result)
#'@export



########채널레벨

mix_reach_channel <- function(result2) {

  result_channel<-unique(subset(result2, select = c(type, Device, Channel)))
  result_channel[,c("type","Device","Channel")] <- lapply(result_channel[,c("type","Device","Channel")],as.character)
  result_channel$output_pop <- as.numeric(unique(subset(result2, select = output_pop)))
  result_channel$output_target_gender <- as.character(unique(subset(result2, select = output_target_gender)))
  result_channel$output_target_age <- as.numeric(unique(subset(result2, select = output_target_age)))
  result_channel$impression_target <- NA
  result_channel$grps <-NA
  result_channel$grps_w <- NA
  result_channel$reach <-NA
  result_channel$reach_w <-NA
  result_channel$reach_n<-NA
  result_channel$reach_n_w<-NA
  result_channel$AF<-NA
  result_channel$AF_w<-NA
  result_channel$temp_grps <-NA
  result_channel$temp_grps_w <- NA


  ####필요한 함수정의################

  inputpopperc <-function(DB_pop_perc) {

    pop_perc<-read.csv(DB_pop_perc,stringsAsFactors=FALSE)
    save(pop_perc, file="popperc_DB.RData")
  }


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

  ###필수 DB 입력

  inputDB("parameter_VA.csv","parameter_DA.csv")
  inputpop("pop.csv")
  inputpopperc("pop_perc.csv")

  load("parameter_DB.RData")
  load("pop_DB.RData")
  load("popperc_DB.RData")



  ##역산함수 정의

  reach_yuk <- function(mix) {

    mix$temp_grps <- NA
    mix$temp_grps_w <- NA

    for ( i in 1:nrow(mix)){

      parameter<-bind_DB%>%
        filter(type==mix[i,]$type & Device==mix[i,]$Device & Channel==mix[i,]$Channel & Vehicle=="overall" & gender==mix[i,]$output_target_gender & target==mix[i,]$output_target_age) %>%
        select(limit_c,b1,a,weight)

      mix[i,]$temp_grps <- exp((parameter$a*-1+log(parameter$limit_c/(mix[i,]$reach/100)-1)*-1)/parameter$b1)
      mix[i,]$temp_grps_w <- exp((parameter$a*-1+log(parameter$limit_c/(mix[i,]$reach_w/100)-1)*-1)/parameter$b1)
    }

    return(mix)
  }



  ##Reach 함수 정의

  reach_cal_channel <- function(mix, i) {

    ##모수
    pop_cal<-pop%>%
      filter(gender==mix[i,]$output_target_gender & age==mix[i,]$output_target_age)%>%
      select(pop)
    ##계수 선택
    parameter<-bind_DB%>%
      filter(type==mix[i,]$type & Device==mix[i,]$Device & Channel==mix[i,]$Channel & Vehicle=="overall" & gender==mix[i,]$output_target_gender & target==mix[i,]$output_target_age) %>%
      select(limit_c,b1,a,weight)

    ## 계산
    mix[i,]$reach <- as.numeric(round(parameter[1]/(1+exp(-(log(mix[i,]$temp_grps)*parameter[2]+parameter[3])))*100, digits=2))
    ifelse(mix[i,]$reach<grps, mix[i,]$reach<-mix[i,]$reach, mix[i,]$reach<-mix[i,]$grps)
    mix[i,]$reach_w <- as.numeric(round(parameter[1]/(1+exp(-(log(mix[i,]$temp_grps_w)*parameter[2]+parameter[3])))*100, digits=2))
    ifelse(mix[i,]$reach_w<grps_w, mix[i,]$reach_w<-mix[i,]$reach_w, mix[i,]$reach_w<-mix[i,]$grps_w)
    mix[i,]$reach_n <- as.numeric(round(pop_cal*mix[i,]$reach/100, digits=0))
    mix[i,]$reach_n_w <- as.numeric(round(pop_cal*mix[i,]$reach_w/100, digits=0))
    mix[i,]$AF <- mix[i,]$grps/mix[i,]$reach
    mix[i,]$AF_w <- mix[i,]$grps_w/mix[i,]$reach_w

    return(mix)
  }

  ##채널레벨용 Reach 함수 정의 끝
  ###필요함수 정의 끝

  ##계산

  temp<-reach_yuk(result2)

  for( i in 1:nrow(result_channel)) {

    temp2 <-subset(temp, type==result_channel[i,1] & Device==result_channel[i,2] & Channel==result_channel[i,3], select = c(type, Device, Channel, impression_target, grps, grps_w, temp_grps, temp_grps_w))
    result_channel[i,]$impression_target<-sum(temp2$impression_target)
    result_channel[i,]$grps<-sum(temp2$grps)
    result_channel[i,]$grps_w<-sum(temp2$grps_w)
    result_channel[i,]$temp_grps<-sum(temp2$temp_grps)
    result_channel[i,]$temp_grps_w<-sum(temp2$temp_grps_w)


    result_channel<-reach_cal_channel(result_channel, i)

  }
  result_channel <- result_channel[,1:15]

  return(result_channel)

}

###채널레벨 함수정의 끝
