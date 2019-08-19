#'mix_Reach_vehicle
#'
#'Copyright : DMC Media Data RnD team
#'version : 1.190719
#'
#'@param mix, mix which result of mix_Reach
#'@return data.frame
#'@examples mix_reach(mix)
#'@export





###Vehicle Level Reach 예측 함수 정의

mix_reach_vehicle <- function(mix) {

  ##결과 레이아웃 정의

  result_vehicle<-unique(subset(mix, select = c(type, Device, Channel, Vehicle)))
  result_vehicle[,c("type","Device","Channel","Vehicle")] <- lapply(result_vehicle[,c("type","Device","Channel","Vehicle")],as.character)
  result_vehicle$output_pop <- as.numeric(unique(subset(mix, select = output_pop)))
  result_vehicle$output_target_gender <- as.character(unique(subset(mix, select = output_target_gender)))
  result_vehicle$output_target_age <- as.numeric(unique(subset(mix, select = output_target_age)))
  result_vehicle$impression_target <- NA
  result_vehicle$grps <-NA
  result_vehicle$grps_w <- NA
  result_vehicle$reach <-NA
  result_vehicle$reach_w <-NA
  result_vehicle$reach_n<-NA
  result_vehicle$reach_n_w<-NA
  result_vehicle$AF<-NA
  result_vehicle$AF_w<-NA

  ####비히클레벨_필요한 함수정의################
  ##비히클레벨용 Reach 함수 정의

  reach_cal <- function(mix, i) {

    ##모수
    pop_cal<-pop%>%
      filter(gender==mix[i,]$output_target_gender & age==mix[i,]$output_target_age)%>%
      select(pop)
    ##계수 선택
    parameter<-bind_DB%>%
      filter(type==mix[i,]$type & Device==mix[i,]$Device & Channel==mix[i,]$Channel & Vehicle==mix[i,]$Vehicle & gender==mix[i,]$output_target_gender & target==mix[i,]$output_target_age) %>%
      select(limit_c,b1,a,weight)

    ## 계산
    mix[i,]$reach <- as.numeric(round(parameter[1]/(1+exp(-(log(mix[i,]$grps)*parameter[2]+parameter[3])))*100, digits=2))
    ifelse(mix[i,]$reach<mix[i,]$grps, mix[i,]$reach<-mix[i,]$reach, mix[i,]$reach<-mix[i,]$grps)
    mix[i,]$reach_w <- as.numeric(round(parameter[1]/(1+exp(-(log(mix[i,]$grps*parameter[4])*parameter[2]+parameter[3])))*100, digits=2))
    ifelse(mix[i,]$reach_w<mix[i,]$grps_w, mix[i,]$reach_w<-mix[i,]$reach_w, mix[i,]$reach_w<-mix[i,]$grps_w)
    mix[i,]$reach_n <- as.numeric(round(pop_cal*mix[i,]$reach/100, digits=0))
    mix[i,]$reach_n_w <- as.numeric(round(pop_cal*mix[i,]$reach_w/100, digits=0))
    mix[i,]$AF <- mix[i,]$grps/mix[i,]$reach
    mix[i,]$AF_w <- mix[i,]$grps_w/mix[i,]$reach_w
    mix[i,]$output_pop <-

      return(mix)
  }

  ##비히클레벨용 Reach 함수 정의 끝

  ###필요함수 정의 끝

  ###필수 DB 입력

  inputDB("parameter_VA.csv","parameter_DA.csv")
  inputpop("pop.csv")
  inputpopperc("pop_perc.csv")

  load("parameter_DB.RData")
  load("pop_DB.RData")
  load("popperc_DB.RData")

  ###필수 DB 입력 끝

  ###결과분석

  for( i in 1:nrow(result_vehicle)) {

    temp2 <-subset(mix, type==result_vehicle[i,1] & Device==result_vehicle[i,2] & Channel==result_vehicle[i,3] & Vehicle==result_vehicle[i,4], select = c(type, Device, Channel, Vehicle, impression_target, grps, grps_w))
    result_vehicle[i,]$impression_target<-sum(temp2$impression_target)
    result_vehicle[i,]$grps<-sum(temp2$grps)
    result_vehicle[i,]$grps_w<-sum(temp2$grps_w)
    result_vehicle<-reach_cal(result_vehicle, i)

  }

  ###결과분석 끝

  return(result_vehicle)


}
#######비히클레벨 분석함수 정의 끝
