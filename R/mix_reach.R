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


mix_reach<- function(mixfile, output){

  ####필요한 함수정의 끝################


  ##e_reach 함수 테스트용
  ##rm(list="mix")
  ##mix<-read.csv("temp1.csv", stringsAsFactors=FALSE)
  ##output<-c("M",5069)
  ##i<-15

  inputDB("parameter_VA.csv","parameter_DA.csv")
  inputpop("pop.csv")
  inputpopperc("pop_perc.csv")

  load("parameter_DB.RData")
  load("pop_DB.RData")
  load("popperc_DB.RData")


  mix<-read.csv(mixfile, stringsAsFactors=FALSE)
  mix<-transoverall(mix)
  mix$output_pop <- NA
  mix$output_target_gender <- output[1]
  mix$output_target_age <- output[2]
  mix$impression_target <- NA
  mix$grps<-NA
  mix$grps_w<-NA
  mix$reach <-NA
  mix$reach_w <-NA
  mix$reach_n<-NA
  mix$reach_n_w<-NA
  mix$AF<-NA
  mix$AF_w<-NA
  pop_perc$target <- as.character(pop_perc$min*100+pop_perc$max)

  for(i in 1:nrow(mix)) {
    ##inputlevel분석인지 확인
    ifelse(output[1] == "input" , mix[i,]$output_target_age <- as.character(mix[i,]$target), mix[i,]$output_target_age <- as.character(mix[i,]$output_target_age))
    ifelse(output[1] == "input" , mix[i,]$output_target_gender <- as.character(mix[i,]$gender), mix[i,]$output_target_gender <- as.character(mix[i,]$output_target_gender))

    ##계수 선택
    parameter<-bind_DB%>%
      filter(type==mix[i,]$type & Device==mix[i,]$Device & Channel==mix[i,]$Channel & Vehicle==mix[i,]$Vehicle & gender==mix[i,]$gender & target==mix[i,]$target) %>%
      select(limit_c,b1,a,weight)
    ##모수 선택
    pop_cal<-pop%>%
      filter(gender==mix[i,]$output_target_gender & age==mix[i,]$output_target_age)%>%
      select(pop)
    #####!! 만약 모수를 직접 입력하고 싶으면 여기에서!
    ##pop_cal<-17821489

    temptarget_input <- c(as.character(mix[i,]$gender),as.numeric(as.character(mix[i,]$target)))
    temptarget_output <-  c(as.character(mix[i,]$output_target_gender),as.numeric(as.character(mix[i,]$output_target_age)))


    temptarget <- temptarget_logic(temptarget_input,temptarget_output)##기존타겟/output타겟의 교집합 타겟 추출

    if(temptarget[2]==0) {
      final_pop_perc<-0
    }else{
      ifelse(sum(grepl(as.character(mix[i,]$Channel),pop_perc$media))>0, tempchannel<-as.character(mix[i,]$Channel), tempchannel<-as.character(mix[i,]$Device)) ##배분비율 기준(채널/디바이스)확정.
      temp_pop_perc_original<-as.numeric(pop_perc%>%
                                           filter(target==as.character(mix[i,]$target) & media==tempchannel & gender == as.character(mix[i,]$gender)) %>%
                                           select(pop_perc)) ##기존 타겟의 배분비율 호출
      temp_pop_perc_temptarget<-as.numeric(pop_perc%>%
                                             filter(target==as.character(temptarget[2]) & media==tempchannel & gender == as.character(temptarget[1])) %>%
                                             select(pop_perc)) ##교집합 타겟의 배분비율 호출
      final_pop_perc <- temp_pop_perc_temptarget/temp_pop_perc_original ##배분비율 최종 산출
    }


    mix[i,]$impression_target<-mix[i,]$impression*final_pop_perc ##Impression에 배분비율 반영

    ## 계산
    mix[i,]$grps <- as.numeric(round(mix[i,]$impression_target/pop_cal*100, digits=2))
    mix[i,]$grps_w <- as.numeric(round(mix[i,]$impression_target*parameter[4]/pop_cal*100, digits=2))
    mix[i,]$reach <- as.numeric(round(parameter[1]/(1+exp(-(log(mix[i,]$grps)*parameter[2]+parameter[3])))*100, digits=2))
    ifelse(mix[i,]$reach<mix[i,]$grps, mix[i,]$reach<-mix[i,]$reach, mix[i,]$reach<-mix[i,]$grps)
    mix[i,]$reach_w <- as.numeric(round(parameter[1]/(1+exp(-(log(mix[i,]$grps*parameter[4])*parameter[2]+parameter[3])))*100, digits=2))
    ifelse(mix[i,]$reach_w<mix[i,]$grps_w, mix[i,]$reach_w<-mix[i,]$reach_w, mix[i,]$reach_w<-mix[i,]$grps_w)
    mix[i,]$reach_n <- as.numeric(round(pop_cal*mix[i,]$reach/100, digits=0))
    mix[i,]$reach_n_w <- as.numeric(round(pop_cal*mix[i,]$reach_w/100, digits=0))
    mix[i,]$AF <- mix[i,]$grps/mix[i,]$reach
    mix[i,]$AF_w <- mix[i,]$grps_w/mix[i,]$reach_w
    mix[i,]$output_pop <- pop_cal


  }

  mix$output_pop <- as.numeric(mix$output_pop)

  return(mix)



}
