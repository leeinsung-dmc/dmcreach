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

  inputDB("parameter_VA.csv","parameter_DA.csv")
  inputpop("pop.csv")
  inputpopperc("pop_perc.csv")

  load("parameter_DB.RData")
  load("pop_DB.RData")
  load("popperc_DB.RData")

  ##교집합 타겟 산출 함수 정의
  temptarget_logic <-function(a,b)
  {
    target_logic<-data.frame(gender = c("M","M","M","M","M","M","M","M","M","M","F","F","F","F","F","F","F","F","F","F"), age_min = c(7,13,19,25,30,35,40,45,50,60,7,13,19,25,30,35,40,45,50,60), age_max = c(12,18,24,29,34,39,44,49,59,69,12,18,24,29,34,39,44,49,59,69))
    target_logic$input_target<-0
    target_logic$output_target<-0
    target_logic$temp_target<-0

    ##a <- temptarget_input
    ##b <- temptarget_output

    in_target <- a
    out_target <- b
    in_target_gender <- in_target[1]
    in_target_age_min <- floor(as.numeric(in_target[2])/100)
    in_target_age_max <- as.numeric(in_target[2])-in_target_age_min*100
    out_target_gender <- out_target[1]
    out_target_age_min <- floor(as.numeric(out_target[2])/100)
    out_target_age_max <- as.numeric(out_target[2])-out_target_age_min*100

    for(i in 1:20) {
      ifelse((target_logic[i,]$gender==in_target_gender)|in_target_gender=="MF", a<-1 , a<-0)
      ifelse((target_logic[i,]$age_min>=in_target_age_min), b<-1 , b<-0)
      ifelse((target_logic[i,]$age_max<=in_target_age_max), c<-1 , c<-0)
      ifelse(a+b+c == 3, target_logic[i,]$input_target <- 1, target_logic[i,]$input_target <- 0)

      ifelse((target_logic[i,]$gender==out_target_gender)|out_target_gender=="MF", a<-1 , a<-0)
      ifelse((target_logic[i,]$age_min>=out_target_age_min), b<-1 , b<-0)
      ifelse((target_logic[i,]$age_max<=out_target_age_max), c<-1 , c<-0)
      ifelse(a+b+c == 3, target_logic[i,]$output_target <- 1, target_logic[i,]$output_target <- 0)

      ifelse(target_logic[i,]$input_target+target_logic[i,]$output_target == 2, target_logic[i,]$temp_target <- 1, target_logic[i,]$temp_target <- 0)

    }

    temp_target <- c(NA,NA)

    temp_target[2]<-ifelse(sum(target_logic$temp_target) == 0, 0, min(target_logic %>%
                                                                        filter(temp_target==1) %>%
                                                                        select(age_min))*100+ max(target_logic %>%
                                                                                                    filter(temp_target==1) %>%
                                                                                                    select(age_max)))

    ifelse(sum(target_logic$temp_target) == 0, temp_target_gender<-data.frame(gender=c(0,0)), temp_target_gender<-unique(target_logic %>%
                                                                                                                           filter(temp_target==1) %>%
                                                                                                                           select(gender)))

    ifelse(sum(target_logic$temp_target) == 0, temp_target[1]<-0,ifelse(nrow(temp_target_gender)==2,temp_target[1]<-paste(temp_target_gender[1,],temp_target_gender[2,], sep=""), temp_target[1]<-as.character(temp_target_gender[1,])))
    return(temp_target)

  }

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

  ##e_reach 함수 테스트용
  ##rm(list="mix")
  ##mix<-read.csv("temp1.csv", stringsAsFactors=FALSE)
  ##output<-c("M",5069)
  ##i<-15

  mix<-read.csv(mixfile, stringsAsFactors=FALSE)
  mix<-transoverall(mix)
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
    ##pop_cal<-1492523

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
    mix[i,]$reach_w <- as.numeric(round(parameter[1]/(1+exp(-(log(mix[i,]$grps*parameter[4])*parameter[2]+parameter[3])))*100, digits=2))
    mix[i,]$reach_n <- as.numeric(round(pop_cal*mix[i,]$reach/100, digits=0))
    mix[i,]$reach_n_w <- as.numeric(round(pop_cal*mix[i,]$reach_w/100, digits=0))
    mix[i,]$AF <- mix[i,]$grps/mix[i,]$reach
    mix[i,]$AF_w <- mix[i,]$grps_w/mix[i,]$reach_w
  }

  return(mix)

  rm(list="parameter")
  rm(list="pop_cal")
}

