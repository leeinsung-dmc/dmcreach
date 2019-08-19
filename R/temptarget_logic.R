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
