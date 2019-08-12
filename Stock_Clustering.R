######################################################################################################################################################
######################################################################################################################################################
##### 코드 0  : DATA EDA



# 3년치 데이터가 다 있는 파일은 바로 financial_1635개_3년치종목.csv



#financal <- fread("financial_1635개_3년치종목.csv", encoding = "UTF-8") %>% as_tibble

financial <- fread("financial_프로펫적합까지_변수추가한것_2월18일.csv", encoding = "UTF-8") %>% as_tibble
financial$date <- ymd(financial$date)


financial <- fread("financial_1635개_3년치종목.csv", encoding = "UTF-8") %>% as_tibble
financial$date <- ymd(financial$date)
financial <- financial %>% select(name,date,price,everything())
financial






financial <- financial %>% mutate(shares = market_cap/price)

financial %>% distinct(name, .keep_all = T) %>% select(sector) %>% table %>% as.tibble %>% arrange(desc(n)) %>% View


### 종목 이름 or 종목 코드 입력하면 그래프가 나온다. 



plot_name_price <- function(NAME){
  ggplot(data = financial %>% filter(name == NAME)
         , aes(x = date, y = close)) + 
    geom_line(size = 1.3) +
    geom_smooth(method = "loess", color = "blue", span = 1/5) +
    labs(title = paste(NAME, "종가"))
}



plot_name_volume <- function(NAME){
  ggplot(data = financial %>% filter(name == NAME)
         , aes(x = date, y = volume)) + 
    geom_line(size = 1.3) +
    geom_smooth(method = "loess", color = "blue", span = 1/5) +
    labs(title = paste(NAME, "시가총액"))
}


plot_name_PER <- function(NAME){
  ggplot(data = financial %>% filter(name == NAME)
         , aes(x = date, y = PER)) + 
    geom_line(size = 1.3) +
    geom_smooth(method = "loess", color = "blue", span = 1/5) +
    labs(title = paste(NAME, "PER"))
}







plot_code <- function(code){
  
  ggplot(data = financial %>% filter(CODE == code)
         
         , aes(x = date, y = price)) + 
    
    geom_line(size = 1.3) +
    
    geom_smooth(method = "loess", color = "blue", span = 1/5) +
    
    labs(title = code)
  
}



plot_name("삼성전자")  

plot_code(5930)  



### 특정 기간을 입력하면 그 부분만 그려진다 



plot_name_zoom <- function(NAME, start, end){
  
  ggplot(data = financial %>% filter(name == NAME) %>% filter(date >= ymd(start) & date <= ymd(end)),
         
         aes(x = date, y = scale(market_cap)) ) +
    
    geom_line(size = 1.3) +
    
    geom_smooth(method = "loess", color = "blue", span = 1/5) +
    
    labs(title = NAME)  
  
}



plot_name_zoom("삼성전자", start = ymd(20151112), end = ymd(20181228))







### 여러 종목을 한 번에 그릴 수 있는 코드



plot_names <- function(NAME1, NAME2){
  
  ggplot() + 
    geom_line(data = financial %>% filter(name == NAME1) ,
              aes(x = date, y = price),
              color = 'green', size = 1.2) +
    
    
    geom_line(data = financial %>% filter(name == NAME2) ,
              aes(x = date, y = price),
              color = 'gold', size = 1.2)
}



plot_names("LG디스플레이", "LG전자")







##### Time Series Parameter   

#    : mean, standard deviation, amplitude, and a the slope of a naive linear fit





# 액면분할 한 종목들은 어떻게 해야될지 잘 모르겠다. 





summary(lm(log(dat$market_cap)~ date, data = dat ))$coef[2]

summary(lm(log(dat$market_cap)~ date, data = dat ))$coef[4]



parameter_market_cap <- function(NAME){
  
  dat = financial %>% filter(name == NAME)
  
  slope <- ifelse(is.na(mean(log(dat$market_cap))), 0, summary(lm(log(dat$market_cap) ~ date, data = dat))$coef[2])
  slope_err <- ifelse(is.na(mean(log(dat$market_cap))), 0, summary(lm(log(dat$market_cap) ~ date, data = dat))$coef[4])
  bar <- tibble(
    
    name = NAME,
    
    min_cap = min(log(dat$market_cap)),
    max_cap = max(log(dat$market_cap)),
    mean_cap = mean(log(dat$market_cap)),
    median_cap = median(log(dat$market_cap)),
    sd_cap = sd(log(dat$market_cap)),
    slope  = slope  )
  return(bar)
  
}

parameter_close <- function(NAME){
  
  dat = financial %>% filter(name == NAME)
  
  slope <- ifelse(is.na(mean(dat$close)), 0, summary(lm(dat$close ~ date, data = dat))$coef[2])
  slope_err <- ifelse(is.na(mean(dat$close)), 0, summary(lm(dat$close ~ date, data = dat))$coef[4])
  
  bar <- tibble(
    name = NAME,
    min_close = min(dat$close),
    max_close = max(dat$close),
    mean_close = mean(dat$close),
    median_close = median(dat$close),
    sd_close = sd(dat$close),
    slope  = slope  
  )
  return(bar)
  
}

parameter_PER <- function(NAME){
  
  dat = financial %>% filter(name == NAME)
  
  slope <- ifelse(is.na(mean(dat$PER)), 0, summary(lm(dat$PER ~ date, data = dat))$coef[2])
  slope_err <- ifelse(is.na(mean(dat$PER)), 0, summary(lm(dat$PER ~ date, data = dat))$coef[4])
  
  bar <- tibble(
    name = NAME,
    min_PER = min(dat$PER),
    max_PER = max(dat$PER),
    mean_PER = mean(dat$PER),
    median_PER = median(dat$PER),
    sd_PER = sd(dat$PER),
    slope  = slope  
  )
  return(bar)
  
}

parameter_market_cap("셀트리온")
parameter_close("셀트리온")
parameter_PER("셀트리온")



params_market_cap_all <- matrix(NA, length(unique(financial$name)),7) %>% as.data.frame
names(params_market_cap_all) <- c("name","min_cap","max_cap",    "mean_cap" ,  "median_cap", "sd_cap"  ,   "slope" )
params_market_cap_all

for(i in 1 : length(unique(financial$name))){
  params_market_cap_all[i,] <-  parameter_market_cap(unique(financial$name)[i])
}

params_close_all <- matrix(NA, length(unique(financial$name)),7) %>% as.data.frame
names(params_close_all) <- c("name","min_close","max_close",    "mean_close" ,  "median_close", "sd_close"  ,   "slope" )
params_close_all

for(i in 1 : length(unique(financial$name))){
  params_close_all[i,] <-  parameter_close(unique(financial$name)[i])
}

params_PER_all <- matrix(NA, length(unique(financial$name)),7) %>% as.data.frame
names(params_PER_all) <- c("name","min_PER","max_PER",    "mean_PER" ,  "median_PER", "sd_PER"  ,   "slope" )

for(i in 1 : length(unique(financial$name))){
  params_PER_all[i,] <-  parameter_PER(unique(financial$name)[i])
}



params_all %>% View



# 구한 서머리값들의 히스토그램 시각화 



p1 <- params_close_all %>% 
  ggplot(aes(mean_close)) + geom_histogram(fill = "red", bins = 100) + scale_x_log10()

p2 <- params_close_all %>% 
  ggplot(aes(max_close)) + geom_histogram(fill = "orange", bins = 100) + scale_x_log10()

p3 <- params_close_all %>% 
  ggplot(aes(sd_close/mean_close)) + geom_histogram(fill = "yellow", bins = 100) + scale_x_log10()

p4 <- params_close_all %>% 
  ggplot(aes(slope)) + geom_histogram(fill = "green", bins = 100) + scale_x_log10()

scale_x_continuous(limits = c(-25,25))    # 슬로프 histogram이 오른쪽으로 기울어져 있다. 

#A left-skewed distribution has a long left tail. 

#Left-skewed distributions are also called negatively-skewed distributions. 

#That’s because there is a long tail in the negative direction on the number line. 

#The mean is also to the left of the peak.



layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)

multiplot(p1, p2, p3, p4, layout=layout)





par_page <- left_join(financial, params_close_all)

ppar_page <- left_join(params_close_all, financial)
par_page2 <- left_join(financial, params_all)





financial$sector %>% unique

sector_names <- c("서비스업", "건설업", "전기가스업", "통신업", "유통업", "어업", "의약품")
sector_names1 <- unique(financial$sector)[1:10]
sector_names2 <- unique(financial$sector)[11:20]
sector_names3 <- unique(financial$sector)[21:30]
sector_names4 <- unique(financial$sector)[31:40]

sector_names5 <- financial$sector %>% table %>% as_tibble %>% arrange(desc(n)) %>% select(1) %>% slice(1:11) %>% as.matrix %>% as.character()

left_join(as_tibble(params_close_all), financial %>% select(name,sector) %>% distinct())

# 업종별로 density 플랏 그려보기 

left_join(as_tibble(params_close_all), financial %>% select(name,sector) %>% distinct()) %>%
  filter(sector %in% sector_names1) %>% 
  ggplot(aes(mean_close, fill = as.factor(sector))) +
  geom_density(position = "stack") 
left_join(as_tibble(params_close_all), financial %>% select(name,sector) %>% distinct()) %>%
  filter(sector %in% sector_names2) %>% 
  ggplot(aes(mean_close, fill = as.factor(sector))) +
  geom_density(position = "stack") 
left_join(as_tibble(params_close_all), financial %>% select(name,sector) %>% distinct()) %>%
  filter(sector %in% sector_names3) %>% 
  ggplot(aes(mean_close, fill = as.factor(sector))) +
  geom_density(position = "stack") 
left_join(as_tibble(params_close_all), financial %>% select(name,sector) %>% distinct()) %>%
  filter(sector %in% sector_names4) %>% 
  ggplot(aes(mean_close, fill = as.factor(sector))) +
  geom_density(position = "stack") 


left_join(as_tibble(params_close_all), financial %>% select(name,sector) %>% distinct()) %>%
  filter(sector %in% sector_names5) %>% 
  ggplot(aes(mean_close, fill = sector)) +
  geom_density(position = "stack") +
  xlim(-10000, 40000)

left_join(as_tibble(params_PER_all), financial %>% select(name,sector) %>% distinct()) %>%
  filter(sector %in% sector_names5) %>% 
  ggplot(aes(mean_PER, fill = sector)) +
  geom_density(position = "identity", alpha = 0.7) 

left_join(as_tibble(params_market_cap_all), financial %>% select(name,sector) %>% distinct()) %>%
  filter(sector %in% sector_names5) %>% 
  ggplot(aes(mean_PER, fill = sector)) +
  geom_density(position = "stack") +
  
  
  p2 <- 
  par_page %>% filter(sector %in% sector_names1) %>%
  ggplot(aes(max_close, fill = sector)) +
  geom_density(position = "stack") +
  scale_x_continuous(breaks = seq(0, 155, 5), lim = c(20, 33)) + 
  theme(legend.position = c(1, 0.8))



p3 <-
  par_page %>% filter(sector %in% sector_names) %>%
  ggplot(aes(sd_close, fill = sector)) +
  geom_density(position = "stack") +
  scale_x_continuous(limits = c(0.1,1)) +
  theme(legend.position = c(1, 0.8))

p4 <- 
  par_page %>% filter(sector %in% sector_names1) %>%
  ggplot(aes(sd_close, fill = sector)) +
  geom_density(position = "stack") +
  scale_x_continuous(limits = c(0.1 , 1)) +
  theme(legend.position = c(1, 0.8)) 





layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)

multiplot(p1, p2, p3, p4, layout=layout)



## Binning - 구간화, 평균화 플랏 -  사실 정확히 뭘 의미하는지는 모르겠음



par_page %>%
  
  ggplot(aes(max_cap, mean_cap)) +
  
  geom_bin2d(bins = c(50,50)) +
  
  scale_x_log10() +
  
  scale_y_log10() +
  
  labs(x = "maximum caps above mean", y = "mean caps")









multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  
  require(grid)
  
  
  
  numPlots = length(plots)
  
  if (is.null(layout)) {
    
    
    
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     
                     ncol = cols, nrow = ceiling(numPlots/cols))
    
  }
  
  
  
  if (numPlots==1) {
    
    print(plots[[1]])
    
    
    
  } else {
    
    grid.newpage()
    
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    
    
    for (i in 1:numPlots) {
      
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      
                                      layout.pos.col = matchidx$col))
      
    }
    
  }
  
}







limx <- c(max(params_all$max_view)/50, max(params_all$max_view))

limy <- c(max(params_all$mean_view)/50, max(params_all$mean_view))

par_page %>%
  
  ggplot(aes(max_view-mean_view, mean_view)) +
  
  geom_point(size = 2, color = "red") +
  
  scale_x_log10(limits = limx) +
  
  scale_y_log10(limits = limy) +
  
  labs(x = "maximum views above mean", y = "mean views") +
  
  geom_label_repel(aes(label = str_c(article, " (",rowname,")")), alpha = 0.5)









### 기울기가 가장 높은 4개 종목 선별해서 살펴보기 



params_all %>% arrange(desc(slope)) %>% head(4) %>% select(name, slope, everything())





p1 <- plot_name_price("나노스")

p2 <- plot_name_price("코스모신소재")

p3 <- plot_name_price("미래컴퍼니")

p4 <- plot_name_price("비에이치")



p1 <- plot_name_market_cap("나노스")

p2 <- plot_name_market_cap("코스모신소재")

p3 <- plot_name_market_cap("미래컴퍼니")

p4 <- plot_name_market_cap("비에이치")



p1 <- plot_name_PER("나노스")

p2 <- plot_name_PER("코스모신소재")

p3 <- plot_name_PER("미래컴퍼니")

p4 <- plot_name_PER("비에이치")



layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)

multiplot(p1, p2, p3, p4, layout=layout)



####################################################################################################################################################################################################################################################################

##### 코드 1  파생변수 생성 최종 



# 파생변수만들기 최종 

financial <- fread("financial_3년치_종가수정_622개종목.csv", encoding = "UTF-8") %>% as_tibble

financial$date <- ymd(financial$date)



getwd()

setwd("D:/user/Desktop")

library(lubridate)

financial <- fread("financial_5개파생변수추가완료_2월18일.csv", encoding = "UTF-8") %>% as_tibble

financial$date <- ymd(financial$date)





#library 및 플랏

plot_name_price <- function(NAME){
  
  ggplot(data = financial %>% filter(name == NAME)
         
         , aes(x = date, y = close)) + 
    
    geom_line(size = 1.3) +
    
    geom_smooth(method = "loess", color = "blue", span = 1/5) +
    
    labs(title = paste(NAME, "종가"))
  
}















### 1) 종가의 분산

financial <- left_join(financial, financial %>% group_by(name) %>% summarise(sd_price = sd(close)))







### 2) IQR 아웃라이어 갯수 









financial <- left_join(financial,
                       
                       financial %>% 
                         
                         group_by(name) %>%
                         
                         summarise(lower_boundary = (quantile(close)[2] - 1.5*(quantile(close)[4] - quantile(close)[2]) ),
                                   
                                   upper_boundary = (quantile(close)[4] + 1.5*(quantile(close)[4] - quantile(close)[2]) )))







outlier_count = matrix(NA, length(names), 2) %>% as.data.frame

names(outlier_count) <- c("name", "outlier_count")

names <- unique(financial$name)



for(i in 1:length(names)){
  
  
  
  dat <- financial %>% filter(name == names[  i  ])
  
  
  
  outlier_count[  i  ,1] = names[  i  ]
  
  outlier_count[  i  ,2] = sum(dat$close < dat$lower_boundary, dat$close > dat$upper_boundary)
  
  
  
  
  
}



financial <- left_join(financial, outlier_count)



((shift(practice$close, n = 1, fill = 0, type = 'lag') - practice$close) >0) %>% sum





practice[is.na(practice),]







### 3)    n 일간의 주가 변화율 



financial <- financial %>% group_by(name) %>%  mutate(max_1day_volatility = max((shift(close, n = 1, fill = 0, type = 'lag')-close)/close))

financial <- financial %>% group_by(name) %>%  mutate(max_3day_volatility = max((shift(close, n = 3, fill = 0, type = 'lag')-close)/close))

financial <- financial %>% group_by(name) %>%  mutate(max_5day_volatility = max((shift(close, n = 5, fill = 0, type = 'lag')-close)/close))

financial <- financial %>% group_by(name) %>%  mutate(max_7day_volatility = max((shift(close, n = 7, fill = 0, type = 'lag')-close)/close))

financial <- financial %>% group_by(name) %>%  mutate(max_10day_volatility = max((shift(close, n = 10, fill = 0, type = 'lag')-close)/close))



financial <- financial %>% group_by(name) %>%  mutate(min_1day_volatility = min((shift(close, n = 1, fill = 1, type = 'lag')-close)/close))

financial <- financial %>% group_by(name) %>%  mutate(min_3day_volatility = min((shift(close, n = 3, fill = 1, type = 'lag')-close)/close))

financial <- financial %>% group_by(name) %>%  mutate(min_5day_volatility = min((shift(close, n = 5, fill = 1, type = 'lag')-close)/close))

financial <- financial %>% group_by(name) %>%  mutate(min_7day_volatility = min((shift(close, n = 7, fill = 1, type = 'lag')-close)/close))

financial <- financial %>% group_by(name) %>%  mutate(min_10day_volatility = min((shift(close, n = 10, fill = 1, type = 'lag')-close)/close))







glimpse(financial)









### 4) 코스피 평균 PER 같은 업종 평균 PER , 그로부터 얼마나 떨어져있는지 

sector_names <- financial$sector %>% unique



# 각 섹터별 평균 PER

financial <- left_join(financial, financial %>% group_by(sector,date) %>% summarise(sector_PER_mean = mean(PER))) 



# 코스피시장 평균 PER 

financial <- 
  
  left_join(financial, 
            
            financial %>% filter(market_category == "코스피") %>% group_by(market_category,date) %>% summarise(kospi_PER_mean = mean(PER)) %>% ungroup %>% select(date,kospi_PER_mean) )









financial <- financial %>% mutate(PER_for_sector = PER/sector_PER_mean,
                                  
                                  PER_for_kospi = PER/kospi_PER_mean)







# 코스피 평균 PER과 , 업종 평균 PER 구하기 

dat <- fread("financial_1635개_3년치종목.csv", encoding = "UTF-8") %>% as_tibble

dat$date <- as.matrix(dat$date) %>% as.vector() 

dat$date %>% head



dat <- filter(date < 20180630)



dat$date <- ymd(dat$date)

dat$date %>% summary





dat <- left_join(dat, dat %>% group_by(sector,date) %>% summarise(sector_PER_mean = mean(PER))) 



dat <- 
  
  left_join(dat, 
            
            dat %>% filter(market_category == "코스피") %>% group_by(market_category,date) %>% summarise(kospi_PER_mean = mean(PER, na.rm = T)) )



dat %>% filter(market_category == "코스피") %>% group_by(market_category,date) %>% summarise(kospi_PER_mean = mean(PER, na.rm = T)) %>% summary

dat %>% group_by(sector,date) %>% summarise(sector_PER_mean = mean(PER, na.rm = T)) %>% summary







### 5) 5일 (1주일)간격으로 기울기를 구하고, 그 변화율을본다. 

financial <- financial %>% group_by(name) %>% mutate(norm_close = scale(close)) %>% ungroup 



names <- financial$name %>% unique



result <- matrix(NA, 622, 131)

names(result)[1] <- "name"

for(i in 1 : length(unique(financial$name))){
  
  
  
  for(j in 1 : round(length(unique(financial$date))/5)){
    
    fit <- summary(lm(norm_close[c(j : c(j+4))] ~ c(j : c(j+4)), data = financial %>% filter(name == names[  i  ]) ))
    
    result[  i  ,  j+1  ] <-  fit$coefficients[2,1]
    
    
    
  }
  
  
  
}  



result[1:3,1:3]

result[,1] <- as.vector(unique(financial$name) )



result <- as.data.frame(result)

names(result)[1] <- "name"



plot(as.numeric(t(result[9,-1])), type = 'l')





## 5-1) 상승할때 5일 기울기 평균, 최대, 같은업종 평균 대비 평균





names(result)[2:131] <- c(1:130)

result <- result[,-1]

result %>% names











result_5days <- matrix(NA, 622, 7) %>% as.data.frame

result_5days %>% head

names(result_5days) <- c("up_5days_slope_mean", "up_5days_slope_max", "up_5days_slope_mean_per_sector",
                         
                         "down_5days_slope_mean", "down_5days_slope_min", "down_5days_slope_mean_per_sector",
                         
                         "name")





result



for(i in 1 : length(unique(financial$name))){
  
  
  
  #상승시
  
  result_5days[  i  ,1] <-  result[  i  , which(as.numeric(t(result[  i  ,])) > 0)] %>% t %>% as.numeric %>% mean
  
  result_5days[  i  ,2] <-  result[  i  , which(as.numeric(t(result[  i  ,])) > 0)] %>% t %>% as.numeric %>% max
  
  
  
  #하락시
  
  result_5days[  i  ,4] <- result[  i  , which(as.numeric(t(result[  i  ,])) < 0)] %>% t %>% as.numeric %>% mean
  
  result_5days[  i  ,5] <- result[  i  , which(as.numeric(t(result[  i  ,])) < 0)] %>% t %>% as.numeric %>% min
  
}





result_5days$name <- unique(financial$name)





result_5days[,-c(3,6)] %>% head



financial <- left_join(financial, result_5days[,-c(3,6)] )

financial









write.csv(financial, "financial_5개파생변수추가완료_2월18일.csv", row.names = F)



### 6) 시가총액 대비 거래대금 
setwd("C:/Users/soong/Desktop/DATA")
financial <-  fread("financial_프로펫적합까지_변수추가한것_2월18일.csv", encoding = "UTF-8") %>% as_tibble

financial %>% names


financial$market_cap %>% summary



### 7)  상장주식수가 변했던 종목들

financial %>% names

financial$shares %>% summary


### 8) 시가총액 구간을 나누고, 그 구간별로 PER의 분산을 구한다 
financial2 <- fread("financial_1635개_3년치종목.csv", encoding = "UTF-8") %>% as_tibble


names <- financial$name %>% unique
financial2 <- financial2 %>% filter(name %in% names)




financial2$market_cap %>% summary

market_cap <- 
  financial2 %>% 
  group_by(name) %>% 
  summarise(mean_market_cap = mean(market_cap)) %>% 
  arrange(desc(mean_market_cap))


market_cap$group <- 0

market_cap$group[467:622] <- 1
market_cap$group[312:466] <- 2
market_cap$group[156:311] <- 3
market_cap$group[1:155] <- 4
market_cap$group <- as.factor(market_cap$group)

financial <- left_join(financial, market_cap[,c(1,3)])

left_join(financial %>% group_by(name) %>% summarise(sd_PER = sd(PER)), market_cap[,c(1,3)])


financial


financial
market_cap
left_join(financial, market_cap) %>% group_by(sector) %>% summarise(mean_market_cap_for_sector = mean(log(market_cap)))


sector_names10 <- left_join(financial, market_cap) %>% distinct(name, .keep_all = T) %>% 
  group_by(sector) %>% summarise(mean = mean(mean_market_cap)) %>% arrange(desc(mean)) %>% select(sector) %>% slice(1:10)


# 업종별 PER 평균
sector_names10 <- sector_names10 %>% as.matrix %>% as.character()
left_join(as_tibble(params_PER_all), financial %>% select(name,sector) %>% distinct()) %>%
  filter(sector %in% sector_names10) %>% 
  ggplot(aes(mean_PER, fill = sector)) +
  geom_density(position = "identity", alpha = 0.5) +
  xlim(c(-10, 40))

# 업종별 PER 분산
left_join(as_tibble(params_PER_all), financial %>% select(name,sector) %>% distinct()) %>%
  filter(sector %in% sector_names10) %>% 
  ggplot(aes(sd_PER, fill = sector)) +
  geom_density(position = "identity", alpha = 0.5) +
  xlim(c(-10, 20))

# 시가총액별 PER 평균 
left_join(financial %>% group_by(name) %>% summarise(mean_PER = mean(PER)), market_cap[,c(1,3)]) %>% 
  ggplot(aes(mean_PER, fill = group)) +
  geom_density(position = "identity", alpha = 0.5)+ 
  xlim(c(-10,60))

# 시가총액별 PER 표준편ㅊ
left_join(financial %>% group_by(name) %>% summarise(sd_PER = sd(PER)), market_cap[,c(1,3)]) %>% 
  ggplot(aes(sd_PER, fill = group)) +
  geom_density(position = "identity", alpha = 0.5) +
  xlim(c(-10,60))



left_join(financial %>% group_by(name) %>% summarise(sd_PER = sd(PER)), market_cap[,c(1,3)]) %>% 
  ggplot(aes(sd_PER, fill = group)) +
  geom_density(position = "identity", alpha) +
  xlim(c(-10,60))

left_join(financial %>% group_by(name) %>% summarise(mean_PER = mean(PER)), market_cap[,c(1,3)]) %>% 
  ggplot(aes(mean_PER, fill = group)) +
  geom_density(position = "identity", alpha = 0.7)+ 
  xlim(c(-10,60))





####################################################################################################################################
####################################################################################################################################
##### 코드 2    군집분석 최종 




###### 최종 군집분석 진행하기


setwd("D:/user/Desktop")
financial <- fread("financial_프로펫적합까지_변수추가한것_2월18일.csv", encoding = "UTF-8") %>% as_tibble
financial$date <- ymd(financial$date)



financial %>% is.na %>% sum



# 한 row가 한 종목이 되도록 정리하자.   -> group_by
# 변수간의 correlation 

library(corrplot)

names <- financial$name %>% unique

c <- cor(financial %>% filter(name == names[1]) %>% select(-1, -2, -13,-14,-15))

corrplot(c, method = "circle")
corrplot(c, method = "pie")
corrplot(c, method = "color")
corrplot(c, method = "number")


financial1 <- 
  financial %>% group_by(name) %>% summarise_at(vars(close, volume, market_cap,
                                                     sd_price, outlier_count), funs(mean))



sector_matrix <- as.data.frame(cbind( unique(financial$sector) , c(1:length(unique(financial$sector))) ))
names(sector_matrix) <- c("sector", "sector_code")
financial <- left_join(financial, sector_matrix)





financial2 <- 
  financial %>% group_by(name) %>% summarise_at(vars(max_5day_volatility, min_5day_volatility ,
                                                     
                                                     PER_for_sector, PER_for_kospi,
                                                     
                                                     up_5days_slope_mean, up_5days_slope_max, 
                                                     
                                                     down_5days_slope_mean, down_5days_slope_min,
                                                     
                                                     MAPE), funs(mean))



financial3 <- 
  financial %>% group_by(name) %>% summarise(sector_code = first(sector_code))



financial_cluster <- inner_join(financial1, financial2) 
financial_cluster <- inner_join(financial_cluster,financial3)
financial_cluster %>% names

par(mfrow = c(1,1))

c <- cor(financial_cluster[,-c(1,2,4,10,
                               
                               11,13,
                               
                               16)])

corrplot(c, method = "circle")

corrplot(c, method = "pie")

corrplot(c, method = "color")

corrplot(c, method = "number")



cor(financial_cluster$market_cap,financial_cluster$close)



# Dissimilarity Matrix 구하기

library(cluster)

financial_cluster %>% names


gower_dist <- daisy(financial_cluster[,-c(1,2,4,10,
                                          
                                          11,13,16)],
                    
                    metric = "gower", #euclidean, manhattan ,gower 
                    
                    stand = FALSE,
                    
                    type = list())

gower_dist <- daisy(financial_cluster[,-c(1,2,4,10,
                                          
                                          11,13)],
                    
                    metric = "gower", #euclidean, manhattan ,gower 
                    
                    stand = FALSE,
                    
                    type = list())

summary(gower_dist)

gower_mat <- as.matrix(gower_dist)



### 최적의 K 찾기  -> 실루엣을 비교해보자

sil_width <- c(NA)

for(i in 2:30){
  
  
  
  pam_fit <- pam(gower_dist,
                 
                 diss = TRUE,
                 
                 k = i)
  
  sil_width[i] <- pam_fit$silinfo$avg.width
  
}

# Plot sihouette width - 높을수록 좋다

plot(1:30, sil_width,
     
     xlab = "Number of clusters",
     
     ylab = "Silhouette Width")

lines(1:30, sil_width)

#  k =  5,    10

# Nbclust 함수를 통한 최적의 군집수 K 찾기 

library("NbClust")





financial_cluster %>% names

nb_result_pam <- NbClust(data = financial_cluster[,-c(1,2,4,10,
                                                      
                                                      11,13
                                                      
)],

distance = NULL,

diss = gower_dist,

min.nc = 2, max.nc = 10, 

method = 'complete')



### PAM clustering 진행

pam_fit <- pam(gower_dist, diss = TRUE, k = 5)

financial_cluster$cluster <- pam_fit$clustering

financial_cluster <- inner_join(financial_cluster, sector_matrix)





# PC로 시각화 

financial_cluster %>% names

pca_financial <- prcomp(as.matrix(financial_cluster[,c(5,6,7,8,12,14,15)]), center = T, scale. = T)





pca_financial %>% summary ; pca_financial$rotation

plot_data <- cbind(as.data.frame(pca_financial$x[, c(1,2)]), cluster = financial_cluster[,"cluster"])

ggplot(plot_data, aes(x = PC1, y = PC2, colour = as.factor(cluster))) +
  
  geom_point()





pca_financial$x[, c(1,2)] %>% View



library("rgl")

pca_financial$x %>% dim

pca_financial$rotation %>% dim

plot3d(pca_financial$x[,1:3], col = financial_cluster$cluster)

# H - clustering

h_fit <- hclust(gower_dist, method = "ward.D2")

plot(h_fit)

nodePar <- list(lab.cex = 0.6, pch = c(NA, 19), cex = 0.7, col = "blue")

library("ape")

colors = c("red", "orange", "yellow", "green", "blue")

clus5 = cutree(h_fit, 5)

plot(as.phylo(hc), type = "fan", tip.color = colors[clus5],
     
     label.offset = 1, cex = 0.7, size = 1)





# K means clustering

set.seed(20)

kmeans_fit <- kmeans(financial_cluster[,c(5:15)], 5, nstart = 20)

financial_cluster$cluster <- kmeans_fit$cluster





plot_data <- cbind(as.data.frame(pca_financial$x[-c(272,366,285,187,386,573,524,146,
                                                    
                                                    509,295,264,146,620,204,98,590), c(1,2)] ),
                   
                   cluster = financial_cluster[-c(272,366,285,187,386,573,524,146,
                                                  
                                                  509,295,264,146,620,204,98,590),"cluster"])

ggplot(plot_data, aes(x = PC1, y = PC2, colour = as.factor(cluster))) +
  geom_point()

ggplot(plot_data, aes(x = log(PC1+4), y = log(PC2+2), colour = as.factor(cluster))) +
  geom_point()+
  xlim(c(0,2.5))




financial_cluster %>% filter(cluster == '4') %>% View





### 각 군집별 통계량을 비교하여 군집별 유형 정의해주기

financial_cluster[financial_cluster$cluster == 1,] %>% summary 

financial_cluster[financial_cluster$cluster == 2,] %>% summary  

financial_cluster[financial_cluster$cluster == 3,] %>% summary  

financial_cluster[financial_cluster$cluster == 4,] %>% summary  

financial_cluster[financial_cluster$cluster == 5,] %>% summary 

financial_cluster[financial_cluster$cluster == 1, "MAPE"] %>% summary 

financial_cluster[financial_cluster$cluster == 2, "MAPE"] %>% summary 

financial_cluster[financial_cluster$cluster == 3, "MAPE"] %>% summary 

financial_cluster[financial_cluster$cluster == 4, "MAPE"] %>% summary  # 4번쨰 군집이 가장 MAPE값이 높다 

financial_cluster[financial_cluster$cluster == 5, "MAPE"] %>% summary



financial_cluster %>% filter(cluster == 4) %>% arrange(desc(MAPE)) %>% View

financial_cluster %>% filter(cluster == 4) %>% arrange(desc(MAPE)) %>% select(name,sector,close,MAPE, sd_price, outlier_count, everything()) %>% View





pca_financial %>% summary

pca_financial$rotation





financial_cluster %>% names

# MAPE & PER_for_sector

plot_data <- cbind(as.data.frame(financial_cluster[,c("MAPE", "PER_for_sector")]), cluster = financial_cluster[,"cluster"])

ggplot(plot_data, aes(x = MAPE, y = PER_for_sector, colour = as.factor(cluster))) +
  
  geom_point()

# MAPE & PER_for_sector

plot_data <- cbind(as.data.frame(financial_cluster[,c("MAPE", "PER_for_kospi")]), cluster = financial_cluster[,"cluster"])

ggplot(plot_data, aes(x = MAPE, y = PER_for_kospi, colour = as.factor(cluster))) +
  
  geom_point()

# down_5days_slope_min & PER_for_sector

plot_data <- cbind(as.data.frame(financial_cluster[,c("down_5days_slope_min", "PER_for_sector")]), cluster = financial_cluster[,"cluster"])

ggplot(plot_data, aes(x = down_5days_slope_min, y = PER_for_sector, colour = as.factor(cluster))) +
  
  geom_point()

# down_5days_slope_min & PER_for_kospi

plot_data <- cbind(as.data.frame(financial_cluster[,c("down_5days_slope_min", "PER_for_kospi")]), cluster = financial_cluster[,"cluster"])

ggplot(plot_data, aes(x = down_5days_slope_min, y = PER_for_kospi, colour = as.factor(cluster))) +
  
  geom_point()







pam_results <- 
  
  sample_data %>%
  
  mutate(cluster = pam_fit$clustering) %>%
  
  group_by(cluster) %>%
  
  do(the_summary = summary(.))





pam_results$the_summary





#중심점들에 있는 obs들을 보여준다.

CLNT_final[pam_fit$medoids, ] %>% View









################################################################################################################################################################
##################################################################################################################################
##### 코드 3  :  prophet 적합 최종 


### 코드2  prophet 적합

result <- r[complete.cases(r), ] 

rownames(result) <- c(1:300)

cor(result$MAPE , result$Market_CAP)





financial <- fread("financial_5개파생변수추가완료_2월18일.csv", encoding = "UTF-8") %>% as_tibble

financial$date <- ymd(financial$date)





celtrion_re <- financial %>% filter(name == "셀트리온") %>% select(date, open, high, low, close, volume)

samchundang_re <- financial %>% filter(name == "삼천당제약") %>% select(date, open, high, low, close, volume)

pillooks_re <- financial %>% filter(name == "필룩스") %>% select(date, open, high, low, close, volume)

woogitoo_re <- financial %>% filter(name == "필룩스") %>% select(date, open, high, low, close, volume)

hansae_re <- financial %>% filter(name == "한세실업") %>% select(date, open, high, low, close, volume)





write.csv(celtrion_re, "celtrion_re.csv", row.names = F)

write.csv(samchundang_re, "samchundang_re.csv", row.names = F)

write.csv(pillooks_re, "pillooks_re.csv", row.names = F)

write.csv(woogitoo_re, "woogitoo_re.csv", row.names = F)

write.csv(hansae_re, "hansae_re.csv", row.names = F)



celtrion_re %>% is.na %>% sum



#############################################################################

### prophet 적합  1 :  using Cross_Validation



library(prophet)





dat1 <- financial %>% filter(name == names[1])

dat1 <- dat1 %>% rename(y = price, ds = date)







# 이렇게 계속해서 외부 변수를 하나하나씩 추가해주는 형식 



m <- prophet(                       #df = train_dat1,
  
  growth = "linear", # logistic  
  
  changepoints = NULL, # 이부분을 구체적으로 명시해주면, 
  
  # 잠재적으로 change point를 알 수 있다
  
  # 명시하지않으면 자동으로 설정된다. 
  
  n.changepoints = 25,
  
  changepoint.range = 0.8,  # prophet은기본적으로 80%크기에서 이걸 지정하므로 디폴트가 0.8이다, 
  
  changepoint.prior.scale = 0.4, # Change Point의 유연성을 조정한다 
  
  
  
  # 유연성이 크다 = 오버피팅이 된다  
  
  
  
  # 이 값이 클 수록 오버피팅이 커진다.
  
  
  
  seasonality.mode = "additive", # 이값을 multiplicative로 해주면 
  
  
  
  # 점점 증가하는 seasonality를 다룰 수 있음 
  
  yearly.seasonality = TRUE,
  
  holidays = NULL, # 홀리데이는 본인이 지정해줄 수 있다 
  
  holidays.prior.scale = 10 # # 홀리데이에 과적합되면 이 값을 조정해주면 된다 
  
  
  
)







#m <- add_regressor(m, 'BPS')

#m <- add_regressor(m, 'EPS')

#m <- add_regressor(m, 'PBR')

m <- add_regressor(m, 'PER', standardize = "auto")

m <- add_regressor(m, 'dividend_yield', standardize = "auto")

m <- add_regressor(m, 'shares', standardize = "auto")

m <- add_regressor(m, 'DPS', standardize = "auto")



# 외부변수들을 하나씩 추가해준 이후, 원래 데이터로 최종 모델 적합 

m <- fit.prophet(m,
                 
                 df = dat1) # 외부변수를 추가했을 때에는 prophet()대신에 fit.prophet()를 써야됨



cv = cross_validation(m, 
                      
                      horizon = 1,  # 이건 언제까지 예측하겠다는ㄴ 뜻임 
                      
                      period = 1,  # 앞으로 n 일씩 계속 예측 
                      
                      units = 'days',
                      
                      initial = 791) # 2018년 3월7일까지 train  / 초반 트레인셋 길이



plot.ts(cv$y, ylim = c(min(dat1$y), max(dat1$y)), col = "red")

lines(cv$yhat, col = 'blue')



(cv$y - cv$yhat)^2 %>% mean %>% sqrt # RMSE

(cv$y - cv$yhat) %>% abs %>% mean # MAE

(cv$y - cv$yhat) %>% abs %>% sd

mean(abs((cv$y - cv$yhat)/cv$y)) # MAPE  





ggplot(data = dat1, aes(ds, y)) + 
  
  geom_line(colour = "red") + 
  
  geom_line(data = cv, aes(ymd(ds), yhat), colour = "blue") 







prophet_plot_components(m, cv)

































#########################################################################################################

### Prophet 적합 2 :     using 일반 적합 



financial





names <- financial$name %>% unique

financial







result_prophet <- matrix(NA, length(names), 4)

result_prophet <- as.data.frame(result_prophet)

names(result_prophet) <- c("RMSE", "MAE",  "MAPE", "name")

result_prophet %>% head



for( i in 1 : length(names)){
  
  
  
  dat1 <- financial %>% filter(name == names[  i  ])
  
  dat1 <- dat1 %>% rename(y = close, ds = date)
  
  
  
  pred_len <- 100
  
  
  
  pred_range <- c(nrow(dat1)-pred_len + 1, nrow(dat1))
  
  train_dat1 <- dat1 %>% head(nrow(dat1) - pred_len)
  
  test_dat1 <- dat1 %>% tail(pred_len)
  
  
  
  train_dat1 <-
    
    train_dat1 %>% select(y,ds,
                          
                          BPS,EPS,PBR,PER,shares, DPS)
  
  test_dat1 <-
    
    test_dat1 %>% select(y,ds,
                         
                         BPS,EPS,PBR,PER,shares, DPS)
  
  #fitting
  
  
  
  m <- prophet(                       #df = train_dat1,
    
    growth = "linear", # logistic  
    
    changepoints = NULL, # 이부분을 구체적으로 명시해주면, 
    
    # 잠재적으로 change point를 알 수 있다
    
    
    
    # 명시하지않으면 자동으로 설정된다. 
    
    
    
    n.changepoints = 25,
    
    
    
    changepoint.range = 0.8,  # prophet은기본적으로 80%크기에서 이걸 지정하므로 디폴트가 0.8이다, 
    
    
    
    changepoint.prior.scale = 0.5, # Change Point의 유연성을 조정한다 
    
    
    
    # 유연성이 크다 = 오버피팅이 된다  
    
    
    
    # 이 값이 클 수록 오버피팅이 커진다.
    
    
    
    
    
    
    
    seasonality.mode = "additive", # 이값을 multiplicative로 해주면 
    
    
    
    # 점점 증가하는 seasonality를 다룰 수 있음 
    
    
    
    
    
    
    
    yearly.seasonality = TRUE,
    
    
    
    holidays = NULL, # 홀리데이는 본인이 지정해줄 수 있다 
    
    
    
    holidays.prior.scale = 10 # # 홀리데이에 과적합되면 이 값을 조정해주면 된다 
    
    
    
  )
  
  
  
  
  
  # 외부변수들을 하나씩 추가해준 이후, 원래 데이터로 최종 모델 적합 
  
  m <- fit.prophet(m,
                   
                   df = train_dat1) 
  
  
  
  future_date <- data.frame(ds = as.Date(dat1$ds))
  
  future_matrix <- left_join(future_date, select(test_dat1, ds))
  
  forecast_matrix <- predict(m, future_matrix)
  
  
  
  
  
  
  
  result_prophet[  i  ,1] <- sqrt(mean( (test_dat1$y - forecast_matrix$yhat[nrow(train_dat1)+1:nrow(test_dat1)])^2 )) # RMSE
  
  result_prophet[  i  ,2] <- mean(abs((test_dat1$y - forecast_matrix$yhat[nrow(train_dat1)+1:nrow(test_dat1)]))) # MAE
  
  result_prophet[  i  ,3] <- mean(abs((test_dat1$y - forecast_matrix$yhat)/test_dat1$y)) # MAPE  
  
  result_prophet[  i  ,4] <- names[  i  ]
  
  
  
}







################################################################################################

# 한종목만 



financial$name %>% unique()

financial$date <- ymd(financial$date)







names <- financial$name %>% unique

dat1 <- financial %>% filter(name == names[  272  ])

dat1 <- dat1 %>% rename(y = close, ds = date)



train_dat1 <- dat1 %>% slice(131:549)

test_dat1 <- dat1 %>% slice(550:649)



train_dat1 <-
  
  train_dat1 %>% select(y,ds,
                        
                        BPS,EPS,PBR,PER,shares, DPS)

test_dat1 <-
  
  test_dat1 %>% select(y,ds,
                       
                       BPS,EPS,PBR,PER,shares, DPS)

#fitting



m <- prophet(                       #df = train_dat1,
  
  growth = "linear", # logistic  
  
  changepoints = NULL, # 이부분을 구체적으로 명시해주면, 
  
  n.changepoints = 25,
  
  changepoint.range = 0.8,  # prophet은기본적으로 80%크기에서 이걸 지정하므로 디폴트가 0.8이다, 
  
  changepoint.prior.scale = 0.5, # Change Point의 유연성을 조정한다 
  
  # 유연성이 크다 = 오버피팅이 된다  
  
  
  
  # 이 값이 클 수록 오버피팅이 커진다.
  
  seasonality.mode = "additive", # 이값을 multiplicative로 해주면 
  
  
  
  # 점점 증가하는 seasonality를 다룰 수 있음 
  
  yearly.seasonality = TRUE,
  
  holidays = NULL, # 홀리데이는 본인이 지정해줄 수 있다 
  
  holidays.prior.scale = 10 # # 홀리데이에 과적합되면 이 값을 조정해주면 된다 
  
)



m <- fit.prophet(m,
                 
                 df = train_dat1) 



future_date <- data.frame(ds = as.Date(dat1$ds))

future_matrix <- left_join(future_date, as.data.frame(select(train_dat1, ds)))

forecast_matrix <- predict(m, future_matrix)





result_prophet[1,1] <- sqrt(mean( (test_dat1$y - forecast_matrix$yhat[nrow(train_dat1)+1:nrow(test_dat1)])^2 )) # RMSE

result_prophet[1,2] <- mean(abs((test_dat1$y - forecast_matrix$yhat[nrow(train_dat1)+1:nrow(test_dat1)]))) # MAE

result_prophet[1,3] <- mean(abs((test_dat1$y - forecast_matrix$yhat)/test_dat1$y)) # MAPE  

result_prophet[1,4] <- names[272]









#result_prophet <- r[complete.cases(r), ] 

#rownames(result) <- c(1:300)

#cor(result$MAPE , result$Market_CAP)



resul









# 기존 데이터와 최종 merge

financial <- left_join(financial, result_prophet)





write.csv(financial, "financial_프로펫적합까지_변수추가한것_2월18일.csv", row.names = F) 

result_prophet











#########

# 한종목씩 해보기 



dat1 <- financial %>% filter(name == names[  50  ]) 

dat1 <- dat1 %>% rename(y = price, ds = date)



pred_len <- 200



pred_range <- c(nrow(dat1)-pred_len + 1, nrow(dat1))

train_dat1 <- dat1 %>% head(nrow(dat1) - pred_len)

test_dat1 <- dat1 %>% tail(pred_len)



train_dat1 <-
  
  train_dat1 %>% select(y,ds,
                        
                        BPS,EPS,PBR,PER,
                        
                        dividend_yield,shares, DPS)

test_dat1 <-
  
  test_dat1 %>% select(y,ds,
                       
                       BPS,EPS,PBR,PER,
                       
                       dividend_yield,shares, DPS)

#fitting



m <- prophet(                       #df = train_dat1,
  
  growth = "linear", # logistic  
  
  changepoints = NULL, # 이부분을 구체적으로 명시해주면, 
  
  # 잠재적으로 change point를 알 수 있다
  
  
  
  # 명시하지않으면 자동으로 설정된다. 
  
  
  
  n.changepoints = 25,
  
  
  
  changepoint.range = 0.8,  # prophet은기본적으로 80%크기에서 이걸 지정하므로 디폴트가 0.8이다, 
  
  
  
  changepoint.prior.scale = 0.4, # Change Point의 유연성을 조정한다 
  
  
  
  # 유연성이 크다 = 오버피팅이 된다  
  
  
  
  # 이 값이 클 수록 오버피팅이 커진다.
  
  
  
  
  
  
  
  seasonality.mode = "additive", # 이값을 multiplicative로 해주면 
  
  
  
  # 점점 증가하는 seasonality를 다룰 수 있음 
  
  
  
  
  
  
  
  yearly.seasonality = TRUE,
  
  
  
  holidays = NULL, # 홀리데이는 본인이 지정해줄 수 있다 
  
  
  
  holidays.prior.scale = 10 # # 홀리데이에 과적합되면 이 값을 조정해주면 된다 
  
  
  
)





# 외부변수들을 하나씩 추가해준 이후, 원래 데이터로 최종 모델 적합 

m <- fit.prophet(m,
                 
                 df = train_dat1) 



future_date <- data.frame(ds = as.Date(dat1$ds))

future_matrix <- left_join(future_date, select(test_dat1, ds))

forecast_matrix <- predict(m, future_matrix)







result3[  50  ,1] <- sqrt(mean( (test_dat1$y - forecast_matrix$yhat[nrow(train_dat1)+1:nrow(test_dat1)])^2 )) # RMSE

result3[  50  ,2] <- mean(abs((test_dat1$y - forecast_matrix$yhat[nrow(train_dat1)+1:nrow(test_dat1)]))) # MAE

result3[  50  ,3] <- mean(abs((test_dat1$y - forecast_matrix$yhat)/test_dat1$y)) # MAPE  

result3[  50  ,4] <- names[  50  ]

result3[  50  ,5] <- financial %>% filter(name == names[  50  ]) %>% group_by(name) %>% summarise(Mean_market_cap = mean(market_cap)) %>% select(Mean_market_cap)



result3[  50  ,]



ggplot(data = dat1, aes(ds, y)) + 
  
  geom_line(colour = "red") + 
  
  geom_line(data = forecast_matrix, aes(ymd(ds), yhat), colour = "blue") 







prophet_plot_components(m, fcst = forecast_matrix,
                        
                        weekly_start = 0,
                        
                        yearly_start = 1,
                        
                        render_plot = TRUE)

