load(url("https://github.com/pbiecek/Diagnoza/raw/master/data/osoby.rda"))
library(plyr)
extract<-na.omit(osoby[,c('plec_all', 'hp52', 'hp53', "waga_2015_ind","klasa_miejscowosci",
                          "wiek2015", "wiek6_2015","hc16",'hp13','hp14',
                          'hp15','hp16','hp17','hp22','hp23',
                          'hp29','hp38','hp39','hp43','hp52',
                          'hp53','hp54n','hp54o','hp54p','hp54q',
                          'hp54r','hp54t','hp54u','hp56','hp62_01',
                          'hp62_02','hp62_03','hp62_04','hp62_05','hp62_06',
                          'hp62_07','hp62_08','hp62_09','hp62_10','hp62_11',
                          'hp62_12','hp62_13','hp62_14','hp62_15','hp65',
                          'hp68_1','hp68_2','hp68_3','hp70','hp71_1',
                          'hp71_2','hp71_3','hp72','hp73','hp74',
                          'hp86','hp95','hp105_14','hp106_01','hc17',
                          'hc11', 'hc30','hd2')])
names(extract)<-c("sex", "height", "weight", "waga", "class",
                  "age", "age6","education","dochody_niestale","dochody_zmartw",
                  "praca_uciaz","praca_obowiaz","praca_niespraw","zdrowie_dolegliw","zdrowie_utrudnia",
                  "hedonizm","kosciol","przyjaciele","pali","wzrost",
                  "waga","Beck_n","Beck_o","Beck_p","Beck_q",
                  "Beck_r","Beck_t","Beck_u","wege","bol_glowy",
                  "bol_brzucha","bol_kark_ramion","bol_kaltka_serce","suchosc","pocenie",
                  "dusznosc","bole_cialo","palpitacje","drgawki","pecherz",
                  "zmeczenie","zaparcia","krew_nos","nadcisnienie","dochod",
                  "ile_rodzina","ile_przyjac","ile_znajomi","tv","kino",
                  "restauracje","spotkanie","psycholog","alkohol","narkotyki",
                  "lamanie_prawa","powazne_choroby","zaufanie_lekarze","nie_uprawia_sportu","lata_nauki",
                  "stan_cywilny","niepelnosprawnosc",'praca')

### Podzia³ zmiennej iloœciowej



### Do ³¹czenia zmiennych u¿yjmy testów statystycznych (Goodman, chi kwadrat, F, V Cramera, tau Kendall)


#Dochody problemy
extract$unstable_income<-as.factor(extract$dochody_niestale)
extract$unstable_income<-revalue(extract$unstable_income, c('1'= '_yes','2'= '_yes','3'= '_no','4'= '_no'))

extract$strenuous_work<-as.factor(extract$praca_uciaz)
extract$strenuous_work<-revalue(extract$strenuous_work, c('1'= '_yes','2'= '_yes','3'= '_no','4'= '_no'))

extract$overloaded_at_work<-as.factor(extract$praca_obowiaz)
extract$overloaded_at_work<-revalue(extract$overloaded_at_work, c('1'= '_yes','2'= '_yes','3'= '_no','4'= '_no'))

extract$unfair_work<-as.factor(extract$praca_niespraw)
extract$unfair_work<-revalue(extract$unfair_work, c('1'= '_yes','2'= '_yes','3'= '_no','4'= '_no'))

extract$income_stressful<-as.factor(extract$dochody_zmartw)
extract$income_stressful<-revalue(extract$income_stressful, c('1'= '_yes','2'= '_yes','3'= '_no'))

#extract$zdrowie_dolegliw_pom<-as.factor(extract$zdrowie_dolegliw)
#extract$zdrowie_dolegliw_<-revalue(extract$zdrowie_dolegliw_pom, c('1'= '_yes','2'= '_yes','3'= '_no'))

#extract$zdrowie_utrudnia_pom<-as.factor(extract$zdrowie_utrudnia)
#extract$zdrowie_utrudnia_<-revalue(extract$zdrowie_utrudnia_pom, c('1'= '_yes','2'= '_yes','3'= '_no'))

### BMI
extract$BMI<-(extract$weight/(extract$height/100)^2)
extract$weight = as.factor(ifelse(extract$BMI<18.5,'_underweight',
                      ifelse(extract$BMI<25,'_normal_weight',
                      ifelse(extract$BMI<30,'_overweight',
                      'obesity'))))
#extract$niedowaga<-as.factor((extract$BMI<18.5)*1)
#extract$normalna_waga<-as.factor((extract$BMI>=18.5 & extract$BMI<25)*1)
#extract$nadwaga<-as.factor((extract$BMI>=25 & extract$BMI<30)*1)
#extract$otylosc<-as.factor((extract$BMI>=30)*1)

### Income
quantile(extract$dochod)
extract$income = as.factor(ifelse(extract$dochod<1100,'_1st_quantile',
                      ifelse(extract$dochod<1530,'_2nd_quantile',
                             ifelse(extract$dochod<2200,'_3rd_quantile',
                                    '_4th_quantile'))))
#extract$Dochod_0_25<-as.factor((extract$dochod<1100)*1)
#extract$Dochod_25_50<-as.factor((extract$dochod>=1100 & extract$dochod<1530)*1)
#extract$Dochod_50_75<-as.factor((extract$dochod>=1530 & extract$dochod<2200)*1)
#extract$Dochod_75_100<-as.factor((extract$dochod>=2200)*1)

### Depression
extract$Beck_SUM<-(extract$Beck_n+extract$Beck_o+extract$Beck_p+extract$Beck_q+extract$Beck_r+extract$Beck_t+extract$Beck_u)
extract$depression<-as.factor((extract$Beck_SUM>=9)*1)
extract$depression = revalue(extract$depression,c('0'='_no','1'='_yes'))
### Kosciol, Kino, Restauracje
extract$goes_to_church <- as.factor((extract$kosciol>0)*1)
extract$goes_to_church = revalue(extract$goes_to_church,c('0'='_no','1'='_yes'))
extract$goes_to_cinema <- as.factor((extract$kino>0)*1)
extract$goes_to_cinema = revalue(extract$goes_to_cinema,c('0'='_no','1'='_yes'))
extract$goes_to_restaurant <- as.factor((extract$restauracje>0)*1)
extract$goes_to_restaurant = revalue(extract$goes_to_restaurant,c('0'='_no','1'='_yes'))
##### nowe rzeczy
extract$class = as.factor(extract$class)
extract$class = revalue(extract$class, c('1'= ' city more than 500 thou.','2'=' city 200-500 thou.','3'=' city 100-200 thou.',
                                         '4'=' city 20-100 thou.','5'=' city less than 20 thou.','6'=' village'))

###### transformacje wiek ######
extract$age = as.factor(ifelse(extract$age<40,'_below_40',
                        ifelse(extract$age<=60,'_between_40_60',
                               '_above_60')))

### alkohol

extract$too_much_alkohol = as.factor(ifelse(extract$alkohol==1,'_yes','_no'))
extract$martial_status = as.factor(ifelse(extract$stan_cywilny==1,'_single',
                     ifelse(extract$stan_cywilny==2,'_married',
                            '_divorced/separated/widowed')))
extract$doing_sports = as.factor(ifelse(extract$nie_uprawia_sportu==2,'_yes','_no'))
extract$drugs = as.factor(ifelse(extract$narkotyki==1,'_yes','_no'))
extract$law_breaking = as.factor(ifelse(extract$lamanie_prawa==1,'_yes','_no'))
extract$job = as.factor(ifelse(extract$praca==1,'_yes','_no'))
extract$smokes = as.factor(as.factor((extract$pali==1)*1))
extract$hedonism = as.factor(ifelse(extract$hedonizm==1,'_yes','_no'))
extract$sex = as.factor(ifelse(extract$sex==1,'male','female'))

#### education
extract$education = as.factor(extract$education)
extract$education_level = revalue(extract$education, c('70'= '_Primary','60'= '_Primary',
                                                   '50'= '_middle','51'= '_middle','40'= '_middle','30'= '_middle','20'= '_middle',
                                                   '12'= '_Higher','11'= '_Higher','10'= '_Higher'))
#czy podzia na 1,2,3 czy na kaÅ¼dy przedzia osobno o 0/1?
extract$friends = as.factor(ifelse(extract$przyjaciele<5,'_small',
                             ifelse(extract$przyjaciele<10,'_medium',
                                    '_many')))

wybrane<-na.omit(extract[,c('smokes','sex','friends',
                            'too_much_alkohol',
                            'martial_status',"doing_sports", "weight",
                            "income",
                            "goes_to_church","goes_to_cinema","goes_to_restaurant","drugs",
                            "law_breaking",
                            "job", "class","age","education_level",
                            "unstable_income","strenuous_work","overloaded_at_work",
                            "unfair_work","income_stressful",
                            "hedonism")])


#wybrane$ <- (wybrane$ ==1)*1
#nie_uprawia_sportu - 1 - nie uprawia, 2 - uprawia sport

#install.packages("caret")
install.packages('e1071')
library(caret)


# To address error message, convert df to data frame object
df <- as.data.frame(wybrane)

### URUCHOM SKRYPT wartosci_skrajne.R - ustaw odpowiednia sciezke 
yourdir = "C:/Users/Tarasiuk/Desktop/smoking-factors/R/"
source(paste0(yourdir,"wartosci_skrajne.R"))

#Partition and create index matrix of selected values
index <- createDataPartition(df_no_outliers$smokes, p=.8, list = FALSE, times = 1)
getActiveDocumentContext()$path


# Create test and training data frames
train_df <- df_no_outliers[index,]
test_df <- df_no_outliers[-index,]

# k-fold cross-validation (10-fold cross-validation) framework
ctrlspecs <- trainControl(method="cv", number=10, savePredictions = "all")

###### Specify & Train LASSO Regression Model
lambda_vector <- 10^seq(5,-5, length=500)

model1 <- train(smokes~ .,
                data = train_df,
                preProcess= c("center", "scale"),
                method="glmnet",
                family = "binomial",
                tuneGrid=expand.grid(alpha=1, lambda=lambda_vector),
                trControl=ctrlspecs,
                na.action = na.omit
              )
ggplot(varImp(model1))


for(i in 1:length(train_df)){
  print(levels(train_df[,i]))
}

#?train
#model1$bestTune
install.packages("ROCR")
library(ROCR)

pred_train <- prediction(predict(model1,type="prob")[2],train_df$smokes)

auc_train <- performance(pred_train, measure = "auc")
auc_train <- auc_train@y.values[[1]]


pred_test <- prediction(predict(model1,newdata = test_df,type="prob")[2],test_df$smokes)

auc_test <- performance(pred_test, measure = "auc")
auc_test <- auc_test@y.values[[1]]