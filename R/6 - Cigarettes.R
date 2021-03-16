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
### 






### Do ≥πczenia zmiennych uøyjmy testÛw statystycznych (Goodman, chi kwadrat, F, V Cramera, tau Kendall)


#Dochody problemy
extract$dochody_niestale_pom<-as.factor(extract$dochody_niestale)
extract$dochody_niestale_<-revalue(extract$dochody_niestale_pom, c('1'= '1','2'= '1','3'= '0','4'= '0'))

extract$praca_uciaz_pom<-as.factor(extract$praca_uciaz)
extract$praca_uciaz_<-revalue(extract$praca_uciaz_pom, c('1'= '1','2'= '1','3'= '0','4'= '0'))

extract$praca_obowiaz_pom<-as.factor(extract$praca_obowiaz)
extract$praca_obowiaz_<-revalue(extract$praca_obowiaz_pom, c('1'= '1','2'= '1','3'= '0','4'= '0'))

extract$praca_niespraw_pom<-as.factor(extract$praca_niespraw)
extract$praca_niespraw_<-revalue(extract$praca_niespraw_pom, c('1'= '1','2'= '1','3'= '0','4'= '0'))

extract$dochody_zmartw_pom<-as.factor(extract$dochody_zmartw)
extract$dochody_zmartw_<-revalue(extract$dochody_zmartw_pom, c('1'= '1','2'= '1','3'= '0'))

extract$zdrowie_dolegliw_pom<-as.factor(extract$zdrowie_dolegliw)
extract$zdrowie_dolegliw_<-revalue(extract$zdrowie_dolegliw_pom, c('1'= '1','2'= '1','3'= '0'))

extract$zdrowie_utrudnia_pom<-as.factor(extract$zdrowie_utrudnia)
extract$zdrowie_utrudnia_<-revalue(extract$zdrowie_utrudnia_pom, c('1'= '1','2'= '1','3'= '0'))

### BMI
extract$BMI<-(extract$weight/(extract$height/100)^2)
extract$niedowaga<-as.factor((extract$BMI<18.5)*1)
extract$normalna_waga<-as.factor((extract$BMI>=18.5 & extract$BMI<25)*1)
extract$nadwaga<-as.factor((extract$BMI>=25 & extract$BMI<30)*1)
extract$otylosc<-as.factor((extract$BMI>=30)*1)
### Income
quantile(extract$dochod)
extract$Dochod_0_25<-as.factor((extract$dochod<1100)*1)
extract$Dochod_25_50<-as.factor((extract$dochod>=1100 & extract$dochod<1530)*1)
extract$Dochod_50_75<-as.factor((extract$dochod>=1530 & extract$dochod<2200)*1)
extract$Dochod_75_100<-as.factor((extract$dochod>=2200)*1)
### Depression
extract$Beck_SUM<-(extract$Beck_n+extract$Beck_o+extract$Beck_p+extract$Beck_q+extract$Beck_r+extract$Beck_t+extract$Beck_u)
extract$Depression<-as.factor((extract$Beck_SUM>=9)*1)
### Kosciol, Kino, Restauracje
extract$Chodzi_Kosciol <- as.factor((extract$kosciol>0)*1)
extract$Chodzi_Kino <- as.factor((extract$kino>0)*1)
extract$Chodzi_Restauracje <- as.factor((extract$restauracje>0)*1)

##### nowe rzeczy
extract$class = as.factor(extract$class)
extract$class = revalue(extract$class, c('1'= ' city more than 500 thou.','2'=' city 200-500 thou.','3'=' city 100-200 thou.',
                                         '4'=' city 20-100 thou.','5'=' city less than 20 thou.','6'=' village'))

###### transformacje wiek ######
extract$age0_40<-as.factor((extract$age<=40)*1)
extract$age40_60<-as.factor((extract$age>40&extract$age<60)*1)
extract$age60plus<-as.factor((extract$age>60)*1)

#### education
extract$education = as.factor(extract$education)
extract$education_level = revalue(extract$education, c('70'= '_Primary','60'= '_Primary',
                                                   '50'= '_middle','51'= '_middle','40'= '_middle','30'= '_middle','20'= '_middle',
                                                   '12'= '_Higher','11'= '_Higher','10'= '_Higher'))
str(extract)
#czy podzia na 1,2,3 czy na ka≈ºdy przedzia osobno o 0/1?

wybrane<-na.omit(extract[,c('pali','sex','przyjaciele',"bol_glowy",
                            "bol_brzucha","bol_kark_ramion","bol_kaltka_serce","suchosc","pocenie",
                            "dusznosc","bole_cialo","palpitacje","drgawki","pecherz",
                            "zmeczenie","zaparcia","krew_nos","nadcisnienie",'alkohol',
                            'stan_cywilny',"nie_uprawia_sportu", "niedowaga", "normalna_waga", "nadwaga", "otylosc",
                            "Dochod_0_25","Dochod_25_50","Dochod_50_75","Dochod_75_100","Depression",
                            "Chodzi_Kosciol","Chodzi_Kino","Chodzi_Restauracje","narkotyki",
                            "lamanie_prawa","powazne_choroby","praca", "class","age0_40","age40_60","age60plus","education_level",
                            "dochody_niestale_","praca_uciaz_","praca_obowiaz_","praca_niespraw_","dochody_zmartw_","zdrowie_dolegliw_","zdrowie_utrudnia_",
                            "hedonizm")])
{
wybrane$class<-as.factor(wybrane$class)
wybrane$education_level<-as.factor(wybrane$education_level)
wybrane$dochody_niestale_<-as.factor(wybrane$dochody_niestale_)
wybrane$praca_uciaz_<-as.factor(wybrane$praca_uciaz_)
wybrane$praca_obowiaz_<-as.factor(wybrane$praca_obowiaz_)
wybrane$praca_niespraw_<-as.factor(wybrane$praca_niespraw_)
wybrane$dochody_zmartw_<-as.factor(wybrane$dochody_zmartw_)
wybrane$zdrowie_dolegliw_<-as.factor(wybrane$zdrowie_dolegliw_)
wybrane$zdrowie_utrudnia_<-as.factor(wybrane$zdrowie_utrudnia_)
wybrane$hedonizm<-as.factor((extract$hedonizm==1)*1)
wybrane$sex<-as.factor((extract$sex==1)*1)
wybrane$pali<-as.factor((wybrane$pali==1)*1)
wybrane$bol_glowy <- as.factor((wybrane$bol_glowy ==1)*1)
wybrane$bol_brzucha <- as.factor((wybrane$bol_brzucha==1)*1)
wybrane$bol_kark_ramion <- as.factor((wybrane$bol_kark_ramion ==1)*1)
wybrane$bol_kaltka_serce <- as.factor((wybrane$bol_kaltka_serce ==1)*1)
wybrane$suchosc <- as.factor((wybrane$suchosc ==1)*1)
wybrane$pocenie <- as.factor((wybrane$pocenie ==1)*1)
wybrane$dusznosc <- as.factor((wybrane$dusznosc ==1)*1)
wybrane$bole_cialo <- as.factor((wybrane$bole_cialo ==1)*1)
wybrane$palpitacje <- as.factor((wybrane$palpitacje ==1)*1)
wybrane$drgawki <- as.factor((wybrane$drgawki ==1)*1)
wybrane$pecherz <- as.factor((wybrane$pecherz ==1)*1)
wybrane$zmeczenie <- as.factor((wybrane$zmeczenie ==1)*1)
wybrane$zaparcia <- as.factor((wybrane$zaparcia ==1)*1)
wybrane$krew_nos <- as.factor((wybrane$krew_nos ==1)*1)
wybrane$nadcisnienie <- as.factor((wybrane$nadcisnienie ==1)*1)
wybrane$alkohol <- as.factor((wybrane$alkohol ==1)*1)
wybrane$stan_cywilny <- as.factor((wybrane$stan_cywilny ==1)*1)
wybrane$nie_uprawia_sportu <- as.factor((wybrane$nie_uprawia_sportu ==1)*1)
wybrane$narkotyki <- as.factor((wybrane$narkotyki ==1)*1)
wybrane$lamanie_prawa <- as.factor((wybrane$lamanie_prawa ==1)*1)
wybrane$powazne_choroby <- as.factor((wybrane$powazne_choroby ==1)*1)
wybrane$praca <- as.factor((wybrane$praca ==1)*1)
}
#str(wybrane)


#wybrane$ <- (wybrane$ ==1)*1
#nie_uprawia_sportu - 1 - nie uprawia, 2 - uprawia sport

#install.packages("caret")
install.packages('e1071')
library(caret)


#Partition and create index matrix of selected values
index <- createDataPartition(wybrane$pali, p=.8, list = FALSE, times = 1)

# To address error message, convert df to data frame object
df <- as.data.frame(wybrane)

# Create test and training data frames
train_df <- wybrane[index,]
test_df <- wybrane[-index,]

# k-fold cross-validation (10-fold cross-validation) framework
ctrlspecs <- trainControl(method="cv", number=10, savePredictions = "all")

###### Specify & Train LASSO Regression Model
lambda_vector <- 10^seq(5,-5, length=500)

model1 <- train(pali~ .,
                data = train_df,
                preProcess= c("center", "scale"),
                method="glmnet",
                family = "binomial",
                tuneGrid=expand.grid(alpha=1, lambda=lambda_vector),
                trControl=ctrlspecs,
                na.action = na.omit
              )
ggplot(varImp(model1))


#?train
#model1$bestTune
install.packages("ROCR")
library(ROCR)

pred_train <- prediction(predict(model1,type="prob")[2],train_df$pali)

auc_train <- performance(pred_train, measure = "auc")
auc_train <- auc_train@y.values[[1]]


pred_test <- prediction(predict(model1,newdata = test_df,type="prob")[2],test_df$pali)

auc_test <- performance(pred_test, measure = "auc")
auc_test <- auc_test@y.values[[1]]

### dlaczego trzeba przeanalizowaÊ korelacjÍ zmiennych - przyk≥ad
table(test_df$age60plus,test_df$pali)
table(test_df$sex,test_df$pali)

### øeby nie zaszumiaÊ modelu, wyrzuÊmy od razu zmienne nieistotne :
table(test_df$bol_kark_ramion,test_df$pali)



