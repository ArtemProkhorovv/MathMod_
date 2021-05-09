#Прохоров ДХ-121
#Создайте модель множественной линейной регрессии потоков углекислого газа 
#за летний период 2013 года по данным измерений методом турбулентной пульсации

rm(list=ls())
setwd("C:/Math_Mod_Prokhorov/MathMod")
getwd()

library(tidyverse)
library(stringr)
library(lubridate)
library(ggplot2)

eddy=read_csv("eddypro.csv", skip=1, na=c(" ","NA","-9999","-9999.0"), comment=c("[")); 
eddy=eddy[-1,]
eddy=select(eddy,-(roll))
eddy=eddy %>% mutate_if(is.character, factor)

names(eddy) = names(eddy) %>%
  str_replace_all("[!]","_exclam_") %>%
  str_replace_all("[?]","_quest_") %>%
  str_replace_all("[*]","_star_") %>%
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_slash_") %>%
  str_replace_all("[%]","_pecent_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_")
glimpse(eddy)

eddy = mutate(eddy, month = month(date))

eddy_num=filter(eddy, month>=6 & month<=8)[,sapply(eddy, is.numeric)]
eddy_non_num=eddy[,!sapply(eddy, is.numeric)]

cor_td=cor(drop_na(eddy_num)) %>% as.data.frame %>% select(co2_flux)
cor_td=cbind(cor_td,adjusted=sqrt(cor_td$co2_flux^2))
vars=row.names(cor_td)[cor_td$co2_flux^2>.1] %>% na.exclude()
correlation_formula=as.formula(paste("co2_flux~",paste(vars,collapse="+"),sep=" "))


model1=lm(correlation_formula,data=eddy_num)

names(model1)

summary(model1)

anova(model1)


#удаляем малозначимые показатели ориентируемся по показателю R^2 и Pr(>F)
cor_lrm=cor(drop_na(eddy_num) %>% as.data.frame %>% select(vars))
cor_lrm=sqrt(cor_lrm^2)
model3 = lm(data = eddy_num, co2_flux~ (Tau+LE+rand_err_LE+rand_err_h2o_flux+H_strg+co2_molar_density+
              co2_mole_fraction+air_density+air_molar_volume+es+max_speed+TKE+
              un_Tau+un_LE+un_h2o_flux+co2)^2)

summary(model3)
anova(model3)
#Для модели - 3 показатель R^2 =0,84, продолжаем исключать малозначимые переменные
model4 = lm(data = eddy_num, co2_flux~ (Tau+LE+rand_err_LE+rand_err_h2o_flux+H_strg+co2_molar_density+
                                          co2_mole_fraction+air_density+air_molar_volume+es+max_speed+TKE+
                                          un_Tau+un_LE+un_h2o_flux+co2)^2 -LE:co2   - H_strg:air_density -
                                          H_strg:air_molar_volume - co2_mole_fraction - un_Tau - Tau:LE - Tau:rand_err_h2o_flux
            - Tau:H_strg - Tau:co2_molar_density - Tau:co2_mole_fraction - Tau:air_density - Tau:co2_mole_fraction -
              Tau:air_density - Tau:es - Tau:TKE - LE:co2_mole_fraction - LE:un_LE - rand_err_LE:rand_err_h2o_flux - 
              rand_err_LE:H_strg - rand_err_LE:co2_mole_fraction - rand_err_LE:air_molar_volume - rand_err_LE:max_speed -
              rand_err_LE:TKE -rand_err_LE:un_h2o_flux - rand_err_h2o_flux:air_molar_volume - rand_err_LE:max_speed - 
              rand_err_LE:TKE - rand_err_LE:un_h2o_flux - rand_err_h2o_flux:air_molar_volume - rand_err_h2o_flux:es - rand_err_h2o_flux:es-
              rand_err_h2o_flux:un_Tau - rand_err_h2o_flux:un_h2o_flux - H_strg:co2_mole_fraction - H_strg:air_density -
              H_strg:max_speed - H_strg:TKE - H_strg:un_Tau - H_strg:un_h2o_flux - co2_molar_density:co2_mole_fraction - 
              co2_molar_density:air_density - co2_molar_density:air_molar_volume - co2_molar_density:es - co2_molar_density:un_Tau - 
              co2_mole_fraction:air_molar_volume -co2_mole_fraction:es - co2_mole_fraction:max_speed - co2_mole_fraction:TKE -
              co2_mole_fraction:un_Tau - co2_mole_fraction:un_h2o_flux - air_density:max_speed - air_density:TKE - air_density:un_Tau -
              air_density:un_h2o_flux - air_density:co2 - air_molar_volume:es - air_molar_volume:max_speed - air_molar_volume:TKE -
              air_molar_volume:un_Tau - air_molar_volume:un_LE - air_molar_volume:un_h2o_flux - es:max_speed - es:TKE - es:un_Tau -
              es:un_LE - es:un_h2o_flux - max_speed:TKE - max_speed:un_Tau - max_speed:un_LE - max_speed:un_h2o_flux - max_speed:co2 -
              TKE:un_Tau - TKE:un_LE - TKE:un_h2o_flux - TKE:co2 - un_Tau:un_LE - un_Tau:un_h2o_flux - un_Tau:co2 -
              un_LE:co2 - un_h2o_flux:co2 )

summary(model4)
anova(model4)
#Для модели - 4 показатель R^2 =0,81, продолжаем исключать малозначимые переменные
model5=lm(data  = eddy_num, co2_flux~ (Tau+LE+rand_err_LE+rand_err_h2o_flux+H_strg+co2_molar_density+
                                        co2_mole_fraction+air_density+air_molar_volume+es+max_speed+TKE+
                                        un_Tau+un_LE+un_h2o_flux+co2)^2 -LE:co2   - H_strg:air_density -
            H_strg:air_molar_volume - co2_mole_fraction - un_Tau - Tau:LE - Tau:rand_err_h2o_flux
          - Tau:H_strg - Tau:co2_molar_density - Tau:co2_mole_fraction - Tau:air_density - Tau:co2_mole_fraction -
            Tau:air_density - Tau:es - Tau:TKE - LE:co2_mole_fraction - LE:un_LE - rand_err_LE:rand_err_h2o_flux - 
            rand_err_LE:H_strg - rand_err_LE:co2_mole_fraction - rand_err_LE:air_molar_volume - rand_err_LE:max_speed -
            rand_err_LE:TKE -rand_err_LE:un_h2o_flux - rand_err_h2o_flux:air_molar_volume - rand_err_LE:max_speed - 
            rand_err_LE:TKE - rand_err_LE:un_h2o_flux - rand_err_h2o_flux:air_molar_volume - rand_err_h2o_flux:es - rand_err_h2o_flux:es-
            rand_err_h2o_flux:un_Tau - rand_err_h2o_flux:un_h2o_flux - H_strg:co2_mole_fraction - H_strg:air_density -
            H_strg:max_speed - H_strg:TKE - H_strg:un_Tau - H_strg:un_h2o_flux - co2_molar_density:co2_mole_fraction - 
            co2_molar_density:air_density - co2_molar_density:air_molar_volume - co2_molar_density:es - co2_molar_density:un_Tau - 
            co2_mole_fraction:air_molar_volume -co2_mole_fraction:es - co2_mole_fraction:max_speed - co2_mole_fraction:TKE -
            co2_mole_fraction:un_Tau - co2_mole_fraction:un_h2o_flux - air_density:max_speed - air_density:TKE - air_density:un_Tau -
            air_density:un_h2o_flux - air_density:co2 - air_molar_volume:es - air_molar_volume:max_speed - air_molar_volume:TKE -
            air_molar_volume:un_Tau - air_molar_volume:un_LE - air_molar_volume:un_h2o_flux - es:max_speed - es:TKE - es:un_Tau -
            es:un_LE - es:un_h2o_flux - max_speed:TKE - max_speed:un_Tau - max_speed:un_LE - max_speed:un_h2o_flux - max_speed:co2 -
            TKE:un_Tau - TKE:un_LE - TKE:un_h2o_flux - TKE:co2 - un_Tau:un_LE - un_Tau:un_h2o_flux - un_Tau:co2 -
            un_LE:co2 - un_h2o_flux:co2 - air_molar_volume - Tau:air_molar_volume - Tau:max_speed - rand_err_LE:co2_molar_density -
            rand_err_LE:air_density - rand_err_LE:un_Tau - rand_err_LE:co2 - rand_err_h2o_flux:max_speed - rand_err_h2o_flux:TKE -
            rand_err_h2o_flux:un_LE - co2_molar_density:co2 - co2_mole_fraction:air_density - co2_mole_fraction:un_LE -
            co2_mole_fraction:co2 - air_density:air_molar_volume - air_density:es - es:co2)
summary(model5)
anova(model5)
#Для модели - 5 показатель R^2 =0,80, продолжаем исключать малозначимые переменные
model6=lm(data  = eddy_num, co2_flux~ (Tau+LE+rand_err_LE+rand_err_h2o_flux+H_strg+co2_molar_density+
                                         co2_mole_fraction+air_density+air_molar_volume+es+max_speed+TKE+
                                         un_Tau+un_LE+un_h2o_flux+co2)^2 -LE:co2   - H_strg:air_density -
            H_strg:air_molar_volume - co2_mole_fraction - un_Tau - Tau:LE - Tau:rand_err_h2o_flux
          - Tau:H_strg - Tau:co2_molar_density - Tau:co2_mole_fraction - Tau:air_density - Tau:co2_mole_fraction -
            Tau:air_density - Tau:es - Tau:TKE - LE:co2_mole_fraction - LE:un_LE - rand_err_LE:rand_err_h2o_flux - 
            rand_err_LE:H_strg - rand_err_LE:co2_mole_fraction - rand_err_LE:air_molar_volume - rand_err_LE:max_speed -
            rand_err_LE:TKE -rand_err_LE:un_h2o_flux - rand_err_h2o_flux:air_molar_volume - rand_err_LE:max_speed - 
            rand_err_LE:TKE - rand_err_LE:un_h2o_flux - rand_err_h2o_flux:air_molar_volume - rand_err_h2o_flux:es - rand_err_h2o_flux:es-
            rand_err_h2o_flux:un_Tau - rand_err_h2o_flux:un_h2o_flux - H_strg:co2_mole_fraction - H_strg:air_density -
            H_strg:max_speed - H_strg:TKE - H_strg:un_Tau - H_strg:un_h2o_flux - co2_molar_density:co2_mole_fraction - 
            co2_molar_density:air_density - co2_molar_density:air_molar_volume - co2_molar_density:es - co2_molar_density:un_Tau - 
            co2_mole_fraction:air_molar_volume -co2_mole_fraction:es - co2_mole_fraction:max_speed - co2_mole_fraction:TKE -
            co2_mole_fraction:un_Tau - co2_mole_fraction:un_h2o_flux - air_density:max_speed - air_density:TKE - air_density:un_Tau -
            air_density:un_h2o_flux - air_density:co2 - air_molar_volume:es - air_molar_volume:max_speed - air_molar_volume:TKE -
            air_molar_volume:un_Tau - air_molar_volume:un_LE - air_molar_volume:un_h2o_flux - es:max_speed - es:TKE - es:un_Tau -
            es:un_LE - es:un_h2o_flux - max_speed:TKE - max_speed:un_Tau - max_speed:un_LE - max_speed:un_h2o_flux - max_speed:co2 -
            TKE:un_Tau - TKE:un_LE - TKE:un_h2o_flux - TKE:co2 - un_Tau:un_LE - un_Tau:un_h2o_flux - un_Tau:co2 -
            un_LE:co2 - un_h2o_flux:co2 - air_molar_volume - Tau:air_molar_volume - Tau:max_speed - rand_err_LE:co2_molar_density -
            rand_err_LE:air_density - rand_err_LE:un_Tau - rand_err_LE:co2 - rand_err_h2o_flux:max_speed - rand_err_h2o_flux:TKE -
            rand_err_h2o_flux:un_LE - co2_molar_density:co2 - co2_mole_fraction:air_density - co2_mole_fraction:un_LE -
            co2_mole_fraction:co2 - air_density:air_molar_volume - air_density:es - es:co2 - Tau:rand_err_LE - Tau:un_Tau -
            Tau:un_h2o_flux - LE:un_h2o_flux - rand_err_h2o_flux:co2_molar_density - rand_err_h2o_flux:co2_molar_density -
            rand_err_h2o_flux:co2 - H_strg:es - co2_molar_density:TKE - air_density:un_LE - un_LE:un_h2o_flux)
summary(model6)
anova(model6)
#Для модели - 6 показатель R^2 =0,79, продолжаем исключать малозначимые переменные

model7=lm(data  = eddy_num, co2_flux~ (Tau+LE+rand_err_LE+rand_err_h2o_flux+H_strg+co2_molar_density+
                                         co2_mole_fraction+air_density+air_molar_volume+es+max_speed+TKE+
                                         un_Tau+un_LE+un_h2o_flux+co2)^2 -LE:co2   - H_strg:air_density -
            H_strg:air_molar_volume - co2_mole_fraction - un_Tau - Tau:LE - Tau:rand_err_h2o_flux
          - Tau:H_strg - Tau:co2_molar_density - Tau:co2_mole_fraction - Tau:air_density - Tau:co2_mole_fraction -
            Tau:air_density - Tau:es - Tau:TKE - LE:co2_mole_fraction - LE:un_LE - rand_err_LE:rand_err_h2o_flux - 
            rand_err_LE:H_strg - rand_err_LE:co2_mole_fraction - rand_err_LE:air_molar_volume - rand_err_LE:max_speed -
            rand_err_LE:TKE -rand_err_LE:un_h2o_flux - rand_err_h2o_flux:air_molar_volume - rand_err_LE:max_speed - 
            rand_err_LE:TKE - rand_err_LE:un_h2o_flux - rand_err_h2o_flux:air_molar_volume - rand_err_h2o_flux:es - rand_err_h2o_flux:es-
            rand_err_h2o_flux:un_Tau - rand_err_h2o_flux:un_h2o_flux - H_strg:co2_mole_fraction - H_strg:air_density -
            H_strg:max_speed - H_strg:TKE - H_strg:un_Tau - H_strg:un_h2o_flux - co2_molar_density:co2_mole_fraction - 
            co2_molar_density:air_density - co2_molar_density:air_molar_volume - co2_molar_density:es - co2_molar_density:un_Tau - 
            co2_mole_fraction:air_molar_volume -co2_mole_fraction:es - co2_mole_fraction:max_speed - co2_mole_fraction:TKE -
            co2_mole_fraction:un_Tau - co2_mole_fraction:un_h2o_flux - air_density:max_speed - air_density:TKE - air_density:un_Tau -
            air_density:un_h2o_flux - air_density:co2 - air_molar_volume:es - air_molar_volume:max_speed - air_molar_volume:TKE -
            air_molar_volume:un_Tau - air_molar_volume:un_LE - air_molar_volume:un_h2o_flux - es:max_speed - es:TKE - es:un_Tau -
            es:un_LE - es:un_h2o_flux - max_speed:TKE - max_speed:un_Tau - max_speed:un_LE - max_speed:un_h2o_flux - max_speed:co2 -
            TKE:un_Tau - TKE:un_LE - TKE:un_h2o_flux - TKE:co2 - un_Tau:un_LE - un_Tau:un_h2o_flux - un_Tau:co2 -
            un_LE:co2 - un_h2o_flux:co2 - air_molar_volume - Tau:air_molar_volume - Tau:max_speed - rand_err_LE:co2_molar_density -
            rand_err_LE:air_density - rand_err_LE:un_Tau - rand_err_LE:co2 - rand_err_h2o_flux:max_speed - rand_err_h2o_flux:TKE -
            rand_err_h2o_flux:un_LE - co2_molar_density:co2 - co2_mole_fraction:air_density - co2_mole_fraction:un_LE -
            co2_mole_fraction:co2 - air_density:air_molar_volume - air_density:es - es:co2 - Tau:rand_err_LE - Tau:un_Tau -
            Tau:un_h2o_flux - LE:un_h2o_flux - rand_err_h2o_flux:co2_molar_density - rand_err_h2o_flux:co2_molar_density -
            rand_err_h2o_flux:co2 - H_strg:es - co2_molar_density:TKE - air_density:un_LE - un_LE:un_h2o_flux - Tau:un_LE -
            LE:H_strg - rand_err_h2o_flux:co2_mole_fraction - air_molar_volume:co2)
summary(model7)
anova(model7)
#Для модели - 7 показатель R^2 =0,79, продолжаем исключать малозначимые переменные
model8=lm(data  = eddy_num, co2_flux~ (Tau+LE+rand_err_LE+rand_err_h2o_flux+H_strg+co2_molar_density+
                                         co2_mole_fraction+air_density+air_molar_volume+es+max_speed+TKE+
                                         un_Tau+un_LE+un_h2o_flux+co2)^2 -LE:co2   - H_strg:air_density -
            H_strg:air_molar_volume - co2_mole_fraction - un_Tau - Tau:LE - Tau:rand_err_h2o_flux
          - Tau:H_strg - Tau:co2_molar_density - Tau:co2_mole_fraction - Tau:air_density - Tau:co2_mole_fraction -
            Tau:air_density - Tau:es - Tau:TKE - LE:co2_mole_fraction - LE:un_LE - rand_err_LE:rand_err_h2o_flux - 
            rand_err_LE:H_strg - rand_err_LE:co2_mole_fraction - rand_err_LE:air_molar_volume - rand_err_LE:max_speed -
            rand_err_LE:TKE -rand_err_LE:un_h2o_flux - rand_err_h2o_flux:air_molar_volume - rand_err_LE:max_speed - 
            rand_err_LE:TKE - rand_err_LE:un_h2o_flux - rand_err_h2o_flux:air_molar_volume - rand_err_h2o_flux:es - rand_err_h2o_flux:es-
            rand_err_h2o_flux:un_Tau - rand_err_h2o_flux:un_h2o_flux - H_strg:co2_mole_fraction - H_strg:air_density -
            H_strg:max_speed - H_strg:TKE - H_strg:un_Tau - H_strg:un_h2o_flux - co2_molar_density:co2_mole_fraction - 
            co2_molar_density:air_density - co2_molar_density:air_molar_volume - co2_molar_density:es - co2_molar_density:un_Tau - 
            co2_mole_fraction:air_molar_volume -co2_mole_fraction:es - co2_mole_fraction:max_speed - co2_mole_fraction:TKE -
            co2_mole_fraction:un_Tau - co2_mole_fraction:un_h2o_flux - air_density:max_speed - air_density:TKE - air_density:un_Tau -
            air_density:un_h2o_flux - air_density:co2 - air_molar_volume:es - air_molar_volume:max_speed - air_molar_volume:TKE -
            air_molar_volume:un_Tau - air_molar_volume:un_LE - air_molar_volume:un_h2o_flux - es:max_speed - es:TKE - es:un_Tau -
            es:un_LE - es:un_h2o_flux - max_speed:TKE - max_speed:un_Tau - max_speed:un_LE - max_speed:un_h2o_flux - max_speed:co2 -
            TKE:un_Tau - TKE:un_LE - TKE:un_h2o_flux - TKE:co2 - un_Tau:un_LE - un_Tau:un_h2o_flux - un_Tau:co2 -
            un_LE:co2 - un_h2o_flux:co2 - air_molar_volume - Tau:air_molar_volume - Tau:max_speed - rand_err_LE:co2_molar_density -
            rand_err_LE:air_density - rand_err_LE:un_Tau - rand_err_LE:co2 - rand_err_h2o_flux:max_speed - rand_err_h2o_flux:TKE -
            rand_err_h2o_flux:un_LE - co2_molar_density:co2 - co2_mole_fraction:air_density - co2_mole_fraction:un_LE -
            co2_mole_fraction:co2 - air_density:air_molar_volume - air_density:es - es:co2 - Tau:rand_err_LE - Tau:un_Tau -
            Tau:un_h2o_flux - LE:un_h2o_flux - rand_err_h2o_flux:co2_molar_density - rand_err_h2o_flux:co2_molar_density -
            rand_err_h2o_flux:co2 - H_strg:es - co2_molar_density:TKE - air_density:un_LE - un_LE:un_h2o_flux - Tau:un_LE -
            LE:H_strg - rand_err_h2o_flux:co2_mole_fraction - air_molar_volume:co2 - LE:max_speed - LE:un_Tau - rand_err_h2o_flux:H_strg -
            H_strg:un_LE)
summary(model8)
anova(model8)
plot(model8)
#Для модели - 8 показатель R^2 =0,78, попробуем исключить еще ряд независимых переменных
#Для модели - 8 показатель R^2 =0,78, остатки рассеяны равномерно, распределение данных близкое к нормальному;
#За пределы диопазона Кука выходит одно значение зависимой переменной которе следует отбросить;
#Графическая интерпертация  множественной линейной регрессии потоков углекислого газа 
#за летний период 2013 года по данным измерений методом турбулентной пульсации (model8)
qplot(Tau+LE+rand_err_h2o_flux+H_strg+co2_molar_density+air_density+
        es+max_speed+TKE+un_LE+un_h2o_flux+co2+Tau:co2+
        LE:rand_err_LE+LE:rand_err_h2o_flux+LE:co2_molar_density+LE:air_density+LE:air_molar_volume+
        LE:es+LE:TKE+rand_err_LE:es+rand_err_LE:un_LE+rand_err_h2o_flux:air_density+
        co2_molar_density:max_speed+co2_molar_density:un_LE+co2_molar_density:un_h2o_flux,co2_flux,data=eddy,alpha=I(1/4))+geom_smooth(se=FALSE, method=lm)+theme_bw()

#Для модели - 8 показатель R^2 =0,78, попробуем исключить еще ряд независимых переменных + 
#добавим ошибочно исключенную w_slash_co2_cov, оптимизируем данные
#R^2=0.99 охват дисперии повысился - далее необходимо упростить модель
model9=lm(data  = eddy_num, co2_flux~ (w_slash_co2_cov+Tau+LE+rand_err_LE+rand_err_h2o_flux+H_strg+co2_molar_density+
                                         co2_mole_fraction+air_density+air_molar_volume+es+max_speed+TKE+
                                         un_Tau+un_LE+un_h2o_flux+co2)^2 -LE:co2   - H_strg:air_density -
            H_strg:air_molar_volume - co2_mole_fraction - un_Tau - Tau:LE - Tau:rand_err_h2o_flux
          - Tau:H_strg - Tau:co2_molar_density - Tau:co2_mole_fraction - Tau:air_density - Tau:co2_mole_fraction -
            Tau:air_density - Tau:es - Tau:TKE - LE:co2_mole_fraction - LE:un_LE - rand_err_LE:rand_err_h2o_flux - 
            rand_err_LE:H_strg - rand_err_LE:co2_mole_fraction - rand_err_LE:air_molar_volume - rand_err_LE:max_speed -
            rand_err_LE:TKE -rand_err_LE:un_h2o_flux - rand_err_h2o_flux:air_molar_volume - rand_err_LE:max_speed - 
            rand_err_LE:TKE - rand_err_LE:un_h2o_flux - rand_err_h2o_flux:air_molar_volume - rand_err_h2o_flux:es - rand_err_h2o_flux:es-
            rand_err_h2o_flux:un_Tau - rand_err_h2o_flux:un_h2o_flux - H_strg:co2_mole_fraction - H_strg:air_density -
            H_strg:max_speed - H_strg:TKE - H_strg:un_Tau - H_strg:un_h2o_flux - co2_molar_density:co2_mole_fraction - 
            co2_molar_density:air_density - co2_molar_density:air_molar_volume - co2_molar_density:es - co2_molar_density:un_Tau - 
            co2_mole_fraction:air_molar_volume -co2_mole_fraction:es - co2_mole_fraction:max_speed - co2_mole_fraction:TKE -
            co2_mole_fraction:un_Tau - co2_mole_fraction:un_h2o_flux - air_density:max_speed - air_density:TKE - air_density:un_Tau -
            air_density:un_h2o_flux - air_density:co2 - air_molar_volume:es - air_molar_volume:max_speed - air_molar_volume:TKE -
            air_molar_volume:un_Tau - air_molar_volume:un_LE - air_molar_volume:un_h2o_flux - es:max_speed - es:TKE - es:un_Tau -
            es:un_LE - es:un_h2o_flux - max_speed:TKE - max_speed:un_Tau - max_speed:un_LE - max_speed:un_h2o_flux - max_speed:co2 -
            TKE:un_Tau - TKE:un_LE - TKE:un_h2o_flux - TKE:co2 - un_Tau:un_LE - un_Tau:un_h2o_flux - un_Tau:co2 -
            un_LE:co2 - un_h2o_flux:co2 - air_molar_volume - Tau:air_molar_volume - Tau:max_speed - rand_err_LE:co2_molar_density -
            rand_err_LE:air_density - rand_err_LE:un_Tau - rand_err_LE:co2 - rand_err_h2o_flux:max_speed - rand_err_h2o_flux:TKE -
            rand_err_h2o_flux:un_LE - co2_molar_density:co2 - co2_mole_fraction:air_density - co2_mole_fraction:un_LE -
            co2_mole_fraction:co2 - air_density:air_molar_volume - air_density:es - es:co2 - Tau:rand_err_LE - Tau:un_Tau -
            Tau:un_h2o_flux - LE:un_h2o_flux - rand_err_h2o_flux:co2_molar_density - rand_err_h2o_flux:co2_molar_density -
            rand_err_h2o_flux:co2 - H_strg:es - co2_molar_density:TKE - air_density:un_LE - un_LE:un_h2o_flux - Tau:un_LE -
            LE:H_strg - rand_err_h2o_flux:co2_mole_fraction - air_molar_volume:co2 - LE:max_speed - LE:un_Tau - rand_err_h2o_flux:H_strg -
            H_strg:un_LE - LE:TKE - co2_molar_density:max_speed - co2_molar_density-LE-max_speed-TKE-co2-w_slash_co2_cov:es-
            w_slash_co2_cov:max_speed-w_slash_co2_cov:TKE-w_slash_co2_cov:un_Tau-w_slash_co2_cov:co2-w_slash_co2_cov:co2-
            Tau:co2-LE:rand_err_h2o_flux-LE:air_density-LE:air_molar_volume-rand_err_LE:es-rand_err_h2o_flux:air_density-
            H_strg:co2_molar_density-H_strg:co2-es-LE:co2_molar_density-co2_molar_density:un_LE-co2_molar_density:un_h2o_flux-
            w_slash_co2_cov:rand_err_h2o_flux-air_density-w_slash_co2_cov:Tau-w_slash_co2_cov:air_molar_volume-w_slash_co2_cov:co2_mole_fraction-
            LE:es-w_slash_co2_cov:H_strg-H_strg-w_slash_co2_cov:air_density-w_slash_co2_cov:LE-w_slash_co2_cov:rand_err_LE)
summary(model9)
anova(model9)
#Модель 9 - после упрощения R^2=0.98 в состав включены 13 независимых переменных
#Графическая интерпертация  множественной линейной регрессии потоков углекислого газа 
#за летний период 2013 года по данным измерений методом турбулентной пульсации (model9)
qplot(w_slash_co2_cov+Tau+rand_err_LE+rand_err_h2o_flux+
        un_LE+un_h2o_flux+LE:rand_err_LE+rand_err_LE:un_LE,co2_flux,data=eddy,alpha=I(1/4))+geom_smooth(se=FALSE, method=lm)+theme_bw()
warnings()


#***********************************************
#*****************************************************
eddy_num[is.na(eddy_num$co2_flux),"co2_flux"] = 0
eddy_num[eddy_num$co2_flux>500,"co2_flux"]=0
model10=lm(data  = eddy_num, co2_flux~ (w_slash_co2_cov+LE+un_H+max_speed+un_LE+co2)^2-co2-w_slash_co2_cov:un_H-
             w_slash_co2_cov:un_H-w_slash_co2_cov:max_speed-w_slash_co2_cov:co2-LE:H_strg-w_slash_co2_cov:un_H-
             w_slash_co2_cov:max_speed-w_slash_co2_cov:co2-LE:H_strg-LE:un_H-H_strg:un_H-H_strg:max_speed-
             H_strg:un_LE-H_strg:co2-max_speed:co2-LE:max_speed-LE:co2-un_H:un_LE-un_H:co2-max_speed:un_LE-un_H:max_speed-
             un_LE:co2-w_slash_co2_cov:H_strg-un_H -LE-un_LE-max_speed) 

summary(model10)
anova(model10)
plot(model10)
#При дальнейшем отбрасывании независимых переменных наблюдается резкое падение величины R^2;
#Поэтому останавливаемся на варианте регрессионной модели с 6 независимыми переменными;+ при R^2=0,97

#Изобразим графически;
#Графическая интерпертация  множественной линейной регрессии потоков углекислого газа 
#за летний период 2013 года по данным измерений методом турбулентной пульсации (model20)
qplot(w_slash_co2_cov+LE+H_strg+un_H,co2_flux,data=eddy,alpha=I(1/4))+geom_smooth(se=FALSE, method=lm)+theme_bw()
#Итог- регресионная модель, уравнение построено по 6 независимым переменным R^2=0.97; распределени остатков- близкое;
#к нормальному, итогования модель - model20.

#w_slash_co2_cov- наиболее значимая переменная




#Аналитические приложения
#*********************************************************************************************************
#-LE:co2   - H_strg:air_density -
#  H_strg:air_molar_volume - co2_mole_fraction - un_Tau - Tau:LE - Tau:rand_err_h2o_flux
# Tau:H_strg - Tau:co2_molar_density - Tau:co2_mole_fraction - Tau:air_density - Tau:co2_mole_fraction -
# Tau:air_density - Tau:es - Tau:TKE - LE:co2_mole_fraction - LE:un_LE - rand_err_LE:rand_err_h2o_flux - 
# rand_err_LE:H_strg - rand_err_LE:co2_mole_fraction - rand_err_LE:air_molar_volume - rand_err_LE:max_speed -
# rand_err_LE:TKE -rand_err_LE:un_h2o_flux - rand_err_h2o_flux:air_molar_volume - rand_err_LE:max_speed - 
#  rand_err_LE:TKE - rand_err_LE:un_h2o_flux - rand_err_h2o_flux:air_molar_volume - rand_err_h2o_flux:es - rand_err_h2o_flux:es-
##  rand_err_h2o_flux:un_Tau - rand_err_h2o_flux:un_h2o_flux - H_strg:co2_mole_fraction - H_strg:air_density -
# H_strg:max_speed - H_strg:TKE - H_strg:un_Tau - H_strg:un_h2o_flux - co2_molar_density:co2_mole_fraction - 
#  co2_molar_density:air_density - co2_molar_density:air_molar_volume - co2_molar_density:es - co2_molar_density:un_Tau - 
# co2_mole_fraction:air_molar_volume -co2_mole_fraction:es - co2_mole_fraction:max_speed - co2_mole_fraction:TKE -
#  co2_mole_fraction:un_Tau - co2_mole_fraction:un_h2o_flux - air_density:max_speed - air_density:TKE - air_density:un_Tau -
# air_density:un_h2o_flux - air_density:co2 - air_molar_volume:es - air_molar_volume:max_speed - air_molar_volume:TKE -
    #  air_molar_volume:un_Tau - air_molar_volume:un_LE - air_molar_volume:un_h2o_flux - es:max_speed - es:TKE - es:un_Tau -
# es:un_LE - es:un_h2o_flux - max_speed:TKE - max_speed:un_Tau - max_speed:un_LE - max_speed:un_h2o_flux - max_speed:co2 -
#  TKE:un_Tau - TKE:un_LE - TKE:un_h2o_flux - TKE:co2 - un_Tau:un_LE - un_Tau:un_h2o_flux - un_Tau:co2 -
#  un_LE:co2 - un_h2o_flux:co2 - air_molar_volume - Tau:air_molar_volume - Tau:max_speed - rand_err_LE:co2_molar_density -
#  rand_err_LE:air_density - rand_err_LE:un_Tau - rand_err_LE:co2 - rand_err_h2o_flux:max_speed - rand_err_h2o_flux:TKE -
#  rand_err_h2o_flux:un_LE - co2_molar_density:co2 - co2_mole_fraction:air_density - co2_mole_fraction:un_LE -
#  co2_mole_fraction:co2 - air_density:air_molar_volume - air_density:es - es:co2 - Tau:rand_err_LE - Tau:un_Tau -
#  Tau:un_h2o_flux - LE:un_h2o_flux - rand_err_h2o_flux:co2_molar_density - rand_err_h2o_flux:co2_molar_density -
#  rand_err_h2o_flux:co2 - H_strg:es - co2_molar_density:TKE - air_density:un_LE - un_LE:un_h2o_flux - Tau:un_LE -
#  LE:H_strg - rand_err_h2o_flux:co2_mole_fraction - air_molar_volume:co2 - LE:max_speed - LE:un_Tau - rand_err_h2o_flux:H_strg -
#  H_strg:un_LE - LE:TKE - co2_molar_density:max_speed - co2_molar_density - 
# air_density - es - H_strg:co2 - LE:rand_err_LE - LE:rand_err_h2o_flux-rand_err_LE:un_LE -Tau:co2 - LE:air_molar_volume - LE:es-
    #  rand_err_LE:es-max_speed-LE:air_density-rand_err_h2o_flux:air_density-rand_err_LE-
#  rand_err_h2o_flux-H_strg:co2_molar_density-v_var-h2o_flux:qc_LE-h2o_flux:es-h2o_flux:u_star_-h2o_flux:v_var-
#  w_slash_ts_cov:rand_err_Tau-w_slash_ts_cov:rand_err_LE-w_slash_ts_cov:H_strg-w_slash_ts_cov:air_molar_volume-
#  w_slash_ts_cov:RH-w_slash_ts_cov:TKE-w_slash_ts_cov:T_star_-w_slash_ts_cov:v_var-w_slash_co2_cov:rand_err_h2o_flux-
# w_slash_co2_cov:co2_molar_density-w_slash_co2_cov:h2o_time_lag-w_slash_co2_cov:es-w_slash_co2_cov:RH-w_slash_co2_cov:VPD-
#  w_slash_co2_cov:un_LE-w_slash_co2_cov:TKE-w_slash_co2_cov:v_var-w_slash_co2_cov:un_h2o_flux-w_slash_co2_cov:co2-
#  Tau:qc_LE-Tau:H-Tau:h2o_time_lag-Tau:sonic_temperature-Tau:RH-Tau:VPD-LE:T_star_-LE:un_co2_flux-LE:v_var-
#  rand_err_Tau:w_slash_h2o_cov-rand_err_Tau:qc_LE-rand_err_Tau:rand_err_LE-rand_err_Tau:rand_err_H-
#  rand_err_Tau:rand_err_h2o_flux-rand_err_Tau:H_strg-rand_err_Tau:co2_molar_density-rand_err_Tau:H-rand_err_Tau:co2_mole_fraction-
    #  w_slash_ts_cov:qc_LE-w_slash_ts_cov:rand_err_h2o_flux-w_slash_ts_cov:h2o_var-Tau:w_slash_h2o_cov-
#  Tau:h2o_var-Tau:air_temperature-Tau:un_H-Tau:v_var-LE:rand_err_Tau-LE:w_slash_h2o_cov-LE:qc_LE-
    #  LE:rand_err_H-LE:H-LE:h2o_time_lag-LE:air_temperature-LE:un_H-LE:RH-rand_err_Tau:h2o_time_lag-
#  rand_err_Tau:h2o_var-rand_err_Tau:air_temperature -rand_err_Tau:sonic_temperature-rand_err_Tau:air_density-
    #  rand_err_Tau:air_molar_volume-rand_err_Tau:es-rand_err_Tau:max_speed-rand_err_Tau:un_H-rand_err_Tau:RH-
#  rand_err_Tau:RH-rand_err_Tau:VPD-rand_err_Tau:un_Tau-rand_err_Tau:un_LE-rand_err_Tau:TKE-rand_err_Tau:T_star_-
#  rand_err_Tau:v_var-w_slash_h2o_cov:rand_err_LE-w_slash_h2o_cov:H_strg-w_slash_h2o_cov:H-w_slash_h2o_cov:h2o_time_lag-
#  w_slash_h2o_cov:air_temperature-w_slash_h2o_cov:sonic_temperature-w_slash_h2o_cov:max_speed-w_slash_h2o_cov:u_star_-
#  w_slash_h2o_cov:un_LE-w_slash_co2_cov:co2_mole_fraction-w_slash_co2_cov:air_temperature-Tau:un_co2_flux-
#  LE:co2_molar_density-LE:sonic_temperature-LE:VPD-rand_err_Tau:u_star_-rand_err_Tau:un_h2o_flux-
#  rand_err_Tau:co2-w_slash_h2o_cov:es-w_slash_h2o_cov:es-w_slash_h2o_cov:un_H-w_slash_h2o_cov:TKE-w_slash_h2o_cov:v_var-
#  w_slash_h2o_cov:co2-qc_LE:rand_err_H-qc_LE:rand_err_h2o_flux-qc_LE:H_strg-qc_LE:co2_molar_density-
#  qc_LE:H-qc_LE:rand_err_h2o_flux-qc_LE:H_strg-qc_LE:co2_molar_density-qc_LE:H-qc_LE:co2_mole_fraction-
#  qc_LE:h2o_time_lag-qc_LE:sonic_temperature-qc_LE:air_density -qc_LE:air_molar_volume-qc_LE:es-qc_LE:max_speed-
#  qc_LE:RH-qc_LE:VPD-qc_LE:un_Tau-qc_LE:un_LE-qc_LE:TKE-qc_LE:T_star_-qc_LE:un_LE-qc_LE:TKE-qc_LE:T_star_-
#  qc_LE:v_var-qc_LE:co2-rand_err_LE:rand_err_H-rand_err_LE:H-rand_err_LE:h2o_time_lag-rand_err_LE:air_temperature-
#  rand_err_LE:sonic_temperature-rand_err_LE:un_H-rand_err_LE:RH-rand_err_LE:VPD-w_slash_h2o_cov:un_Tau-
#  w_slash_h2o_cov:un_co2_flux-qc_LE:air_temperature-qc_LE:un_H-qc_LE:u_star_-qc_LE:un_h2o_flux-rand_err_LE:u_star_-
#  rand_err_LE:T_star_-rand_err_LE:un_co2_flux-rand_err_LE:v_var-rand_err_H:rand_err_h2o_flux-rand_err_H:co2_molar_density-
#  rand_err_H:H-rand_err_H:co2_mole_fraction-rand_err_H:h2o_time_lag-rand_err_H:air_temperature-rand_err_H:sonic_temperature-
#  rand_err_H:air_density-rand_err_H:air_molar_volume-rand_err_H:es-rand_err_H:max_speed-rand_err_H:RH-
#  rand_err_H:VPD-rand_err_H:un_LE-rand_err_H:T_star_-rand_err_H:un_co2_flux-rand_err_H:v_var-rand_err_H:un_h2o_flux-
#  rand_err_H:co2-rand_err_h2o_flux:H-rand_err_h2o_flux:h2o_time_lag-rand_err_h2o_flux:air_temperature-rand_err_h2o_flux:sonic_temperature-
#  rand_err_h2o_flux:un_H-rand_err_h2o_flux:RH-rand_err_h2o_flux:T_star_-rand_err_h2o_flux:T_star_-
#  rand_err_h2o_flux:un_co2_flux-rand_err_h2o_flux:v_var-H_strg:H-h2o_flux:h2o_var-w_slash_h2o_cov:rand_err_h2o_flux-
    #  rand_err_LE:h2o_var-rand_err_H:h2o_var-rand_err_H:un_H-rand_err_h2o_flux:h2o_var-rand_err_h2o_flux:u_star_-
    #  H_strg:h2o_time_lag-H_strg:h2o_var-H_strg:air_temperature-H_strg:sonic_temperature-H_strg:un_H-H_strg:VPD-
#  H_strg:un_co2_flux-H_strg:v_var-co2_molar_density:h2o_time_lag-co2_molar_density:h2o_var-co2_molar_density:air_temperature-
#  co2_molar_density:sonic_temperature-co2_molar_density:RH-co2_molar_density:VPD-co2_molar_density:u_star_-
#  co2_molar_density:un_LE-co2_molar_density:T_star_-co2_molar_density:un_co2_flux-co2_molar_density:v_var-
#  co2_molar_density:un_h2o_flux-H:co2_mole_fraction-H:h2o_time_lag-H:h2o_var-H:sonic_temperature-
#  H:un_H-H:RH-H:VPD-H:u_star_-h2o_flux:rand_err_Tau-h2o_flux:w_slash_h2o_cov-h2o_flux:rand_err_H-h2o_flux:H_strg-
#  h2o_flux:max_speed-h2o_flux:TKE-h2o_flux:un_h2o_flux-w_slash_co2_cov:max_speed -H:TKE-H:co2-H:un_h2o_flux-
#  un_H:TKE-un_H:co2-un_H:un_h2o_flux-un_H:un_LE-H_strg-TKE-w_slash_co2_cov:H -co2 -w_slash_co2_cov:LE-
#  w_slash_co2_cov:rand_err_LE-w_slash_co2_cov:un_H-H:un_LE)

