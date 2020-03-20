#### CODING NATIONAL EDUCATION VARIABLES INTO INTERNATIONALLY STANDARDIZED ISCED SCALE####

class(df.socdem$education)

df.socdem$education_fact <- as.factor(df.socdem$education)

table(df.socdem$country)
table(df.socdem$education)

country_ed <- table(df.socdem$education,df.socdem$country) 
country_ed 

#####ISCED coding table######


#German 
#1: Volks-/Hauptschule =2
#2: Weiterfuehrende Schule (Mittel-, Real-, Handelsschule) =3
#3: Abitur, (Fach-)Hochschulreife =4
#4: No qualification (yet) =8

#UK
#1: GCSEs or equivalent =3
#2: GNVQs or equivalent =3
#3: A levels / AS levels / Scottish Highers / NVQ levels / Int. Baccalaureate =4
#4: Undergraduate degree or equivalent =6
#5: Postgraduate degree or equivalent =7
#6: Professional Qualification =4
#98: No formal education or qualifications (yet) =8

#France
#1: BEPC =2
#2: BEP =3
#3: CAP =3
#4: BAC =4
#5: Brevet de technicien =5
#6: Bac+2 / DUT / BTS =5
#7: bac+3 / Licence =6
#8: Bac+5 / Master =7
#97: Other education =8
#NA: No qualification (yet) =8

#As Germany has only the highest value of 4, the two other countrY's higher values should be downcoded to 4 as well.:


#rename factor levels

library(plyr)
df.socdem$ISCED <- revalue(df.socdem$education_fact, 
                           
                                    #Germany
                                    c("1: Volks-/Hauptschule"="2", 
                                    "2: Weiterfuehrende Schule (Mittel-, Real-, Handelsschule)"="3", 
                                    "3: Abitur, (Fach-)Hochschulreife"="4",
                                    "4: No qualification (yet)" = "8",
                                    
                                    #UK
                                    "1: GCSEs or equivalent" = "3",
                                    "2: GNVQs or equivalent" = "3",
                                    "3: A levels / AS levels / Scottish Highers / NVQ levels / Int. Baccalaureate" ="4",
                                    "4: Undergraduate degree or equivalent" = "4",
                                    "5: Postgraduate degree or equivalent" = "4",
                                    "6: Professional Qualification" = "4",
                                    "98: No formal education or qualifications (yet)" = "8",
                                    
                                    #France
                                    "1: BEPC" ="2",
                                    "2: BEP" ="3",
                                    "3: CAP" ="3",
                                    "4: BAC" ="4",
                                    "5: Brevet de technicien" = "4",
                                    "6: Bac+2 / DUT / BTS" ="4",
                                    "7: bac+3 / Licence"= "4",
                                    "8: Bac+5 / Master" = "4",
                                    "97: Other education" ="8",
                                    "NA: No qualification (yet)" ="8"
                                    ))



country_ISCED <- table(df.socdem$ISCED,df.socdem$country) 
country_ISCED

#12: France 16: United Kingdom 9: Germany
#2         34                  0        196
#3        229                170        433
#4        865                440        435
#8         31                 20         11

#As we can see, the input education variable is not very well measured, thus their standardized (ISCED) version might probably not
#be a strong predictor.


