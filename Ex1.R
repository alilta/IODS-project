data<-as.data.frame(Kaikki_data_artikkeli3)
names(data)
case<-data[,1]
sex<-data[,2]
age<-data[,3]
duration<-data[,4]
onset<-data[,5]
bmi<-data[,6]
whr<-data[,7]
syst<-data[,8]
diast<-data[,9]
syke<-data[,10]
HbA1c<-data[,11]
gluk<-data[,13]
krea<-data[,14]
gfr<-data[,15]
uaer<-data[,16]
kol<-data[,17]
ldl<-data[,18]
hdl<-data[,19]
trigly<-data[,20]
crp<-data[,21]
albuminuria<-data[,23]
tupakka<-data[,34]
names(data)
cmb1<-data[,90]
cmb2<-data[,91]
date<-data[,87]
etdrso<-as.numeric(data[,57])[1:191]
etdrsv<-as.numeric(data[,58])[1:191]
etdrso
etdrs<-pmax(etdrso, etdrsv)
etdrs
wmhi<-data[,83]

cmb2[date>0]
date>0
sum(as.numeric(date>0), na.rm=T) #121 diabeetikkoa kuvattu
121/188 #64%
quantile(age)

x=c()
x<-for(i in 3:21){
 a<- wilcox.test(data[,i][date>0], data[,i], na.rm=T)$p.value
 print(a)
}
#ryhmien välillä on merkitsevät eros iässä ja HbA1c. Kuvatut ovat korkeampi HbA1c ja hieman iäkkäämpiä
quantile(syst[date>0], na.rm=T)
sum(as.numeric(sex[date>0]=="F"), na.rm=T)
73/121
wilcox.test(syst[,3][date>0], data[,3])$p.value
sum(as.numeric(etdrs[date>0]>19), na.rm=T)
95/121
20/121    

#monellako wmhi&cmb? #16
sum(as.numeric(wmhi[date>0]>0&cmb2[date>0]), na.rm=T)
#monellako cmb? #42, eli 42-16=26 vain cmb
sum(as.numeric(cmb2[date>0]>0), na.rm=T)
#monellako wmhi? #35, eli 19 vain wmhi
sum(as.numeric(wmhi[date>0]>0), na.rm=T)
#lisäksi kahdella uudet infarktit, joista toisella wmhi, toisella ei mitään
#any marker?
cor.test(as.numeric(cmb1>0), as.numeric((cmb2-cmb1)>0), method="spearman")

stdcoeff <- function(x, y){
  b <- summary(lm(y~x, na.rm=T))$coef[-1, 1]
  sx <- sd(x, na.rm=T)
  sy <- sd(y, na.rm=T)
  beta <- b*sx/sy
  return(beta)
}

kvantiilit <- function(a){
  mediaani <- median(a,na.rm=T)
  alaraja <- quantile(a[1:191], 1/4, na.rm=T)
  ylaraja <- quantile(a[1:191], 3/4, na.rm=T)
  return(c(mediaani, alaraja, ylaraja))
}

#table 1: Clinical charasteristics of subjects and controls
# yläriville nimet: Subjects, controls, p for difference
# rivien nimet: Female sex, age (years), Diabetes duration (years), BMI (kg/m2), Systolic BP, Diastolic BP, HbA1c, Krea, 
#Kol, LDL, HDL, Trigly, Albuminuria, smoking, MRI-findings, Cerebral SVD, CMBs, 1, 2 ≥3, Topography of CMBs, Lobar,
#Deep, Mixed, Any WMH, Lacunes

#Eli 27 riviä ja 4 saraketta
#

nimet_taulukko1 <- c("Female sex", "Age (years)", "Diabetes duration (years)", "BMI (kg/m2)", "Systolic blood pressure (mmHg)", "Diastolic blood pressure (mmHg)",
                     "HbA1c (mmol/mol)", "Krea", "Kol", "LDL", "HDL", "Trigly", "Albuminuria", "Smoking", "MRI-findings", "Cerebral SVD",
                     "CMBs", "1", "2", "≥3", "Topography of CMBs", "Lobar", "Deep", "Mixed", "Any WMH", "Lacunes")
sarakkeet_taulukko1 <- c("Subjects", "alaraja", "yläraja", "Controls", "alaraja", "yläraja", "P value")
taulukko1 <- matrix(nrow=length(nimet_taulukko1), ncol=length(sarakkeet_taulukko1))
colnames(taulukko1)=sarakkeet_taulukko1
rownames(taulukko1)=nimet_taulukko1
taulukko1

#mieti hetki
#HALUAT esim. tutkittavien iän mediaanin (ja IQR)
#saat sen median(x[date>0]), eikö? ja kvantiilit saat kvantiilit(x[date>0])!!!

kvantiilit(age[date>0])
tauluun<-function(x){
  subj<-kvantiilit(x[1:191][date>0])
  cntrl<-kvantiilit(x[192:221])
  pvalue<-wilcox.test(x[1:191][date>0], x[192:221])$p.value

  
  
  
  result<-c(subj, cntrl, pvalue)
  return(result)
}
tauluun(age)
taulukko1[2,]<-round(tauluun(age), digits=3)
taulukko1[4,]<-round(tauluun(bmi), digits=3)
taulukko1[5,]<-round(tauluun(syst), digits=3)
taulukko1[6,]<-round(tauluun(diast), digits=3)
taulukko1[7,]<-round(tauluun(HbA1c), digits=3)
taulukko1[8,]<-round(tauluun(krea), digits=3)
taulukko1[9,]<-round(tauluun(kol), digits=3)
taulukko1[10,]<-round(tauluun(ldl), digits=3)
taulukko1[11,]<-round(tauluun(hdl), digits=3)
taulukko1[12,]<-round(tauluun(trigly), digits=3)
taulukko1[13,]<-round(tauluun(albuminuria), digits=3)
taulukko1[14,]<-round(tauluun(tupakka), digits=3)
#tyhjä
taulukko1[16,]<-c(sum(as.numeric(cmb2[1:191]==1), na.rm=T), "-", "-", 2, "-", "-", "<0.001")
taulukko1[17,]<-c(sum(as.numeric(cmb2[1:191]==2), na.rm=T), "-", "-", 0, "-", "-", "<0.001")
taulukko1[18,]<-c(sum(as.numeric(cmb2[1:191]>2), na.rm=T), "-", "-", 0, "-", "-", "<0.001")
taulukko1[19,]<-round(tauluun(hdl), digits=3)
taulukko1



#taulukko 3 pitäisi olla helppo rakentaa
#halutaan sarakkeisiin "Standardized correlation coefficient", "p-value"
#riveille samoja nimiä kuin ennenkin

nimet_taulukko3 <- c("Age (years)", "Diabetes duration (years)", "BMI (kg/m2)", "Systolic blood pressure (mmHg)", "Diastolic blood pressure (mmHg)",
                     "HbA1c (mmol/mol)", "Krea", "Kol", "LDL", "HDL", "Trigly")
sarakkeet_taulukko3 <- c("Correlation", "p value")
taulukko3 <- matrix(nrow=length(nimet_taulukko3), ncol=length(sarakkeet_taulukko3))


colnames(taulukko3)=sarakkeet_taulukko3
rownames(taulukko3)=nimet_taulukko3
taulukko3

tauluun3<-function(x){
  correlation<-stdcoeff(cmb2[date>0][1:191], x[date>0][1:191])
  pvalue<-cor.test(cmb2[date>0][1:191], x[date>0][1:191], method="spearman")$p.value
  results<-c(correlation, pvalue)
  return(results)
}
tauluun3(age)

taulukko3[1,]<-round(tauluun3(age), digits=3)
taulukko3[2,]<-round(tauluun3(duration), digits=3)
taulukko3[3,]<-round(tauluun3(bmi), digits=3)
taulukko3[4,]<-round(tauluun3(syst), digits=3)
taulukko3[5,]<-round(tauluun3(diast), digits=3)
taulukko3[6,]<-round(tauluun3(HbA1c), digits=3)
taulukko3[7,]<-round(tauluun3(krea), digits=3)
taulukko3[8,]<-round(tauluun3(kol), digits=3)
taulukko3[9,]<-round(tauluun3(ldl), digits=3)
taulukko3[10,]<-round(tauluun3(hdl), digits=3)
taulukko3[11,]<-round(tauluun3(trigly), digits=3)
taulukko3

taulukko3a<-summary(lm(as.numeric(cmb2>cmb1)~age+duration+syst+HbA1c+trigly))
round(taulukko3a[4], digits=3)
table(taulukko3a[4])

pi
