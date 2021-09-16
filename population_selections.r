library('ggplot2')
library("dplyr")
library('ggbeeswarm')


metadata2<- read.delim("~/Documents/data/covid/dundee_february_2021/metadata/covid_metadata_V4.csv", header=TRUE, sep=",",stringsAsFactors=FALSE)
metadata2$Age<-as.numeric(metadata2$Age)
metadata2$score_at_enrollment<-as.numeric(metadata2$at.enrolment)
metadata2$score_at_29<-as.numeric(metadata2$at.29)


metadata2$r_id<-unlist(lapply(metadata2$Patient, function(x)(gsub(" ", "", paste(x, '.01.N.PG.Quantity')))))
metadata2$r_id[grepl("^[[:digit:]]+",metadata2$r_id)]<-unlist(lapply(metadata2$r_id[grepl("^[[:digit:]]+",metadata2$r_id)], function(x)gsub(" ", "", paste('X', x))))
#metadata2[metadata2$r_id=='X043(43+STOP1updatedPREDICTclinicaldata).01.N.PG.Quantity',]$r_id<-'X43.01.N.PG.Quantity'
metadata2$r_id[1:9]<-unlist(lapply(metadata2$r_id[1:9], function(x)gsub("X", "X00", x)))
metadata2$r_id[10:96]<-unlist(lapply(metadata2$r_id[10:96], function(x)gsub("X", "X0", x)))


all_copies<-read.delim("~/Documents/data/covid/dundee_february_2021/copy_numbers_august_2021.csv", header=TRUE, sep=",",stringsAsFactors=FALSE)

proteome_control<-intersect(names(all_copies), c(metadata2[(metadata2$Diagnosis=='CONTROL'),]$r_id,
                                                 'HK.01.N.PG.Quantity','JC.01.N.PG.Quantity',
                                                 'RH.01.N.PG.Quantity','ML.01.N.PG.Quantity','Healthy.DC.N.PG.Quantity'))
batch3<-c('ST.008.01.N.PG.Quantity','ST.009.01.N.PG.Quantity','ST.010.01.N.PG.Quantity','ST.011.01.N.PG.Quantity',
          'ST.012.01.N.PG.Quantity','ST.013.01.N.PG.Quantity','ST.014.01.N.PG.Quantity','ST.015.01.N.PG.Quantity',
          'ST.017.01.N.PG.Quantity','ST.018.01.N.PG.Quantity','ST.019.01.N.PG.Quantity','SF.020.01.N.PG.Quantity',
          'ST.021.01.N.PG.Quantity','ST.022.01.N.PG.Quantity','ST.023.01.N.PG.Quantity','ST.024.01.N.PG.Quantity',
          'ST.025.01.N.PG.Quantity','ST.026.01.N.PG.Quantity','ST.027.01.N.PG.Quantity','ST.028.01.N.PG.Quantity',
          'ST.029.01.N.PG.Quantity','ST.030.01.N.PG.Quantity','ST.031.01.N.PG.Quantity','ST.032.01.N.PG.Quantity',
          'X150.01.N.PG.Quantity','X151.01.N.PG.Quantity','X152.01.N.PG.Quantity','X154.01.N.PG.Quantity',
          'X157.01.N.PG.Quantity', 'X153.01.N.PG.Quantity',
          'X159.01.N.PG.Quantity', 'X161.01.N.PG.Quantity'
          #'X155.01.N.PG.Quantity', 'X156.01.N.PG.Quantity','X160.01.N.PG.Quantity','ST.020.01.N.PG.Quantity',
          #'ST.033.01.N.PG.Quantity','ST.034.01.N.PG.Quantity',
)

batch4<-c( 'ST.003.01.N.PG.Quantity', 'ST.004.01.N.PG.Quantity', 'ST.005.01.N.PG.Quantity', 'ST.006.N.PG.Quantity',
           'ST.007.N.PG.Quantity',
           'ST.016.01.N.PG.Quantity', 'ST.035.01.N.PG.Quantity', 'ST.038.01.N.PG.Quantity','ST.039.01.N.PG.Quantity',
           'ST.040.01.N.PG.Quantity', 'ST.041.01.N.PG.Quantity', 'ST.042.01.N.PG.Quantity', 'ST.043.01.N.PG.Quantity',
           'ST.044.01.N.PG.Quantity', 'ST.045.01.N.PG.Quantity', 'ST.046.01.N.PG.Quantity', 'ST.047.01.N.PG.Quantity',
           'ST.050.01.N.PG.Quantity', 'ST.051.01.N.PG.Quantity', 'ST.052.01.N.PG.Quantity', 
           #'ST.053.01.N.PG.Quantity','ST.054.01.N.PG.Quantity','ST.055.01.N.PG.Quantity','ST.059.01.N.PG.Quantity',
           #'X168.01.N.PG.Quantity','X172.01.N.PG.Quantity','X177.01.N.PG.Quantity','X178.01.N.PG.Quantity',
           #'X181.01.N.PG.Quantity', 'ST.048.01.N.PG.Quantity','ST.060.01.N.PG.Quantity', 'ST.064.01.N.PG.Quantity',
           'ST.057.01.N.PG.Quantity', 'ST.058.01.N.PG.Quantity', 'ST.060.01.N.PG.Quantity',
           'ST.061.01.N.PG.Quantity', 'ST.062.01.N.PG.Quantity',
           'ST.063.01.N.PG.Quantity', 'ST.065.01.N.PG.Quantity', 'ST.066.01.N.PG.Quantity',
           'ST.067.01.N.PG.Quantity',
           'ST.069.01.N.PG.Quantity', 'ST.072.01.N.PG.Quantity', 'ST.073.01.N.PG.Quantity', 'ST.074.01.N.PG.Quantity',
           'ST.075.01.N.PG.Quantity', 'ST.076.01.N.PG.Quantity', 'ST.077.01.N.PG.Quantity', 'ST.078.01.N.PG.Quantity',
           'ST.079.01.N.PG.Quantity', 'ST.080.01.N.PG.Quantity', 'ST.081.01.N.PG.Quantity','X131.01.N.PG.Quantity',
           'X131.01.N.PG.Quantity','X145.01.N.PG.Quantity'
)

batch1<-c('X001.01.N.PG.Quantity','X005.01.N.PG.Quantity','X009.01.N.PG.Quantity','X017.01.N.PG.Quantity',
          'X027.01.N.PG.Quantity','X043.01.N.PG.Quantity')
batch2<-c('X061.01.N.PG.Quantity','X079.01.N.PG.Quantity','X080.01.N.PG.Quantity','X096.01.N.PG.Quantity')
control_1<-proteome_control[1:10]
control_2<-proteome_control[11:20]
control_4<-proteome_control[21:42]

asymptomatic<-c('ST.069.01.N.PG.Quantity', 'X154.01.N.PG.Quantity', 'X152.01.N.PG.Quantity', 'X151.01.N.PG.Quantity', 'X145.01.N.PG.Quantity',
                'X153.01.N.PG.Quantity', 'X157.01.N.PG.Quantity')
no_ox_improved<-intersect(names(all_copies), box_in[(box_in$r_id %in% c(batch3,batch4, batch1, batch2)) & (box_in$score_at_enrollment<=3)
                                                    & (box_in$Maximum.severity<=3)& (!box_in$r_id %in% c(asymptomatic)),]$r_id)

no_ox_worse<-intersect(names(all_copies), box_in[(box_in$r_id %in% c(batch3,batch4, batch1, batch2)) & (box_in$score_at_enrollment<=3)
                                                 & (box_in$Maximum.severity>3),]$r_id)
oxygen_imp<-intersect(names(all_copies), box_in[(box_in$r_id %in% c(batch3,batch4, batch1, batch2)) & (box_in$score_at_enrollment==4) &
                                                  ((box_in$Maximum.severity==4)&(box_in$score_at_29<7)),]$r_id)
oxygen_wor<-intersect(names(all_copies), box_in[(box_in$r_id %in% c(batch3,batch4, batch1, batch2)) & (box_in$score_at_enrollment==4) &
                                                  ((box_in$Maximum.severity>4)|(box_in$score_at_29==7)),]$r_id)
high_o2_imp<-intersect(names(all_copies), box_in[(box_in$r_id %in% c(batch3,batch4, batch1, batch2)) & (box_in$score_at_enrollment==5) &
                                                   ((box_in$Maximum.severity==5)&(box_in$score_at_29<7)),]$r_id)
high_o2_wor<-intersect(names(all_copies), box_in[(box_in$r_id %in% c(batch3,batch4, batch1, batch2)) & (box_in$score_at_enrollment==5) &
                                                   ((box_in$Maximum.severity>5)|(box_in$score_at_29==7)),]$r_id)
venti<-intersect(names(all_copies), box_in[(box_in$r_id %in% c(batch3,batch4, batch1, batch2)) & (box_in$score_at_enrollment==6),]$r_id)
proteome_copd<-intersect(names(all_copies), metadata2[metadata2$Diagnosis=='COPD',]$r_id)
proteome_pneumonia<-intersect(names(all_copies), metadata2[(metadata2$Diagnosis=='pneumonia')|
                                                             (metadata2$Diagnosis=='pneumonia (rhinovirus)')|
                                                             (metadata2$Diagnosis=='pneumonia'),]$r_id)
proteome_lrti<-intersect(names(all_copies), metadata2[(metadata2$Diagnosis=='LRTI')|
                                                        (metadata2$Diagnosis=='LRTI control')|
                                                        (metadata2$Diagnosis=='LRTI/biliary disease'),]$r_id)
                                                        
                                                        
                                                 
