##########################################################
#                                                        #
#                                                        #
#         METAFOR SCRIPT - POWER POINT SLIDES            #
#                                                        #
#                                                        #
##########################################################

#Installing Metafor package and getting other sources#

install.packages('metafor')
library(metafor)
help(metafor)


#############################################
#Example: Ejemplo_Ansiedad_Genero           #
#Differences between male and female in     #
#math anxiety                               #
#############################################

#Reading the data file

smd.means <- read.table(file = 'Ejemplo_Ansiedad_Genero.txt', 
                        header = TRUE)
smd.means

#Computing SMD (g) and its variance#

smd <- escalc(measure = "SMD", 
              m1i = wm, 
              m2i = mm, 
              sd1i = wsd, 
              sd2i = msd, 
              n1i = wn, 
              n2i = mn, 
              data = smd.means, 
              append = TRUE)

smd #Two columns (yi and vi) have been added#


#######################################################
#Example:Ejemplo_Relacion_Extraversion-Rendimiento.txt#
#Relation between extraversion and                    #
#academic performance                                 #
#######################################################

##Reading the data file

zcor.correlations <- read.table(file = 'Ejemplo_Relacion_Extraversion-Rendimiento.txt', 
                                header = TRUE) 
zcor.correlations

#Computing Fisher's z and its variance#

zcor <- escalc(measure = "ZCOR", 
               ri = cor, 
               ni = sample, 
               data = zcor.correlations, 
               append = TRUE)

zcor


################################################
#Example: Ejemplo_Terapia_Control-Tratamiento###
#Comparison between treatment and control group#
#in depressive symptomathology                 #
################################################

##Reading the data file

dat <- read.table(file = 'Ejemplo_Terapia_Control-Tratamiento.txt', 
                          header = TRUE) #Getting the data#
dat 

#Since data already include Hedges' g and its variance, there is no
#need to use escal() function

#Fitting a random effects model to get the mean Hedges' g across
#studies

res <- rma(yi = g, vi = var, data = dat) 
res

#The output also shows the test for heterogeneity and the I2

confint(res) #To obtain corresponding confidence intervals of heterogeneity measures#

#Obtaining the forest plot#

forest(res)

#Testing for moderators#

#Analog to ANOVA: 'random (yes/no)'# 

#Option 1#

res_r <- rma(yi = g, vi = var, 
            mods = ~ factor(random), 
            data = dat)   
res_r

#Option 2: From Meta-regression (see below)#

res_r <- rma(yi = g, vi = var, 
             mods = ~ random, 
             data = dat)   
res_r

#Analog to ANOVA: If we want to know the mean ES for each 
#'random' level: yes/no

res_r <- rma(yi = g, vi = var, 
            mods = ~ factor(random)-1, 
            data = dat)   
res_r

#Meta-regression: intensity (from 1 to 7)# 

#Option 1#

res_i <- rma(yi = g, vi = var, 
            mods = ~ intensity, 
            data = dat) 
res_i

#Option 2#

res_i <- rma(yi = g, vi = var, 
           mods = cbind(intensity), 
           data = dat) 
res_i

#Meta-regression: random (yes/no) and intensity (from 1 to 7)

res_r_i <- rma(yi = g, vi = var, 
               mods = ~ random + intensity, 
               data = dat)   
res_r_i

#Obtaining a funnel plot (model without moderators)#

funnel(res, main = "Random-Effects Model")

#Testing for funnel plot assymetry#

regtest(res)  #Egger's regression test#

ranktest(res) #Rank correlation test#

#Trim and fill method#

rtf <- trimfill(res)

rtf

funnel(rtf)  #Obtaining a funnel plot#

#Write methods section: 'res' model#

install.packages("rmarkdown")
library(rmarkdown)

reporter(res)
