#!/usr/bin/r 

library(lme4)

fm = lmer(Reaction ~ Days + (Days | Subject), sleepstudy) 
summary(fm)