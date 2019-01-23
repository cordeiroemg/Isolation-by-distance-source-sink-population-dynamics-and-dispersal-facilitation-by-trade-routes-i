Sys.setenv(JAVA_HOME='C:/Program Files/Java/jdk1.8.0_171')
library(rJava)
library(glmulti)


setwd("PATH/TO/YOUR/DIRECTORY/")
data <- read.csv("full_model.csv")
data <- data.frame(data)
names(data)

####### Modeling averange migrants exchanged 

res <- glmulti.lm.out<- glmulti(scale(data$M) ~ 1        + scale(data$DIST) + 
                                  scale(data$SHIPMENTS)  + scale(data$RECEIPTS) +
                                  scale(data$WHEAT_50b)  + scale(data$RICE_50b) + 
                                  scale(data$CENTRALITY) + scale(data$Env) ,
                                  data=data, level=1, method= "h", 
                                  crit= "aicc", plotty= F, 
                                  report= F, fitfunction = "glm") 

# Getting the best model based on the lower AICc value
plot(res)

tmp <- weightable(res)
tmp <- tmp[tmp$aicc <= min(tmp$aicc) + 2,]
tmp


#glmulti.lm.out@formulas
summary(glmulti.lm.out@objects[[1]])

# Most important variables
plot(res, type="s")
