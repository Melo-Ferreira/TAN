setwd('E:\\Artigo TAN\\ANÁLISES 2020') 
load('TAN.rda') 
library(lme4)
library(car)
library(DHARMa)

# Predisponente
m1SemPesos=glmer(tan~p_ext_pobres_c +
                   idade_mae +racacor_mae +compx +educ_mae +partos_mae +
                   (1|capitais),family=binomial(link = logit),data=criamae)

simulationOutput <- simulateResiduals(fittedModel = m1SemPesos, n = 1000)
res=simulationOutput$scaledResiduals
layout(matrix(c(1,2,3,4),nrow=2,byrow=TRUE))
testUniformity(simulationOutput)
plotResiduals(simulationOutput, rank = TRUE, quantreg = TRUE, main="Residual vs. predicted lines shoud match",
              ylab="Standardized residual", xlab="Predicted values (rank transformed)")
testDispersion(simulationOutput)
testOutliers(simulationOutput)


# Capacitante
m2SemPesos=glmer(tan~ p_ext_pobres_c + idade_mae + racacor_mae + compx + educ_mae + partos_mae +
                   c7_c+ct_c+cnt_c+pomif_c+atiremu+esf+plano+locpre+locparto+ 
                   (1|capitais),family=binomial(link = logit),data=criamae)


simulationOutput <- simulateResiduals(fittedModel = m2SemPesos, n = 1000)
res=simulationOutput$scaledResiduals
layout(matrix(c(1,2,3,4),nrow=2,byrow=TRUE))
testUniformity(simulationOutput)
plotResiduals(simulationOutput, rank = TRUE, quantreg = TRUE, main="Residual vs. predicted lines shoud match",
              ylab="Standardized residual", xlab="Predicted values (rank transformed)")
testDispersion(simulationOutput)
testOutliers(simulationOutput)

# Necessidade
m3SemPesos=glmer(tan~ p_ext_pobres_c + idade_mae + racacor_mae + compx + educ_mae + partos_mae +
                   c7_c +ct_c +plano +locpre +
                   taxami_c + taxasc_c + 
                   (1|capitais),family=binomial(link = logit),data=criamae)

simulationOutput <- simulateResiduals(fittedModel = m3SemPesos, n = 1000)
res=simulationOutput$scaledResiduals
layout(matrix(c(1,2,3,4),nrow=2,byrow=TRUE))
testUniformity(simulationOutput)
plotResiduals(simulationOutput, rank = TRUE, quantreg = TRUE, main="Residual vs. predicted lines shoud match",
              ylab="Standardized residual", xlab="Predicted values (rank transformed)")
testDispersion(simulationOutput)
testOutliers(simulationOutput)


# Comportamentos
m4SemPesos=glmer(tan~ p_ext_pobres_c + idade_mae + racacor_mae + compx + educ_mae + partos_mae +
                   c7_c +ct_c +plano +locpre + 
                   consultas7 + risco + sifilis + hiv + 
                   (1|capitais),family=binomial(link = logit),data=criamae)

simulationOutput <- simulateResiduals(fittedModel = m4SemPesos, n = 1000)
res=simulationOutput$scaledResiduals
layout(matrix(c(1,2,3,4),nrow=2,byrow=TRUE))
testUniformity(simulationOutput)
plotResiduals(simulationOutput, rank = TRUE, quantreg = TRUE, main="Residual vs. predicted lines shoud match",
              ylab="Standardized residual", xlab="Predicted values (rank transformed)")
testDispersion(simulationOutput)
testOutliers(simulationOutput)

