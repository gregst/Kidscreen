#########################################################
##
## R source code for data analysis in "The KIDSCREEN-27
## scale: translation and validation study of the 
## Slovenian version
##
#########################################################

for (n in c('SparseM', 'foreign', 'utils', 'relimp', 'ggplot2', 'ggdendro', 
            'foreign', 'psych', 'Hmisc', 'ltm', 'mirt', 'eRm', 'psych',
            'mokken', 'lavaan','semTools','semPlot', 'qgraph','sem','CTT',
            'MBESS','cluster', 'ggpubr')) {
  if(!require(n,character.only=TRUE)){install.packages(n)}
  library(n,character.only=TRUE)
}

mydata = read.spss("db_kidscreen.sav", use.value.labels=FALSE, to.data.frame=TRUE)

attach(mydata)

## KIDSCREEN items
## recoding variables (1, 9, 10, 11) scale from 5 to 1 
sad1 <- 6-sad
bad1 <- 6-bad
lonely1 <- 6-lonely
general_health1 <- 6-general_health

## sum of all items 
KIDSCREEN_total_score <- general_health1 + feeling_healthy + physical_activity + ability_run + energy + life + good_mood + enjoy + sad1 + bad1 + lonely1 + happy + free_time + free_activities + time_parents + parents_fair + parents_talk + money_friends + money_expenses + free_time_friends + enjoy_friends + help_friends + reliability_friends + happy_school + good_school + attention + good_relations
KIDSCREEN_total_score_Health <- general_health1 + feeling_healthy + physical_activity + ability_run + energy 
KIDSCREEN_total_score_Mood <- life + good_mood + enjoy + sad1 + bad1 + lonely1 + happy
KIDSCREEN_total_score_Family <- free_time + free_activities + time_parents + parents_fair + parents_talk + money_friends + money_expenses
KIDSCREEN_total_score_Friends <- free_time_friends + enjoy_friends + help_friends + reliability_friends
KIDSCREEN_total_score_School <- happy_school + good_school + attention + good_relations

KIDSCREEN_table_score_Health <- cbind(general_health1, feeling_healthy, physical_activity, ability_run, energy) 
KIDSCREEN_table_score_Mood <- cbind(life, good_mood, enjoy , sad1, bad1, lonely1, happy)
KIDSCREEN_table_score_Family <- cbind(free_time, free_activities, time_parents, parents_fair, parents_talk, money_friends, money_expenses)
KIDSCREEN_table_score_Friends <- cbind(free_time_friends, enjoy_friends, help_friends, reliability_friends)
KIDSCREEN_table_score_School <- cbind(happy_school, good_school, attention, good_relations)

KIDSCREEN_total_score

## histograms
df <- data.frame(health=KIDSCREEN_total_score_Health, mood=KIDSCREEN_total_score_Mood,
                 family=KIDSCREEN_total_score_Family, friends=KIDSCREEN_total_score_Friends,
                 friends=KIDSCREEN_total_score_School, total=KIDSCREEN_total_score)

p1 <- ggplot(df, aes(x=KIDSCREEN_total_score_Family)) + labs(x = "Family") +
  geom_histogram(aes(y=..density..), binwidth=2, colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + theme(axis.title.y = element_blank(), plot.margin = margin(0.4, 0, 0, 0.6, "cm"))
p2 <- ggplot(df, aes(x=KIDSCREEN_total_score_Friends)) + labs(x = "Friends") + 
  geom_histogram(aes(y=..density..), binwidth=2, colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + theme(axis.title.y = element_blank(), plot.margin = margin(0.4, 0, 0, 0.6, "cm"))
p3 <- ggplot(df, aes(x=KIDSCREEN_total_score_Health)) + labs(x = "Health") + 
  geom_histogram(aes(y=..density..), binwidth=2, colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + theme(axis.title.y = element_blank(), plot.margin = margin(0.4, 0, 0, 0.6, "cm"))
p4 <- ggplot(df, aes(x=KIDSCREEN_total_score_Mood)) + labs(x = "Mood") + 
  geom_histogram(aes(y=..density..), binwidth=2, colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + theme(axis.title.y = element_blank(), plot.margin = margin(0.4, 0, 0, 0.6, "cm"))
p5 <- ggplot(df, aes(x=KIDSCREEN_total_score_School)) + labs(x = "School") + 
  geom_histogram(aes(y=..density..), binwidth=2, colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + theme(axis.title.y = element_blank(), plot.margin = margin(0.4, 0, 0, 0.6, "cm"))
p6 <- ggplot(df, aes(x=KIDSCREEN_total_score)) + labs(x = "Total score") + 
  geom_histogram(aes(y=..density..), binwidth=2, colour="black", fill="white") +
  geom_density(alpha=.2, fill="#66FF66") + theme(axis.title.y = element_blank(), plot.margin = margin(0.4, 0, 0, 0.6, "cm"))

ggarrange(p1, p2, p3, p4, p5, p6, 
          labels = c("A", "B", "C", "D", "E", "F"),
          ncol = 2, nrow = 3)
ggsave("figure4.png", width = 6, height = 6, dpi = 300)
dev.off()


######### STEP 1 - descriptive statistics ##########
WEMWBS_table <- mydata[,29:42]
lowerCor(WEMWBS_table, method = "spearman")

# Correlation plot
KIDSCREEN_table <- cbind(general_health1, feeling_healthy, physical_activity, ability_run, energy, life, good_mood, enjoy, sad1, bad1, lonely1, happy, free_time, free_activities, time_parents, parents_fair, parents_talk, money_friends, money_expenses, free_time_friends, enjoy_friends, help_friends, reliability_friends, happy_school, good_school, attention, good_relations)
png(filename="figure1.png", type="cairo", height = 8, width = 8, units = 'in', res=300)
cor.plot(lowerCor(KIDSCREEN_table, method = "spearman"), numbers=TRUE, main="Correlations between KIDSCREEN items", 
         cex=0.5, cex.axis=0.7, xlas = 2)
dev.off()

######### STEP 2 - IRT analyses (the mokken, ltm, and mirt packages) ##########
moscales.for.lowerbounds <- function( x, lowerbounds=seq(from=0.05,to=0.60,by=0.05) )
{
  ret.value <- NULL;
  for( lowerbound in lowerbounds )
  {
    tmp <- aisp( x,  lowerbound=lowerbound );
    if( is.null(ret.value) )
    {
      ret.value <- data.frame( "Item"=rownames(tmp), "Scales."=tmp[,1] );
    }
    else
    {
      ret.value <- cbind( ret.value, "Scales."=tmp[,1] );
    }
    names(ret.value)[ncol(ret.value)] <- paste("c=",sprintf("%.2f",lowerbound),sep="");
  }
  rownames(ret.value) <- NULL;
  ret.value;
}
# Compute scalability coefficients
coefH(WEMWBS_table)$H

coefH(KIDSCREEN_table_score_Health)$H
coefH(KIDSCREEN_table_score_Mood)$H
coefH(KIDSCREEN_table_score_Family)$H
coefH(KIDSCREEN_table_score_Friends)$H
coefH(KIDSCREEN_table_score_School)$H

# examine aisp for increasing c levels (run the function you defined above and give it a name)
motable.WEMWBS_table <- moscales.for.lowerbounds( WEMWBS_table )
motable.KIDSCREEN_table_Health <- moscales.for.lowerbounds( KIDSCREEN_table_score_Health )
motable.KIDSCREEN_table_Mood <- moscales.for.lowerbounds( KIDSCREEN_table_score_Mood )
motable.KIDSCREEN_table_Family <- moscales.for.lowerbounds( KIDSCREEN_table_score_Family )
motable.KIDSCREEN_table_Friends <- moscales.for.lowerbounds( KIDSCREEN_table_score_Friends )
motable.KIDSCREEN_table_School <- moscales.for.lowerbounds( KIDSCREEN_table_score_School )

# see the results
motable.WEMWBS_table
motable.KIDSCREEN_table_Health
motable.KIDSCREEN_table_Mood
motable.KIDSCREEN_table_Family
motable.KIDSCREEN_table_Friends
motable.KIDSCREEN_table_School

# save it as a data frame (Mokken scaling tables)
WEMWBS_table2 <- as.data.frame(motable.WEMWBS_table)
KIDSCREEN_table_health_2 <- as.data.frame(motable.KIDSCREEN_table_Health)
KIDSCREEN_table_mood_2 <- as.data.frame(motable.KIDSCREEN_table_Mood)
KIDSCREEN_table_family_2 <- as.data.frame(motable.KIDSCREEN_table_Family)
KIDSCREEN_table_friends_2 <- as.data.frame(motable.KIDSCREEN_table_Friends)
KIDSCREEN_table_school_2 <- as.data.frame(motable.KIDSCREEN_table_School)

######### STEP 3 - Parametric IRT ##########
# === ordinal items 
# Rating Scale model (equivalent of Rasch for ordinal items)

fit1.KS_health <- RSM(KIDSCREEN_table_score_Health)
fit1.KS_mood <- RSM(KIDSCREEN_table_score_Mood)
fit1.KS_family <- RSM(KIDSCREEN_table_score_Family)
fit1.KS_friends <- RSM(KIDSCREEN_table_score_Friends)
fit1.KS_school <- RSM(KIDSCREEN_table_score_School)

# separation reliability (proportion of item variance not due to error - similar to C-alpha)
ppr1 <- person.parameter(fit1.KS_health)
ppr2 <- person.parameter(fit1.KS_mood)
ppr3 <- person.parameter(fit1.KS_family)
ppr4 <- person.parameter(fit1.KS_friends)
ppr5 <- person.parameter(fit1.KS_school)

# item fit (between 0.6 and 1.4 acc to Wright BD, Linacre JM. Reasonable mean-square fit values. Rasch Meas Trans. 1994;8(2):370.)
itemfit.KS_health <- itemfit(ppr1)
itemfit.KS_mood <- itemfit(ppr2)
itemfit.KS_family <- itemfit(ppr3)
itemfit.KS_friends <- itemfit(ppr4)
itemfit.KS_school <- itemfit(ppr5)

# check min and max infit and outfit
min(itemfit.KS_health$i.infitMSQ)
max(itemfit.KS_health$i.infitMSQ)
min(itemfit.KS_health$i.outfitMSQ)
max(itemfit.KS_health$i.outfitMSQ)

min(itemfit.KS_mood$i.infitMSQ)
max(itemfit.KS_mood$i.infitMSQ)
min(itemfit.KS_mood$i.outfitMSQ)
max(itemfit.KS_mood$i.outfitMSQ)

min(itemfit.KS_family$i.infitMSQ)
max(itemfit.KS_family$i.infitMSQ)
min(itemfit.KS_family$i.outfitMSQ)
max(itemfit.KS_family$i.outfitMSQ)

min(itemfit.KS_friends$i.infitMSQ)
max(itemfit.KS_friends$i.infitMSQ)
min(itemfit.KS_friends$i.outfitMSQ)
max(itemfit.KS_friends$i.outfitMSQ)

min(itemfit.KS_school$i.infitMSQ)
max(itemfit.KS_school$i.infitMSQ)
min(itemfit.KS_school$i.outfitMSQ)
max(itemfit.KS_school$i.outfitMSQ)

#Personfit (z values should be </= 1.96)
personfit.KS_health <- personfit(ppr1)
personfit.KS_mood <- personfit(ppr2)
personfit.KS_family <- personfit(ppr3)
personfit.KS_friends <- personfit(ppr4)
personfit.KS_school <- personfit(ppr5)

# number of respondents that don't fit
length(personfit.KS_health$p.outfitZ[personfit.KS_health$p.outfitZ > 2])
length(personfit.KS_health$p.infitZ [personfit.KS_health$p.infitZ > 2])

length(personfit.KS_mood$p.outfitZ[personfit.KS_mood$p.outfitZ > 2])
length(personfit.KS_mood$p.infitZ [personfit.KS_mood$p.infitZ > 2])

length(personfit.KS_family$p.outfitZ[personfit.KS_family$p.outfitZ > 2])
length(personfit.KS_family$p.infitZ [personfit.KS_family$p.infitZ > 2])

length(personfit.KS_friends$p.outfitZ[personfit.KS_friends$p.outfitZ > 2])
length(personfit.KS_friends$p.infitZ [personfit.KS_friends$p.infitZ > 2])

length(personfit.KS_school$p.outfitZ[personfit.KS_school$p.outfitZ > 2])
length(personfit.KS_school$p.infitZ [personfit.KS_school$p.infitZ > 2])

# proportion of respondents that don't fit
(length(personfit.KS_health$p.outfitZ[personfit.KS_health$p.outfitZ > 2]))*100/(length(personfit.KS_health$p.outfitZ))
(length(personfit.KS_health$p.outfitZ [personfit.KS_health$p.outfitZ < -2]))*100/(length(personfit.KS_health$p.outfitZ))

(length(personfit.KS_mood$p.outfitZ[personfit.KS_mood$p.outfitZ > 2]))*100/(length(personfit.KS_mood$p.outfitZ))
(length(personfit.KS_mood$p.outfitZ [personfit.KS_mood$p.outfitZ < -2]))*100/(length(personfit.KS_mood$p.outfitZ))

(length(personfit.KS_family$p.outfitZ[personfit.KS_family$p.outfitZ > 2]))*100/(length(personfit.KS_family$p.outfitZ))
(length(personfit.KS_family$p.outfitZ [personfit.KS_family$p.outfitZ < -2]))*100/(length(personfit.KS_family$p.outfitZ))

(length(personfit.KS_friends$p.outfitZ[personfit.KS_friends$p.outfitZ > 2]))*100/(length(personfit.KS_friends$p.outfitZ))
(length(personfit.KS_friends$p.outfitZ [personfit.KS_friends$p.outfitZ < -2]))*100/(length(personfit.KS_friends$p.outfitZ))

(length(personfit.KS_school$p.outfitZ[personfit.KS_school$p.outfitZ > 2]))*100/(length(personfit.KS_school$p.outfitZ))
(length(personfit.KS_school$p.outfitZ [personfit.KS_school$p.outfitZ < -2]))*100/(length(personfit.KS_school$p.outfitZ))

######### STEP 4 - Confirmatory factor analysis ##########

#EFA
# factor analysis via parallel analysis plot
p1 <- fa.parallel(KIDSCREEN_table,cor="poly")
png(filename="figure2.png", type="cairo", height = 6, width = 6, units = 'in', res=300)
plot(p1)
dev.off()

# very simple structure analysis plot
p1 <- vss(KIDSCREEN_table, 5)
png(filename="figure3.png", type="cairo", height = 6, width = 6, units = 'in', res=300)
plot(p1)
dev.off()

# default FA - 5 factor, min residual & principal axis
fa(KIDSCREEN_table,  nfactors=5, fm="minres", n.iter=10)
fa(KIDSCREEN_table,  nfactors=5, fm="pa")
# plot the fa solution
plot(fa(KIDSCREEN_table,  nfactors=5, fm="pa"))
# plot diagram fa solution
fa.diagram(fa(KIDSCREEN_table,  nfactors=5, fm="pa"))
# pca (in case you need it, but would not advise - data reduction, but not structural validity test)
# principal(BESdata,5,rotate="varimax")

# hierarchical cluster analysis using ICLUST (groups items)
iclust(KIDSCREEN_table, title="KIDSCREEN_table using Pearson correlations")
summary(iclust(KIDSCREEN_table))
iclust.diagram(iclust(KIDSCREEN_table, title="KIDSCREEN_table using Pearson correlations"))
# hierarchical factor solution to find omega coefficient
omega(KIDSCREEN_table, nfactors=5, sl=FALSE)
omega(KIDSCREEN_table, nfactors=5, sl=TRUE)

# omega with polychoric matrix
KIDSCREEN_table.poly <- polychoric(KIDSCREEN_table)
omega(KIDSCREEN_table.poly$rho, nfactors=5,  sl=FALSE)

# CFA
# specify the model
CFA.BES <- '
# factor structure
health =~ general_health1 + feeling_healthy + physical_activity + ability_run + energy 
mood =~ life + good_mood + enjoy + sad1 + bad1 + lonely1 + happy
family =~ free_time + free_activities + time_parents + parents_fair + parents_talk + money_friends + money_expenses
friends =~ free_time_friends + enjoy_friends + help_friends + reliability_friends
school =~ happy_school + good_school + attention + good_relations
'

# fit the model
fitCFA.BES <- lavaan::cfa(CFA.BES, data=KIDSCREEN_table)
# model summary
summary(fitCFA.BES, standardized=TRUE, fit.measures = TRUE)
# coefficients only
coef(fitCFA.BES)
# CFA diagram from psych package
lavaan.diagram(fitCFA.BES, errors=TRUE)
# OR diagram from semPlot package
semPaths(fitCFA.BES,what="std", label.cex=0.3, edge.label.cex=0.5, sizeLat=5, sizeMan=4, curvePivot = TRUE, rotation=4)

semPaths(fitCFA.BES,what="std",layout="circle",edge.label.cex=0.5, curvePivot = TRUE, rotation=3)


############### STEP 5 - CCT
# CTT for a single scale
# gives:
# C-alpha  & CIs
# Guttman's lambda 6 (squared multiple correlation)
# and CTT item properties - reliability if item excluded, item statistics, response frequencies(%)

# Alpha by bootstrapping
ci.reliability(data=KIDSCREEN_table, type="alpha", conf.level = 0.95, interval.type="perc", B=100)
ci.reliability(data=KIDSCREEN_table_score_Health, type="alpha", conf.level = 0.95, interval.type="perc", B=100)
ci.reliability(data=KIDSCREEN_table_score_Mood, type="alpha", conf.level = 0.95, interval.type="perc", B=100)
ci.reliability(data=KIDSCREEN_table_score_Family, type="alpha", conf.level = 0.95, interval.type="perc", B=100)
ci.reliability(data=KIDSCREEN_table_score_Friends, type="alpha", conf.level = 0.95, interval.type="perc", B=100)
ci.reliability(data=KIDSCREEN_table_score_School, type="alpha", conf.level = 0.95, interval.type="perc", B=100)

# Beta
guttman(KIDSCREEN_table)
guttman(KIDSCREEN_table_score_Health)
guttman(KIDSCREEN_table_score_Mood)
guttman(KIDSCREEN_table_score_Family)
guttman(KIDSCREEN_table_score_Friends)
guttman(KIDSCREEN_table_score_School)

# Guttman lambda 6 (G6)
splitHalf(KIDSCREEN_table) 
splitHalf(KIDSCREEN_table_score_Health) 
splitHalf(KIDSCREEN_table_score_Mood) 
splitHalf(KIDSCREEN_table_score_Family) 
splitHalf(KIDSCREEN_table_score_Friends) 
splitHalf(KIDSCREEN_table_score_School) 

# Omega
ci.reliability(data=KIDSCREEN_table, type="omega", conf.level = 0.95, interval.type="perc", B=100)
ci.reliability(data=KIDSCREEN_table_score_Health, type="omega", conf.level = 0.95, interval.type="perc", B=100)
ci.reliability(data=KIDSCREEN_table_score_Mood, type="omega", conf.level = 0.95, interval.type="perc", B=100)
ci.reliability(data=KIDSCREEN_table_score_Family, type="omega", conf.level = 0.95, interval.type="perc", B=100)
ci.reliability(data=KIDSCREEN_table_score_Friends, type="omega", conf.level = 0.95, interval.type="perc", B=100)
ci.reliability(data=KIDSCREEN_table_score_School, type="omega", conf.level = 0.95, interval.type="perc", B=100)

# Omega & CIs as per Dunn et al 2014 (http://onlinelibrary.wiley.com/doi/10.1111/bjop.12046/abstract
# ***** recommended number of bootstraps is 1000, but can be slow, so change if needed *****
# ***** interval.type="bca" is recommended, but if not working "perc" may give close results

##################### STEP 6
# check scores basics

# examine frequencies
table(KIDSCREEN_table, exclude=NULL)
table(KIDSCREEN_table_score_Health, exclude=NULL)
table(KIDSCREEN_table_score_Mood, exclude=NULL)
table(KIDSCREEN_table_score_Family, exclude=NULL)
table(KIDSCREEN_table_score_Friends, exclude=NULL)
table(KIDSCREEN_table_score_School, exclude=NULL)

# check descriptives
summary(KIDSCREEN_total_score)
summary(KIDSCREEN_total_score_Health)
summary(KIDSCREEN_total_score_Mood)
summary(KIDSCREEN_total_score_Family)
summary(KIDSCREEN_total_score_Friends)
summary(KIDSCREEN_total_score_School)

mean(KIDSCREEN_table)
sd(KIDSCREEN_table)
min(KIDSCREEN_table)
max(KIDSCREEN_table)

mean(KIDSCREEN_total_score)
sd(KIDSCREEN_total_score)
min(KIDSCREEN_total_score)
max(KIDSCREEN_total_score)

mean(KIDSCREEN_total_score_Health)
mean(KIDSCREEN_total_score_Mood)
mean(KIDSCREEN_total_score_Family)
mean(KIDSCREEN_total_score_Friends)
mean(KIDSCREEN_total_score_School)

sd(KIDSCREEN_total_score_Health)
sd(KIDSCREEN_total_score_Mood)
sd(KIDSCREEN_total_score_Family)
sd(KIDSCREEN_total_score_Friends)
sd(KIDSCREEN_total_score_School)
