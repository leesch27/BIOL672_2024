# Lee_Schoneman_unit1.2_BIOL672.r

library(ggplot2)
library(grid)
library(Hmisc)

#preliminary variable creation and analysis
personality_data <- read.csv("/Users/leesch/BIOL672_Unit_1/data.csv")
introversion_score <- personality_data$Introversion.Score
sensing_score <- personality_data$Sensing.Score
thinking_score <- personality_data$Thinking.Score
judging_score <- personality_data$Judging.Score
personality <- personality_data$Personality

introversion_mean<- tapply(introversion_score,personality,mean) 
sensing_mean<- tapply(sensing_score,personality,mean) 
thinking_mean<- tapply(thinking_score,personality,mean) 
judging_mean<- tapply(judging_score,personality,mean)

introversion_mean
sensing_mean
thinking_mean
judging_mean

introversion_sd<- tapply(introversion_score,personality,sd) 
sensing_sd<- tapply(sensing_score,personality,sd) 
thinking_sd<- tapply(thinking_score,personality,sd) 
judging_sd<- tapply(judging_score,personality,sd)

introversion_sd
sensing_sd
thinking_sd
judging_sd

var.names <- rep(c("introversion", "sensing", "thinking", "judging"), each = 16)
personality.vars <- rep(c("ENFJ", "ENFP", "ENTJ", "ENTP", "ESFJ", "ESFP", "ESTJ", "ESTP", "INFJ", "INFP", "INTJ", "INTP", "ISFJ", "ISFP", "ISTJ", "ISTP"), time = 4)
personality.means<-c(introversion_mean, sensing_mean, thinking_mean, judging_mean)
personality.sd<-c(introversion_sd, sensing_sd, thinking_sd, judging_sd)
personality.df <- data.frame(var.names, personality.vars, personality.means, personality.sd)
introversion.df <- data.frame(introversion_mean, introversion_sd)
sensing.df <- data.frame(sensing_mean, sensing_sd)
thinking.df <- data.frame(thinking_mean, thinking_sd)
judging.df <- data.frame(judging_mean, judging_sd)

# anova testing
introversion.anova = oneway.test(introversion_score~personality)
sensing.anova = oneway.test(sensing_score~personality)
thinking.anova = oneway.test(thinking_score~personality)
judging.anova = oneway.test(judging_score~personality)
print(introversion.anova)
print(sensing.anova)
print(thinking.anova)
print(judging.anova)

sink(file = '/Users/leesch/BIOL672_Unit_1/personality_anova_data.txt')
print(personality.means)
print(summary(personality.means))
print(personality.sd)
print(summary(personality.sd))
print(introversion.anova)
print(sensing.anova)
print(thinking.anova)
print(judging.anova)
sink()

# nonparametric tests and correlation tests
kruskal_introversion <- kruskal.test(introversion_score~personality)
kruskal_sensing <- kruskal.test(sensing_score~personality)
kruskal_thinking <- kruskal.test(thinking_score~personality)
kruskal_judging <- kruskal.test(judging_score~personality)

pearson_introversion_age <- rcorr(personality_data[1], personality_data[4], type="pearson")
pearson_introversion_gender <- rcorr(personality_data[2], personality_data[4], type="pearson")
spearman_introversion_age <- rcorr(personality_data[1], personality_data[4], type="spearman")
spearman_introversion_gender <- rcorr(personality_data[2], personality_data[4], type="spearman")

#linear regression
linreg_introversion_age <- lm(unlist(personality_data[4])~unlist(personality_data[1]))
linreg_introversion_gender <- lm(unlist(personality_data[4])~unlist(personality_data[2]))

#pairwise t tests
bonf.introversion <- pairwise.t.test(introversion_score, personality, p.adj = "bonf")
bonf.sensing <- pairwise.t.test(sensing_score, personality, p.adj = "bonf")
bonf.thinking <- pairwise.t.test(thinking_score, personality, p.adj = "bonf")
bonf.judging <- pairwise.t.test(judging_score, personality, p.adj = "bonf")
bh.introversion <- pairwise.t.test(introversion_score, personality, p.adj = "BH")
bh.sensing <- pairwise.t.test(sensing_score, personality, p.adj = "BH")
bh.thinking <- pairwise.t.test(thinking_score, personality, p.adj = "BH")
bh.judging <- pairwise.t.test(judging_score, personality, p.adj = "BH")

sink(file = '/Users/leesch/BIOL672_Unit_1/personality_pairwise_t_data.txt')
print(bonf.introversion)
print(bonf.sensing)
print(bonf.thinking)
print(bonf.judging)
print(bh.introversion)
print(bh.sensing)
print(bh.thinking)
print(bh.judging)
sink() 

#plotting
introversion_plot<- ggplot(introversion.df, aes(x=row.names(introversion.df), y=introversion_mean)) + 
  geom_bar(stat="identity", color="black", fill="blue", position=position_dodge()) +
  geom_errorbar(aes(ymin = introversion_mean-introversion_sd, ymax=introversion_mean+introversion_sd), width=.2, position = position_dodge(.9)) + 
  xlab("Personality Type") + theme(text = element_text(size = 8))
print(introversion_plot)

sensing_plot<- ggplot(sensing.df, aes(x=row.names(sensing.df), y=sensing_mean)) + 
  geom_bar(stat="identity", color="black", fill="red", position=position_dodge()) +
  geom_errorbar(aes(ymin = sensing_mean-sensing_sd, ymax=sensing_mean+sensing_sd), width=.2, position = position_dodge(.9)) + 
  xlab("Personality Type") + theme(text = element_text(size = 8))
print(sensing_plot)

thinking_plot<- ggplot(thinking.df, aes(x=row.names(thinking.df), y=thinking_mean)) + 
  geom_bar(stat="identity", color="black", fill="darkgreen", position=position_dodge()) +
  geom_errorbar(aes(ymin = thinking_mean-thinking_sd, ymax=thinking_mean+thinking_sd), width=.2, position = position_dodge(.9)) + 
  xlab("Personality Type") + theme(text = element_text(size = 8))
print(thinking_plot)

judging_plot<- ggplot(judging.df, aes(x=row.names(judging.df), y=judging_mean)) + 
  geom_bar(stat="identity", color="black", fill="purple", position=position_dodge()) +
  geom_errorbar(aes(ymin = judging_mean-judging_sd, ymax=judging_mean+judging_sd), width=.2, position = position_dodge(.9)) + 
  xlab("Personality Type") + theme(text = element_text(size = 8))
print(judging_plot)

pushViewport(viewport(layout = grid.layout(2, 2)))
print(introversion_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(sensing_plot, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))
print(thinking_plot, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
print(judging_plot, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
