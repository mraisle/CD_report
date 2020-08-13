# create a dataset
specie <- c(rep("sorgho" , 3) , rep("poacee" , 3) , rep("banana" , 3) , rep("triticum" , 3) )
condition <- rep(c("normal" , "stress" , "Nitrogen") , 4)
value <- abs(rnorm(12 , 0 , 15))
data <- data.frame(specie,condition,value)

theme_meg <- function () {
  theme_bw(base_family="Georgia") %+replace%
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.border = element_blank(),
          axis.line = element_line(colour = "black"), axis.title.x=element_text(face = "bold", size = 14),
          axis.title.y=element_text(face = "bold", size = 14, angle=90), plot.title=element_text(face = "bold", size = 14, hjust =.5),
          axis.text.x=element_text(face = "bold", size = 14), axis.text.y=element_text(face = "bold", size = 12))
}


scale_fill_manual(values=c("#e56d13" , "#25539f", "#d43a69"))+
  labs(y="Inspections per 1000 Facilities", x="Program")+
  geom_text(aes(label=Inspectionsper1000), position = position_dodge(width=1), size=8)+
  ggtitle("Comparing Inspections per 1000 Facilities")+
  theme_bw()+
  theme(legend.position = c(0.8, 0.5))

scale_color_viridis(option = "D", discrete = FALSE)+

# Grouped
test <- ggplot(data, aes(fill=condition, y=value, x=specie)) +
  geom_bar(position="dodge", stat="identity")+
  theme_meg()

test


#testing pie charts


df <- data.frame(
  group = c("RestofState", "MA 4"),
  value = c(75,3)
)
head(df)

bp<- ggplot(df, aes(x="", y=value, fill=group))+
  geom_bar(width = 1, stat = "identity")
bp

pie <- bp + coord_polar("y", start=0)
pie


##let's attempt these pie charts

#RCRA

##first let's fix up the data
#ok so We'll put the % in the title for the state, and try to figure out the same for MA 4
RCRArecurring <- `recurring-violations_All_pg3_MA-4`[-c(1,2,4,5),]
RCRArecurring <- RCRArecurring %>%
  select(CD,Facilities, Percent)
names(RCRArecurring)[1] <- "Region"
RCRArecurring[1,1] <- as.character("Rest of State")
RCRArecurring[2,1] <- as.character("MA 4")
RCRArecurring$Region2 <- as.character("all of MA")
RCRArecurring[2,4] <- as.character("MA 4")
RCRArecurring$RecurringViolators <- (as.numeric(RCRArecurring[1,2]-RCRArecurring[2,2]))
RCRArecurring[2,5] <- as.numeric(RCRArecurring[2,2])
RCRArecurring$Proportional <- (as.numeric((100/(RCRArecurring[1,2]))*RCRArecurring[1,5]))
RCRArecurring[2,6] <- as.numeric(100-RCRArecurring[1,6])
RCRArecurring[3] <-round(RCRArecurring[3],2)


##RCRA pie chart

# Compute the position of labels
RCRArecurring <- RCRArecurring %>%
  arrange(desc(Region)) %>%
  mutate(prop = Proportional) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

RCRApie<- ggplot(RCRArecurring, aes(x="", y=Proportional, fill=Region))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0) +
  geom_text(aes(y = ypos, label = RecurringViolators), family="Georgia", size=8) +
  geom_text(aes(y = ypos, label = (paste(Percent,"% of regulated facilities\nin", Region2))),
            family="Georgia", size=4, vjust=2) +
  ggtitle("Number of Resource Conservation and Recovery Act\nFacilities that spent at least 25% of the last 3 years in Violation")+
  theme_meg()+
  theme(plot.title=element_text(face = "bold", size = 12, hjust =.5),
        axis.line.y=element_blank(), axis.line.x = element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank(),
        axis.title.x=element_blank(), legend.title=element_blank())

RCRApie



