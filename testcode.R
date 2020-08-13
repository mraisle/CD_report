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

##first let's fix up the data
#ok so We'll put the % in the title for the state, and try to figure out the same for MA 4
CAArecurring <- `recurring-violations_All_pg3_MA-4`[-c(2,3,5,6),]
CAArecurring <- CAArecurring %>%
  select(CD,Facilities, Percent)
names(CAArecurring)[1] <- "Region"
CAArecurring[1,1] <- as.character("Rest of State")
CAArecurring[2,1] <- as.character("MA 4")
CAArecurring$Region2 <- as.character("all of MA")
CAArecurring[2,4] <- as.character("MA 4")
CAArecurring$RecurringViolators <- (as.numeric(CAArecurring[1,2]-CAArecurring[2,2]))
CAArecurring[2,5] <- as.numeric(CAArecurring[2,2])
CAArecurring$Proportional <- (as.numeric((100/(CAArecurring[1,2]))*CAArecurring[1,5]))
CAArecurring[2,6] <- as.numeric(100-CAArecurring[1,6])
CAArecurring[3] <-round(CAArecurring[3],2)


##then let's try to make a pie chart

# Compute the position of labels
CAArecurring <- CAArecurring %>%
  arrange(desc(Region)) %>%
  mutate(prop = Proportional) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

CAApie<- ggplot(CAArecurring, aes(x="", y=Proportional, fill=Region))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0) +
  geom_text(aes(y = ypos, label = RecurringViolators), family="Georgia", size=8) +
  geom_text(aes(y = ypos, label = (paste(Percent,"% of regulated facilities\nin", Region2))),
            family="Georgia", size=4, vjust=2) +
  ggtitle("Number of Clean Air Act Facilities that spent\nat least 25% of the last 3 years in Violation")+
  theme_meg()+
  theme(axis.line.y=element_blank(), axis.line.x = element_blank(),
        axis.title.y=element_blank(), axis.text.y=element_blank(), axis.text.x=element_blank(),
        axis.title.x=element_blank(), legend.title=element_blank())

CAApie



# Create Data
data <- data.frame(
  group=LETTERS[1:5],
  value=c(13,7,9,21,2)
)

# Compute the position of labels
data <- data %>%
  arrange(desc(group)) %>%
  mutate(prop = value / sum(data$value) *100) %>%
  mutate(ypos = cumsum(prop)- 0.5*prop )

# Basic piechart
testpie <- ggplot(data, aes(x="", y=prop, fill=group)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +
  theme(legend.position="none") +

  geom_text(aes(y = ypos, label = group), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")

testpie
