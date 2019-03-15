library(arules)
library(arulesViz)

rules_df = subset(terrorism, select = c("country_txt", "region_txt", "attacktype1_txt", "targtype1_txt", "gname", "targsubtype1_txt", "weaptype1_txt", "nkill", "nwound"))

#change nkill and nwound to factors
rules_df <- rules_df %>%
  mutate(nkill = ifelse(nkill==0, 0, 
                        ifelse(nkill<2, 1, 
                               ifelse(nkill <6, 2, 
                                      ifelse(nkill < 16, 3, 4))))) %>%
  mutate(nwound = ifelse(nwound==0, 0, 
                         ifelse(nwound<2, 1, 
                                ifelse(nwound <6, 2, 
                                       ifelse(nwound < 16, 3, 4)))))

#change everything to factors
rules_df$country_txt <- as.factor(rules_df$country_txt)
rules_df$region_txt <- as.factor(rules_df$region_txt)
rules_df$attacktype1_txt <- as.factor(rules_df$attacktype1_txt)
rules_df$targtype1_txt <- as.factor(rules_df$targtype1_txt)
rules_df$gname <- as.factor(rules_df$gname)
rules_df$targsubtype1_txt <- as.factor(rules_df$targsubtype1_txt)
rules_df$weaptype1_txt <- as.factor(rules_df$weaptype1_txt)
rules_df$nkill <- as.factor(rules_df$nkill)
rules_df$nwound <- as.factor(rules_df$nwound)

terrorism_rules <- apriori(rules_df, parameter=list(support =0.01, confidence =0.5, minlen=2, maxlen=5))
inspect(head(sort(terrorism_rules, by="lift"),3))

rules_df = subset(rules_df, rules_df$gname != 'Unknown' & rules_df$attacktype1_txt != 'Unknown' & 
                    rules_df$targtype1_txt != 'Unknown' & rules_df$nkill != 'Unknown' & 
                    rules_df$nwound != 'Unknown' & rules_df$weaptype1_txt != 'Unknown',
                  select = -c(targsubtype1_txt, region_txt, country_txt, nwound))
rules_df %>%
  group_by(gname) %>%
  summarise(nr_of_attacks = n()) %>%
  arrange(desc(nr_of_attacks)) %>%
  head(n=10)

taliban_rules <- apriori(rules_df, parameter=list(support=0.01, confidence=0.1, minlen=1, maxlen=5), appearance = list(rhs='gname=Taliban', default="lhs"))  
inspect(head(sort(taliban_rules, by='lift'), 10))
#The Taliban tends to attack the Police extremely frequently with various types of weapons, firearms and explosives most commonly. They also seem to kill a small amount per attack, less than 6 but more than 1.
plot(taliban_rules, method = "graph")

isil_rules <- apriori(rules_df, parameter=list(support=0.01, confidence=0.1, minlen=1, maxlen=5), appearance = list(rhs='gname=Islamic State of Iraq and the Levant (ISIL)', default="lhs"))
inspect(head(sort(isil_rules, by='lift'), 10))
#ISIL uses bombs and explosives almost exclusively on citizens. They ofcourse are famous for their terror campaigns targetting innocent citizens. ISIL also kills a small amount per attack, similar to Taliban.
plot(isil_rules, method = "graph")

FMLN_rules <- apriori(rules_df, parameter=list(support=0.01, confidence=0.1, minlen=1, maxlen=5), appearance = list(rhs='gname=Farabundo Marti National Liberation Front (FMLN)', default="lhs"))
inspect(head(sort(FMLN_rules, by='lift'), 10))
plot(FMLN_rules, method = "graph")

FMLN_rules <- apriori(rules_df, parameter=list(support=0.01, confidence=0.1, minlen=1, maxlen=5), appearance = list(rhs='gname=Farabundo Marti National Liberation Front (FMLN)', default="lhs"))
inspect(head(sort(FMLN_rules, by='lift'), 10))
plot(FMLN_rules, method = "graph")