####Analysis 18-19####

a<-gameweekpoints%>%
  group_by(Week) %>%
  mutate(rank=rank(-Cumulative,ties.method = "first"))

x <- bind_rows(finaldf)
x<-replace(x, is.na(x), 0)

fourteenth<-lapply(finaldf,function(w){lapply(w[,c(11:48)],function(e){sort(e,TRUE)[25]})})

fourteenth <- replace(unlist(fourteenth),is.na(unlist(fourteenth)),0)

fourteenth[!is.na(match(fourteenth,4))]

split(fourteenth,rep(1:5,each=38))

nonscorers<-lapply(finaldf,function(w){lapply(w[,c(11:48)],function(e){sort(e,TRUE)[15:25]})})

lapply(nonscorers,function(x){sum(unlist(x)>1,na.rm = T)})

##
q<-x %>%
  group_by(`Picked by`) %>%
  
  mutate(gwrank=rank(row_number()))

q[q$gwrank==14,]


arrange(desc(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`,`11`,`12`,`13`,`14`,`15`,`16`,`17`,`18`,`19`,`20`,`21`,`22`,`23`,`24`,`25`,`26`,
             `27`,`28`,`29`,`30`,`31`,`32`,`33`,`34`,`35`,`36`,`37`,`38`,.by_group=T))
