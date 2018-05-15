library(plumber)
library(analogsea)
library(jsonlite)
library(ggplot2)
library(igraph)
library(ggraph)
library(markovchain)
library(diagram)
library(dplyr)

getwd()

#retrieving data
a = read_json("C:\\Users\\daryl\\Downloads\\assessment\\data\\formatted-issue.json")
a$fields$created

long <- c("New Feature", "Sub-task", "Task")
short <- c("Bug", "Improvement", "Test", "Wish")


avro <- stream_in(file("avro-issues.json"))

names(avro_issues)[names(avro_issues)=="issue"] <- "key"

head(avro,10)
str(avro)
avro_flat = flatten(avro)

avro_daycounts %>%
  filter(status %in% c("Open")) %>%
  ggplot(aes(x = as.POSIXct(day), y = (count))) +
  geom_point() +
  geom_smooth()

avro_daycounts %>%
  ggplot(aes(x = as.POSIXct(day), y = count, col = factor(status))) +
  geom_point() +
  geom_smooth()


avro_transitions %>%
  #  filter(key %in% c("AVRO-2")) %>%
  ggplot(aes(x = as.POSIXct(when), fill = transition)) +
  geom_histogram(position = "fill") +
  facet_wrap(~priority)

# An arc diagram
ggraph(graph, layout = 'linear') + 
  geom_edge_arc(aes(colour = factor(year)))

keten = avro_transitions[c(3,8)]
keten[is.na(keten)] <- "Start"

telling_keten = keten %>%
  count(from_status, to_status, sort = T)

keten_graph = telling_keten %>%
  graph_from_data_frame()

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(keten_graph, layout = 'linear') +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  geom_edge_arc(aes(colour = (n)), arrow = a)

trans.matrix <- function(X, prob=T)
{
  tt <- table( c(X[,-ncol(X)]), c(X[,-1]) )
  if(prob) tt <- tt / rowSums(tt)
  tt
}

trans.kans <- function(tabel, from, to){
  t1 = tabel %>%
    filter(from_status == from & to_status == to)
  
  t2 = tabel %>%
    filter(from_status == from)
  
  (nrow(t1)/nrow(t2))
  
  if(is.na(nrow(t1)/nrow(t2)) == T){
    return(0)
  } else {
    return(nrow(t1)/nrow(t2))
  }
  
}

keten2 = keten

keten2[keten2 == "Start"] <- NA
keten2 = keten2[complete.cases(keten2),]


transitie = trans.matrix(as.matrix(keten))
transitie2 = trans.matrix(as.matrix(keten2))


blockersL = subset(avro_transitions, avro_transitions$priority == "Blocker" & avro_transitions$issue_type %in% long)
blockersS = subset(avro_transitions, avro_transitions$priority == "Blocker" & avro_transitions$issue_type %in% short)
criticalsL = subset(avro_transitions, avro_transitions$priority == "Critical" & avro_transitions$issue_type %in% long)
criticalsS = subset(avro_transitions, avro_transitions$priority == "Critical" & avro_transitions$issue_type %in% short)
majorsL = subset(avro_transitions, avro_transitions$priority == "Major" & avro_transitions$issue_type %in% long)
majorsS = subset(avro_transitions, avro_transitions$priority == "Major" & avro_transitions$issue_type %in% short)
minorsL = subset(avro_transitions, avro_transitions$priority == "Minor" & avro_transitions$issue_type %in% long)
minorsS = subset(avro_transitions, avro_transitions$priority == "Minor" & avro_transitions$issue_type %in% short)
trivialsL = subset(avro_transitions, avro_transitions$priority == "Trivial" & avro_transitions$issue_type %in% long)
trivialsS = subset(avro_transitions, avro_transitions$priority == "Trivial" & avro_transitions$issue_type %in% short)

blockersKetenL = blockersL[c(3,8)]
blockersKetenS = blockersS[c(3,8)]
criticalsKetenL = criticalsL[c(3,8)]
criticalsKetenS = criticalsS[c(3,8)]
majorsKetenL = majorsL[c(3,8)]
majorsKetenS = majorsS[c(3,8)]
minorsKetenL = minorsL[c(3,8)]
minorsKetenS = minorsS[c(3,8)]
trivialsKetenL = trivialsL[c(3,8)]
trivialsKetenS = trivialsS[c(3,8)]

#blockersKeten[is.na(blockersKeten)] <- "Start"
#criticalsKeten[is.na(criticalsKeten)] <- "Start"
#majorsKeten[is.na(majorsKeten)] <- "Start"
#minorsKeten[is.na(minorsKeten)] <- "Start"
#trivialsKeten[is.na(trivialsKeten)] <- "Start"

blockersKetenL2 = blockersKetenL[complete.cases(blockersKetenL),]
blocker.transitie = trans.matrix(as.matrix(blockersKetenL2))

blockersKetenS2 = blockersKetenS[complete.cases(blockersKetenS),]
blocker.transitie = trans.matrix(as.matrix(blockersKetenS2))

blockersTelling = blockersKeten %>%
  count(from_status, to_status, sort = T)

criticalKeten2 = criticalsKetenL[complete.cases(criticalsKetenL),]
critical.transitie = trans.matrix(as.matrix(criticalKeten2))

criticalKeten2 = criticalsKetenS[complete.cases(criticalsKetenS),]
critical.transitie = trans.matrix(as.matrix(criticalKeten2))

criticalsTelling = criticalsKeten %>%
  count(from_status, to_status, sort = T)

majorKeten2 = majorsKetenL[complete.cases(majorsKetenL),]
major.transitie = trans.matrix(as.matrix(majorKeten2))

majorKeten2 = majorsKetenS[complete.cases(majorsKetenS),]
major.transitie = trans.matrix(as.matrix(majorKeten2))

majorsTelling = majorsKeten %>%
  count(from_status, to_status, sort = T)

minorKeten2 = minorsKetenL[complete.cases(minorsKetenL),]
minor.transitie = trans.matrix(as.matrix(minorKeten2))

minorKeten2 = minorsKetenS[complete.cases(minorsKetenS),]
minor.transitie = trans.matrix(as.matrix(minorKeten2))

minorsTelling = minorsKeten %>%
  count(from_status, to_status, sort = T)

trivialKeten2 = trivialsKetenL[complete.cases(trivialsKetenL),]
trivial.transitie = trans.matrix(as.matrix(trivialKeten2))

trivialKeten2 = trivialsKetenS[complete.cases(trivialsKetenS),]
trivial.transitie = trans.matrix(as.matrix(trivialKeten2))

trivialsTelling = trivialsKeten %>%
  count(from_status, to_status, sort = T)

blocker_graph = blockersTelling %>%
  graph_from_data_frame()

critical_graph = criticalsTelling %>%
  graph_from_data_frame()

major_graph = majorsTelling %>%
  graph_from_data_frame()

minor_graph = minorsTelling %>%
  graph_from_data_frame()

trivial_graph = trivialsTelling %>%
  graph_from_data_frame()

ggraph(majorr_graph, layout = 'linear') +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  geom_edge_arc(aes(colour = (n)), arrow = a)

toestanden = c("Open", "In Progress", "Patch Available", "Resolved", "Reopened", "Closed")
prioriteit = c("Blocker", "Critical", "Major", "Minor", "Trivial")
typen = c("Bug", "Improvement", "New Feature", "Sub-task", "Task", "Test", "Wish")

mcJIRA <- new("markovchain", states=toestanden,
              transitionMatrix=matrix(data=c(0, 0.02658960, 0.61387283, 0.35953757, 0, 0,
                                             0.10909091, 0, 0.63636364, 0.25454545, 0 ,0,
                                             0.09246231, 0.01105528, 0, 0.89648241, 0, 0,
                                             0, 0, 0, 0, 0.03286385, 0.96713615,
                                             0, 0, 0, 0, 0, 1),
                                      byrow=TRUE, nrow=6, ncol = 6), name="JIRA")

mcBlockerL <- new("markovchain", states=toestanden,
                  transitionMatrix=matrix(data=c(trans.kans(blockersKetenL, toestanden[1], toestanden[1]), trans.kans(blockersKetenL, toestanden[1], toestanden[2]), trans.kans(blockersKetenL, toestanden[1], toestanden[3]), trans.kans(blockersKetenL, toestanden[1], toestanden[4]), trans.kans(blockersKetenL, toestanden[1], toestanden[5]), trans.kans(blockersKetenL, toestanden[1], toestanden[6]),
                                                 trans.kans(blockersKetenL, toestanden[2], toestanden[1]), trans.kans(blockersKetenL, toestanden[2], toestanden[2]), trans.kans(blockersKetenL, toestanden[2], toestanden[3]), trans.kans(blockersKetenL, toestanden[2], toestanden[4]), trans.kans(blockersKetenL, toestanden[2], toestanden[5]), trans.kans(blockersKetenL, toestanden[2], toestanden[6]),
                                                 trans.kans(blockersKetenL, toestanden[3], toestanden[1]), trans.kans(blockersKetenL, toestanden[3], toestanden[2]), trans.kans(blockersKetenL, toestanden[3], toestanden[3]), trans.kans(blockersKetenL, toestanden[3], toestanden[4]), trans.kans(blockersKetenL, toestanden[3], toestanden[5]), trans.kans(blockersKetenL, toestanden[3], toestanden[6]),
                                                 trans.kans(blockersKetenL, toestanden[4], toestanden[1]), trans.kans(blockersKetenL, toestanden[4], toestanden[2]), trans.kans(blockersKetenL, toestanden[4], toestanden[3]), trans.kans(blockersKetenL, toestanden[4], toestanden[4]), trans.kans(blockersKetenL, toestanden[4], toestanden[5]), trans.kans(blockersKetenL, toestanden[4], toestanden[6]),
                                                 0, 0, 0, 0, 1, 0, 
                                                 0, 0, 0, 0, 0, 1),
                                          byrow=TRUE, nrow=6, ncol = 6), name="JIRA")

mcBlockerS <- new("markovchain", states=toestanden,
                  transitionMatrix=matrix(data=c(trans.kans(blockersKetenS, toestanden[1], toestanden[1]), trans.kans(blockersKetenS, toestanden[1], toestanden[2]), trans.kans(blockersKetenS, toestanden[1], toestanden[3]), trans.kans(blockersKetenS, toestanden[1], toestanden[4]), trans.kans(blockersKetenS, toestanden[1], toestanden[5]), trans.kans(blockersKetenS, toestanden[1], toestanden[6]),
                                                 trans.kans(blockersKetenS, toestanden[2], toestanden[1]), trans.kans(blockersKetenS, toestanden[2], toestanden[2]), trans.kans(blockersKetenS, toestanden[2], toestanden[3]), trans.kans(blockersKetenS, toestanden[2], toestanden[4]), trans.kans(blockersKetenS, toestanden[2], toestanden[5]), trans.kans(blockersKetenS, toestanden[2], toestanden[6]),
                                                 trans.kans(blockersKetenS, toestanden[3], toestanden[1]), trans.kans(blockersKetenS, toestanden[3], toestanden[2]), trans.kans(blockersKetenS, toestanden[3], toestanden[3]), trans.kans(blockersKetenS, toestanden[3], toestanden[4]), trans.kans(blockersKetenS, toestanden[3], toestanden[5]), trans.kans(blockersKetenS, toestanden[3], toestanden[6]),
                                                 trans.kans(blockersKetenS, toestanden[4], toestanden[1]), trans.kans(blockersKetenS, toestanden[4], toestanden[2]), trans.kans(blockersKetenS, toestanden[4], toestanden[3]), trans.kans(blockersKetenS, toestanden[4], toestanden[4]), trans.kans(blockersKetenS, toestanden[4], toestanden[5]), trans.kans(blockersKetenS, toestanden[4], toestanden[6]),
                                                 trans.kans(blockersKetenS, toestanden[5], toestanden[1]), trans.kans(blockersKetenS, toestanden[5], toestanden[2]), trans.kans(blockersKetenS, toestanden[5], toestanden[3]), trans.kans(blockersKetenS, toestanden[5], toestanden[4]), trans.kans(blockersKetenS, toestanden[5], toestanden[5]), trans.kans(blockersKetenS, toestanden[5], toestanden[6]),
                                                 0, 0, 0, 0, 0, 1),
                                          byrow=TRUE, nrow=6, ncol = 6), name="JIRA")


mcCriticalL <- new("markovchain", states=toestanden,
                   transitionMatrix=matrix(data=c(0, 0.04000000, 0.70000000, 0.26000000, 0, 0,
                                                  0, 0, 0, 1, 0 ,0,
                                                  0.03333333, 0.03333333, 0, 0.93333333, 0, 0,
                                                  0, 0, 0, 0, 0.04081633, 0.95918367,
                                                  0, 0, 0, 1, 0, 0,
                                                  0, 0, 0, 0, 0, 1),
                                           byrow=TRUE, nrow=6, ncol = 6), name="JIRA")

mcCriticalS <- new("markovchain", states=toestanden,
                   transitionMatrix=matrix(data=c(trans.kans(criticalsKetenS, toestanden[1], toestanden[1]), trans.kans(criticalsKetenS, toestanden[1], toestanden[2]), trans.kans(criticalsKetenS, toestanden[1], toestanden[3]), trans.kans(criticalsKetenS, toestanden[1], toestanden[4]), trans.kans(criticalsKetenS, toestanden[1], toestanden[5]), trans.kans(criticalsKetenS, toestanden[1], toestanden[6]),
                                                  trans.kans(criticalsKetenS, toestanden[2], toestanden[1]), trans.kans(criticalsKetenS, toestanden[2], toestanden[2]), trans.kans(criticalsKetenS, toestanden[2], toestanden[3]), trans.kans(criticalsKetenS, toestanden[2], toestanden[4]), trans.kans(criticalsKetenS, toestanden[2], toestanden[5]), trans.kans(criticalsKetenS, toestanden[2], toestanden[6]),
                                                  trans.kans(criticalsKetenS, toestanden[3], toestanden[1]), trans.kans(criticalsKetenS, toestanden[3], toestanden[2]), trans.kans(criticalsKetenS, toestanden[3], toestanden[3]), trans.kans(criticalsKetenS, toestanden[3], toestanden[4]), trans.kans(criticalsKetenS, toestanden[3], toestanden[5]), trans.kans(criticalsKetenS, toestanden[3], toestanden[6]),
                                                  trans.kans(criticalsKetenS, toestanden[4], toestanden[1]), trans.kans(criticalsKetenS, toestanden[4], toestanden[2]), trans.kans(criticalsKetenS, toestanden[4], toestanden[3]), trans.kans(criticalsKetenS, toestanden[4], toestanden[4]), trans.kans(criticalsKetenS, toestanden[4], toestanden[5]), trans.kans(criticalsKetenS, toestanden[4], toestanden[6]),
                                                  trans.kans(criticalsKetenS, toestanden[5], toestanden[1]), trans.kans(criticalsKetenS, toestanden[5], toestanden[2]), trans.kans(criticalsKetenS, toestanden[5], toestanden[3]), trans.kans(criticalsKetenS, toestanden[5], toestanden[4]), trans.kans(criticalsKetenS, toestanden[5], toestanden[5]), trans.kans(criticalsKetenS, toestanden[5], toestanden[6]),
                                                  0, 0, 0, 0, 0, 1),
                                           byrow=TRUE, nrow=6, ncol = 6), name="JIRA")

mcMajorL <- new("markovchain", states=toestanden,
                transitionMatrix=matrix(data=c(0, 0.024291498, 0.598380567, 0.377327935, 0, 0,
                                               0.156250000, 0, 0.593750000, 0.250000000, 0 ,0,
                                               0.095170455, 0.004261364, 0, 0.900568182, 0, 0,
                                               0, 0, 0, 0, 0.033578174, 0.966421826,
                                               0, 0, 0.222222222, 0.777777778, 0, 0,
                                               0, 0, 0, 0, 0, 1),
                                        byrow=TRUE, nrow=6, ncol = 6), name="JIRA")

mcMajorS <- new("markovchain", states=toestanden,
                transitionMatrix=matrix(data=c(trans.kans(majorsKetenS, toestanden[1], toestanden[1]), trans.kans(majorsKetenS, toestanden[1], toestanden[2]), trans.kans(majorsKetenS, toestanden[1], toestanden[3]), trans.kans(majorsKetenS, toestanden[1], toestanden[4]), trans.kans(majorsKetenS, toestanden[1], toestanden[5]), trans.kans(majorsKetenS, toestanden[1], toestanden[6]),
                                               trans.kans(majorsKetenS, toestanden[2], toestanden[1]), trans.kans(majorsKetenS, toestanden[2], toestanden[2]), trans.kans(majorsKetenS, toestanden[2], toestanden[3]), trans.kans(majorsKetenS, toestanden[2], toestanden[4]), trans.kans(majorsKetenS, toestanden[2], toestanden[5]), trans.kans(majorsKetenS, toestanden[2], toestanden[6]),
                                               trans.kans(majorsKetenS, toestanden[3], toestanden[1]), trans.kans(majorsKetenS, toestanden[3], toestanden[2]), trans.kans(majorsKetenS, toestanden[3], toestanden[3]), trans.kans(majorsKetenS, toestanden[3], toestanden[4]), trans.kans(majorsKetenS, toestanden[3], toestanden[5]), trans.kans(majorsKetenS, toestanden[3], toestanden[6]),
                                               trans.kans(majorsKetenS, toestanden[4], toestanden[1]), trans.kans(majorsKetenS, toestanden[4], toestanden[2]), trans.kans(majorsKetenS, toestanden[4], toestanden[3]), trans.kans(majorsKetenS, toestanden[4], toestanden[4]), trans.kans(majorsKetenS, toestanden[4], toestanden[5]), trans.kans(majorsKetenS, toestanden[4], toestanden[6]),
                                               trans.kans(majorsKetenS, toestanden[5], toestanden[1]), trans.kans(majorsKetenS, toestanden[5], toestanden[2]), trans.kans(majorsKetenS, toestanden[5], toestanden[3]), trans.kans(majorsKetenS, toestanden[5], toestanden[4]), trans.kans(majorsKetenS, toestanden[5], toestanden[5]), trans.kans(majorsKetenS, toestanden[5], toestanden[6]),
                                               0, 0, 0, 0, 0, 1),
                                        byrow=TRUE, nrow=6, ncol = 6), name="JIRA")

mcMinorL <- new("markovchain", states=toestanden,
                transitionMatrix=matrix(data=c(trans.kans(minorsKetenL, toestanden[1], toestanden[1]), trans.kans(minorsKetenL, toestanden[1], toestanden[2]), trans.kans(minorsKetenL, toestanden[1], toestanden[3]), trans.kans(minorsKetenL, toestanden[1], toestanden[4]), trans.kans(minorsKetenL, toestanden[1], toestanden[5]), trans.kans(minorsKetenL, toestanden[1], toestanden[6]),
                                               0, 1, 0, 0, 0, 0, 
                                               trans.kans(minorsKetenL, toestanden[3], toestanden[1]), trans.kans(minorsKetenL, toestanden[3], toestanden[2]), trans.kans(minorsKetenL, toestanden[3], toestanden[3]), trans.kans(minorsKetenL, toestanden[3], toestanden[4]), trans.kans(minorsKetenL, toestanden[3], toestanden[5]), trans.kans(minorsKetenL, toestanden[3], toestanden[6]),
                                               trans.kans(minorsKetenL, toestanden[4], toestanden[1]), trans.kans(minorsKetenL, toestanden[4], toestanden[2]), trans.kans(minorsKetenL, toestanden[4], toestanden[3]), trans.kans(minorsKetenL, toestanden[4], toestanden[4]), trans.kans(minorsKetenL, toestanden[4], toestanden[5]), trans.kans(minorsKetenL, toestanden[4], toestanden[6]),
                                               trans.kans(minorsKetenL, toestanden[5], toestanden[1]), trans.kans(minorsKetenL, toestanden[5], toestanden[2]), trans.kans(minorsKetenL, toestanden[5], toestanden[3]), trans.kans(minorsKetenL, toestanden[5], toestanden[4]), trans.kans(minorsKetenL, toestanden[5], toestanden[5]), trans.kans(minorsKetenL, toestanden[5], toestanden[6]),
                                               0, 0, 0, 0, 0, 1),
                                        byrow=TRUE, nrow=6, ncol = 6), name="JIRA")

mcMinorS <- new("markovchain", states=toestanden,
                transitionMatrix=matrix(data=c(trans.kans(minorsKetenS, toestanden[1], toestanden[1]), trans.kans(minorsKetenS, toestanden[1], toestanden[2]), trans.kans(minorsKetenS, toestanden[1], toestanden[3]), trans.kans(minorsKetenS, toestanden[1], toestanden[4]), trans.kans(minorsKetenS, toestanden[1], toestanden[5]), trans.kans(minorsKetenS, toestanden[1], toestanden[6]),
                                               trans.kans(minorsKetenS, toestanden[2], toestanden[1]), trans.kans(minorsKetenS, toestanden[2], toestanden[2]), trans.kans(minorsKetenS, toestanden[2], toestanden[3]), trans.kans(minorsKetenS, toestanden[2], toestanden[4]), trans.kans(minorsKetenS, toestanden[2], toestanden[5]), trans.kans(minorsKetenS, toestanden[2], toestanden[6]),
                                               trans.kans(minorsKetenS, toestanden[3], toestanden[1]), trans.kans(minorsKetenS, toestanden[3], toestanden[2]), trans.kans(minorsKetenS, toestanden[3], toestanden[3]), trans.kans(minorsKetenS, toestanden[3], toestanden[4]), trans.kans(minorsKetenS, toestanden[3], toestanden[5]), trans.kans(minorsKetenS, toestanden[3], toestanden[6]),
                                               trans.kans(minorsKetenS, toestanden[4], toestanden[1]), trans.kans(minorsKetenS, toestanden[4], toestanden[2]), trans.kans(minorsKetenS, toestanden[4], toestanden[3]), trans.kans(minorsKetenS, toestanden[4], toestanden[4]), trans.kans(minorsKetenS, toestanden[4], toestanden[5]), trans.kans(minorsKetenS, toestanden[4], toestanden[6]),
                                               trans.kans(minorsKetenS, toestanden[5], toestanden[1]), trans.kans(minorsKetenS, toestanden[5], toestanden[2]), trans.kans(minorsKetenS, toestanden[5], toestanden[3]), trans.kans(minorsKetenS, toestanden[5], toestanden[4]), trans.kans(minorsKetenS, toestanden[5], toestanden[5]), trans.kans(minorsKetenS, toestanden[5], toestanden[6]),
                                               0, 0, 0, 0, 0, 1),
                                        byrow=TRUE, nrow=6, ncol = 6), name="JIRA")

mcTrivialL <- new("markovchain", states=toestanden,
                  transitionMatrix=matrix(data=c(0, 0.01724138, 0.58620690, 0.39655172, 0, 0,
                                                 0, 0, 1, 0, 0 ,0,
                                                 0.10810811, 0.08108108, 0, 0.81081081, 0, 0,
                                                 0, 0, 0, 0, 0, 1,
                                                 0, 0, 0, 0, 1, 0,
                                                 0, 0, 0, 0, 0, 1),
                                          byrow=TRUE, nrow=6, ncol = 6), name="JIRA")

mcTrivialS <- new("markovchain", states=toestanden,
                  transitionMatrix=matrix(data=c(trans.kans(trivialsKetenS, toestanden[1], toestanden[1]), trans.kans(trivialsKetenS, toestanden[1], toestanden[2]), trans.kans(trivialsKetenS, toestanden[1], toestanden[3]), trans.kans(trivialsKetenS, toestanden[1], toestanden[4]), trans.kans(trivialsKetenS, toestanden[1], toestanden[5]), trans.kans(trivialsKetenS, toestanden[1], toestanden[6]),
                                                 trans.kans(trivialsKetenS, toestanden[2], toestanden[1]), trans.kans(trivialsKetenS, toestanden[2], toestanden[2]), trans.kans(trivialsKetenS, toestanden[2], toestanden[3]), trans.kans(trivialsKetenS, toestanden[2], toestanden[4]), trans.kans(trivialsKetenS, toestanden[2], toestanden[5]), trans.kans(trivialsKetenS, toestanden[2], toestanden[6]),
                                                 trans.kans(trivialsKetenS, toestanden[3], toestanden[1]), trans.kans(trivialsKetenS, toestanden[3], toestanden[2]), trans.kans(trivialsKetenS, toestanden[3], toestanden[3]), trans.kans(trivialsKetenS, toestanden[3], toestanden[4]), trans.kans(trivialsKetenS, toestanden[3], toestanden[5]), trans.kans(trivialsKetenS, toestanden[3], toestanden[6]),
                                                 trans.kans(trivialsKetenS, toestanden[4], toestanden[1]), trans.kans(trivialsKetenS, toestanden[4], toestanden[2]), trans.kans(trivialsKetenS, toestanden[4], toestanden[3]), trans.kans(trivialsKetenS, toestanden[4], toestanden[4]), trans.kans(trivialsKetenS, toestanden[4], toestanden[5]), trans.kans(trivialsKetenS, toestanden[4], toestanden[6]),
                                                 0, 0, 0, 0, 1, 0,
                                                 0, 0, 0, 0, 0, 1),
                                          byrow=TRUE, nrow=6, ncol = 6), name="JIRA")


transitie2 = as.data.frame(transitie2)
levels(transitie2$Var1) <- toestanden
transitie2[nrow(transitie2) + 1,] = c("Closed","Closed",1)
transitie3 = as.matrix(transitie2$Freq, nrow = 6, dimnames = list(transitie2$Var1, transitie2$Var2))
transitie2

## outliers bepalen en transitie tijden bepalen nadat deze zijn weggehaald

transtijd = function(tabel, from, to){
  t1 = tabel %>%
    filter(from_status == from & to_status == to)
  
  lowerq = quantile(t1$days_in_from_status)[2]
  upperq = quantile(t1$days_in_from_status)[4]
  iqr = upperq - lowerq
  mild.threshold.upper = (iqr * 1.5) + upperq
  extreme.threshold.upper = (iqr * 3) + upperq
  
  t2 = tabel %>%
    filter(from_status == from & to_status == to & days_in_from_status < extreme.threshold.upper)
  
  if(is.na(mean(t2$days_in_from_status))==T){
    return(0)
  }else{
    return(mean(t2$days_in_from_status))
  }
}

##example don't run



blockersSTimes = matrix(data = c(transtijd(blockersS, toestanden[1], toestanden[1]), transtijd(blockersS, toestanden[1], toestanden[2]), transtijd(blockersS, toestanden[1], toestanden[3]), transtijd(blockersS, toestanden[1], toestanden[4]), transtijd(blockersS, toestanden[1], toestanden[5]), transtijd(blockersS, toestanden[1], toestanden[6]),
                                 transtijd(blockersS, toestanden[2], toestanden[1]), transtijd(blockersS, toestanden[2], toestanden[2]), transtijd(blockersS, toestanden[2], toestanden[3]), transtijd(blockersS, toestanden[2], toestanden[4]), transtijd(blockersS, toestanden[2], toestanden[5]), transtijd(blockersS, toestanden[2], toestanden[6]),
                                 transtijd(blockersS, toestanden[3], toestanden[1]), transtijd(blockersS, toestanden[3], toestanden[2]), transtijd(blockersS, toestanden[3], toestanden[3]), transtijd(blockersS, toestanden[3], toestanden[4]), transtijd(blockersS, toestanden[3], toestanden[5]), transtijd(blockersS, toestanden[3], toestanden[6]),
                                 transtijd(blockersS, toestanden[4], toestanden[1]), transtijd(blockersS, toestanden[4], toestanden[2]), transtijd(blockersS, toestanden[4], toestanden[3]), transtijd(blockersS, toestanden[4], toestanden[4]), transtijd(blockersS, toestanden[4], toestanden[5]), transtijd(blockersS, toestanden[4], toestanden[6]),
                                 transtijd(blockersS, toestanden[5], toestanden[1]), transtijd(blockersS, toestanden[5], toestanden[2]), transtijd(blockersS, toestanden[5], toestanden[3]), transtijd(blockersS, toestanden[5], toestanden[4]), transtijd(blockersS, toestanden[5], toestanden[5]), transtijd(blockersS, toestanden[5], toestanden[6]),
                                 transtijd(blockersS, toestanden[6], toestanden[1]), transtijd(blockersS, toestanden[6], toestanden[2]), transtijd(blockersS, toestanden[6], toestanden[3]), transtijd(blockersS, toestanden[6], toestanden[4]), transtijd(blockersS, toestanden[6], toestanden[5]), transtijd(blockersS, toestanden[6], toestanden[6])), byrow = T, nrow = 6, ncol = 6, dimnames = list(toestanden, toestanden))

criticalsLTimes = matrix(data = c(transtijd(criticalsL, toestanden[1], toestanden[1]), transtijd(criticalsL, toestanden[1], toestanden[2]), transtijd(criticalsL, toestanden[1], toestanden[3]), transtijd(criticalsL, toestanden[1], toestanden[4]), transtijd(criticalsL, toestanden[1], toestanden[5]), transtijd(criticalsL, toestanden[1], toestanden[6]),
                                  transtijd(criticalsL, toestanden[2], toestanden[1]), transtijd(criticalsL, toestanden[2], toestanden[2]), transtijd(criticalsL, toestanden[2], toestanden[3]), transtijd(criticalsL, toestanden[2], toestanden[4]), transtijd(criticalsL, toestanden[2], toestanden[5]), transtijd(criticalsL, toestanden[2], toestanden[6]),
                                  transtijd(criticalsL, toestanden[3], toestanden[1]), transtijd(criticalsL, toestanden[3], toestanden[2]), transtijd(criticalsL, toestanden[3], toestanden[3]), transtijd(criticalsL, toestanden[3], toestanden[4]), transtijd(criticalsL, toestanden[3], toestanden[5]), transtijd(criticalsL, toestanden[3], toestanden[6]),
                                  transtijd(criticalsL, toestanden[4], toestanden[1]), transtijd(criticalsL, toestanden[4], toestanden[2]), transtijd(criticalsL, toestanden[4], toestanden[3]), transtijd(criticalsL, toestanden[4], toestanden[4]), transtijd(criticalsL, toestanden[4], toestanden[5]), transtijd(criticalsL, toestanden[4], toestanden[6]),
                                  transtijd(criticalsL, toestanden[5], toestanden[1]), transtijd(criticalsL, toestanden[5], toestanden[2]), transtijd(criticalsL, toestanden[5], toestanden[3]), transtijd(criticalsL, toestanden[5], toestanden[4]), transtijd(criticalsL, toestanden[5], toestanden[5]), transtijd(criticalsL, toestanden[5], toestanden[6]),
                                  transtijd(criticalsL, toestanden[6], toestanden[1]), transtijd(criticalsL, toestanden[6], toestanden[2]), transtijd(criticalsL, toestanden[6], toestanden[3]), transtijd(criticalsL, toestanden[6], toestanden[4]), transtijd(criticalsL, toestanden[6], toestanden[5]), transtijd(criticalsL, toestanden[6], toestanden[6])), byrow = T, nrow = 6, ncol = 6, dimnames = list(toestanden, toestanden))

criticalsSTimes = matrix(data = c(transtijd(criticalsS, toestanden[1], toestanden[1]), transtijd(criticalsS, toestanden[1], toestanden[2]), transtijd(criticalsS, toestanden[1], toestanden[3]), transtijd(criticalsS, toestanden[1], toestanden[4]), transtijd(criticalsS, toestanden[1], toestanden[5]), transtijd(criticalsS, toestanden[1], toestanden[6]),
                                  transtijd(criticalsS, toestanden[2], toestanden[1]), transtijd(criticalsS, toestanden[2], toestanden[2]), transtijd(criticalsS, toestanden[2], toestanden[3]), transtijd(criticalsS, toestanden[2], toestanden[4]), transtijd(criticalsS, toestanden[2], toestanden[5]), transtijd(criticalsS, toestanden[2], toestanden[6]),
                                  transtijd(criticalsS, toestanden[3], toestanden[1]), transtijd(criticalsS, toestanden[3], toestanden[2]), transtijd(criticalsS, toestanden[3], toestanden[3]), transtijd(criticalsS, toestanden[3], toestanden[4]), transtijd(criticalsS, toestanden[3], toestanden[5]), transtijd(criticalsS, toestanden[3], toestanden[6]),
                                  transtijd(criticalsS, toestanden[4], toestanden[1]), transtijd(criticalsS, toestanden[4], toestanden[2]), transtijd(criticalsS, toestanden[4], toestanden[3]), transtijd(criticalsS, toestanden[4], toestanden[4]), transtijd(criticalsS, toestanden[4], toestanden[5]), transtijd(criticalsS, toestanden[4], toestanden[6]),
                                  transtijd(criticalsS, toestanden[5], toestanden[1]), transtijd(criticalsS, toestanden[5], toestanden[2]), transtijd(criticalsS, toestanden[5], toestanden[3]), transtijd(criticalsS, toestanden[5], toestanden[4]), transtijd(criticalsS, toestanden[5], toestanden[5]), transtijd(criticalsS, toestanden[5], toestanden[6]),
                                  transtijd(criticalsS, toestanden[6], toestanden[1]), transtijd(criticalsS, toestanden[6], toestanden[2]), transtijd(criticalsS, toestanden[6], toestanden[3]), transtijd(criticalsS, toestanden[6], toestanden[4]), transtijd(criticalsS, toestanden[6], toestanden[5]), transtijd(criticalsS, toestanden[6], toestanden[6])), byrow = T, nrow = 6, ncol = 6, dimnames = list(toestanden, toestanden))

majorsLTimes = matrix(data = c(transtijd(majorsL, toestanden[1], toestanden[1]), transtijd(majorsL, toestanden[1], toestanden[2]), transtijd(majorsL, toestanden[1], toestanden[3]), transtijd(majorsL, toestanden[1], toestanden[4]), transtijd(majorsL, toestanden[1], toestanden[5]), transtijd(majorsL, toestanden[1], toestanden[6]),
                               transtijd(majorsL, toestanden[2], toestanden[1]), transtijd(majorsL, toestanden[2], toestanden[2]), transtijd(majorsL, toestanden[2], toestanden[3]), transtijd(majorsL, toestanden[2], toestanden[4]), transtijd(majorsL, toestanden[2], toestanden[5]), transtijd(majorsL, toestanden[2], toestanden[6]),
                               transtijd(majorsL, toestanden[3], toestanden[1]), transtijd(majorsL, toestanden[3], toestanden[2]), transtijd(majorsL, toestanden[3], toestanden[3]), transtijd(majorsL, toestanden[3], toestanden[4]), transtijd(majorsL, toestanden[3], toestanden[5]), transtijd(majorsL, toestanden[3], toestanden[6]),
                               transtijd(majorsL, toestanden[4], toestanden[1]), transtijd(majorsL, toestanden[4], toestanden[2]), transtijd(majorsL, toestanden[4], toestanden[3]), transtijd(majorsL, toestanden[4], toestanden[4]), transtijd(majorsL, toestanden[4], toestanden[5]), transtijd(majorsL, toestanden[4], toestanden[6]),
                               transtijd(majorsL, toestanden[5], toestanden[1]), transtijd(majorsL, toestanden[5], toestanden[2]), transtijd(majorsL, toestanden[5], toestanden[3]), transtijd(majorsL, toestanden[5], toestanden[4]), transtijd(majorsL, toestanden[5], toestanden[5]), transtijd(majorsL, toestanden[5], toestanden[6]),
                               transtijd(majorsL, toestanden[6], toestanden[1]), transtijd(majorsL, toestanden[6], toestanden[2]), transtijd(majorsL, toestanden[6], toestanden[3]), transtijd(majorsL, toestanden[6], toestanden[4]), transtijd(majorsL, toestanden[6], toestanden[5]), transtijd(majorsL, toestanden[6], toestanden[6])), byrow = T, nrow = 6, ncol = 6, dimnames = list(toestanden, toestanden))

majorsSTimes = matrix(data = c(transtijd(majorsS, toestanden[1], toestanden[1]), transtijd(majorsS, toestanden[1], toestanden[2]), transtijd(majorsS, toestanden[1], toestanden[3]), transtijd(majorsS, toestanden[1], toestanden[4]), transtijd(majorsS, toestanden[1], toestanden[5]), transtijd(majorsS, toestanden[1], toestanden[6]),
                               transtijd(majorsS, toestanden[2], toestanden[1]), transtijd(majorsS, toestanden[2], toestanden[2]), transtijd(majorsS, toestanden[2], toestanden[3]), transtijd(majorsS, toestanden[2], toestanden[4]), transtijd(majorsS, toestanden[2], toestanden[5]), transtijd(majorsS, toestanden[2], toestanden[6]),
                               transtijd(majorsS, toestanden[3], toestanden[1]), transtijd(majorsS, toestanden[3], toestanden[2]), transtijd(majorsS, toestanden[3], toestanden[3]), transtijd(majorsS, toestanden[3], toestanden[4]), transtijd(majorsS, toestanden[3], toestanden[5]), transtijd(majorsS, toestanden[3], toestanden[6]),
                               transtijd(majorsS, toestanden[4], toestanden[1]), transtijd(majorsS, toestanden[4], toestanden[2]), transtijd(majorsS, toestanden[4], toestanden[3]), transtijd(majorsS, toestanden[4], toestanden[4]), transtijd(majorsS, toestanden[4], toestanden[5]), transtijd(majorsS, toestanden[4], toestanden[6]),
                               transtijd(majorsS, toestanden[5], toestanden[1]), transtijd(majorsS, toestanden[5], toestanden[2]), transtijd(majorsS, toestanden[5], toestanden[3]), transtijd(majorsS, toestanden[5], toestanden[4]), transtijd(majorsS, toestanden[5], toestanden[5]), transtijd(majorsS, toestanden[5], toestanden[6]),
                               transtijd(majorsS, toestanden[6], toestanden[1]), transtijd(majorsS, toestanden[6], toestanden[2]), transtijd(majorsS, toestanden[6], toestanden[3]), transtijd(majorsS, toestanden[6], toestanden[4]), transtijd(majorsS, toestanden[6], toestanden[5]), transtijd(majorsS, toestanden[6], toestanden[6])), byrow = T, nrow = 6, ncol = 6, dimnames = list(toestanden, toestanden))

minorsLTimes = matrix(data = c(transtijd(minorsL, toestanden[1], toestanden[1]), transtijd(minorsL, toestanden[1], toestanden[2]), transtijd(minorsL, toestanden[1], toestanden[3]), transtijd(minorsL, toestanden[1], toestanden[4]), transtijd(minorsL, toestanden[1], toestanden[5]), transtijd(minorsL, toestanden[1], toestanden[6]),
                               transtijd(minorsL, toestanden[2], toestanden[1]), transtijd(minorsL, toestanden[2], toestanden[2]), transtijd(minorsL, toestanden[2], toestanden[3]), transtijd(minorsL, toestanden[2], toestanden[4]), transtijd(minorsL, toestanden[2], toestanden[5]), transtijd(minorsL, toestanden[2], toestanden[6]),
                               transtijd(minorsL, toestanden[3], toestanden[1]), transtijd(minorsL, toestanden[3], toestanden[2]), transtijd(minorsL, toestanden[3], toestanden[3]), transtijd(minorsL, toestanden[3], toestanden[4]), transtijd(minorsL, toestanden[3], toestanden[5]), transtijd(minorsL, toestanden[3], toestanden[6]),
                               transtijd(minorsL, toestanden[4], toestanden[1]), transtijd(minorsL, toestanden[4], toestanden[2]), transtijd(minorsL, toestanden[4], toestanden[3]), transtijd(minorsL, toestanden[4], toestanden[4]), transtijd(minorsL, toestanden[4], toestanden[5]), transtijd(minorsL, toestanden[4], toestanden[6]),
                               transtijd(minorsL, toestanden[5], toestanden[1]), transtijd(minorsL, toestanden[5], toestanden[2]), transtijd(minorsL, toestanden[5], toestanden[3]), transtijd(minorsL, toestanden[5], toestanden[4]), transtijd(minorsL, toestanden[5], toestanden[5]), transtijd(minorsL, toestanden[5], toestanden[6]),
                               transtijd(minorsL, toestanden[6], toestanden[1]), transtijd(minorsL, toestanden[6], toestanden[2]), transtijd(minorsL, toestanden[6], toestanden[3]), transtijd(minorsL, toestanden[6], toestanden[4]), transtijd(minorsL, toestanden[6], toestanden[5]), transtijd(minorsL, toestanden[6], toestanden[6])), byrow = T, nrow = 6, ncol = 6, dimnames = list(toestanden, toestanden))

minorsSTimes = matrix(data = c(transtijd(minorsS, toestanden[1], toestanden[1]), transtijd(minorsS, toestanden[1], toestanden[2]), transtijd(minorsS, toestanden[1], toestanden[3]), transtijd(minorsS, toestanden[1], toestanden[4]), transtijd(minorsS, toestanden[1], toestanden[5]), transtijd(minorsS, toestanden[1], toestanden[6]),
                               transtijd(minorsS, toestanden[2], toestanden[1]), transtijd(minorsS, toestanden[2], toestanden[2]), transtijd(minorsS, toestanden[2], toestanden[3]), transtijd(minorsS, toestanden[2], toestanden[4]), transtijd(minorsS, toestanden[2], toestanden[5]), transtijd(minorsS, toestanden[2], toestanden[6]),
                               transtijd(minorsS, toestanden[3], toestanden[1]), transtijd(minorsS, toestanden[3], toestanden[2]), transtijd(minorsS, toestanden[3], toestanden[3]), transtijd(minorsS, toestanden[3], toestanden[4]), transtijd(minorsS, toestanden[3], toestanden[5]), transtijd(minorsS, toestanden[3], toestanden[6]),
                               transtijd(minorsS, toestanden[4], toestanden[1]), transtijd(minorsS, toestanden[4], toestanden[2]), transtijd(minorsS, toestanden[4], toestanden[3]), transtijd(minorsS, toestanden[4], toestanden[4]), transtijd(minorsS, toestanden[4], toestanden[5]), transtijd(minorsS, toestanden[4], toestanden[6]),
                               transtijd(minorsS, toestanden[5], toestanden[1]), transtijd(minorsS, toestanden[5], toestanden[2]), transtijd(minorsS, toestanden[5], toestanden[3]), transtijd(minorsS, toestanden[5], toestanden[4]), transtijd(minorsS, toestanden[5], toestanden[5]), transtijd(minorsS, toestanden[5], toestanden[6]),
                               transtijd(minorsS, toestanden[6], toestanden[1]), transtijd(minorsS, toestanden[6], toestanden[2]), transtijd(minorsS, toestanden[6], toestanden[3]), transtijd(minorsS, toestanden[6], toestanden[4]), transtijd(minorsS, toestanden[6], toestanden[5]), transtijd(minorsS, toestanden[6], toestanden[6])), byrow = T, nrow = 6, ncol = 6, dimnames = list(toestanden, toestanden))


trivialsLTimes = matrix(data = c(transtijd(trivialsL, toestanden[1], toestanden[1]), transtijd(trivialsL, toestanden[1], toestanden[2]), transtijd(trivialsL, toestanden[1], toestanden[3]), transtijd(trivialsL, toestanden[1], toestanden[4]), transtijd(trivialsL, toestanden[1], toestanden[5]), transtijd(trivialsL, toestanden[1], toestanden[6]),
                                 transtijd(trivialsL, toestanden[2], toestanden[1]), transtijd(trivialsL, toestanden[2], toestanden[2]), transtijd(trivialsL, toestanden[2], toestanden[3]), transtijd(trivialsL, toestanden[2], toestanden[4]), transtijd(trivialsL, toestanden[2], toestanden[5]), transtijd(trivialsL, toestanden[2], toestanden[6]),
                                 transtijd(trivialsL, toestanden[3], toestanden[1]), transtijd(trivialsL, toestanden[3], toestanden[2]), transtijd(trivialsL, toestanden[3], toestanden[3]), transtijd(trivialsL, toestanden[3], toestanden[4]), transtijd(trivialsL, toestanden[3], toestanden[5]), transtijd(trivialsL, toestanden[3], toestanden[6]),
                                 transtijd(trivialsL, toestanden[4], toestanden[1]), transtijd(trivialsL, toestanden[4], toestanden[2]), transtijd(trivialsL, toestanden[4], toestanden[3]), transtijd(trivialsL, toestanden[4], toestanden[4]), transtijd(trivialsL, toestanden[4], toestanden[5]), transtijd(trivialsL, toestanden[4], toestanden[6]),
                                 transtijd(trivialsL, toestanden[5], toestanden[1]), transtijd(trivialsL, toestanden[5], toestanden[2]), transtijd(trivialsL, toestanden[5], toestanden[3]), transtijd(trivialsL, toestanden[5], toestanden[4]), transtijd(trivialsL, toestanden[5], toestanden[5]), transtijd(trivialsL, toestanden[5], toestanden[6]),
                                 transtijd(trivialsL, toestanden[6], toestanden[1]), transtijd(trivialsL, toestanden[6], toestanden[2]), transtijd(trivialsL, toestanden[6], toestanden[3]), transtijd(trivialsL, toestanden[6], toestanden[4]), transtijd(trivialsL, toestanden[6], toestanden[5]), transtijd(trivialsL, toestanden[6], toestanden[6])), byrow = T, nrow = 6, ncol = 6, dimnames = list(toestanden, toestanden))


a = markovchainSequence(timesteps, mcJIRA, "Open", include.t0 = T)

b = numeric(length = length(a))

lowerq = quantile(data)[2]
upperq = quantile(data)[4]
mild.threshold.upper = (iqr * 1.5) + upperq


for(i in 0:(length(a)-2) ){
  
  b[i+1] = transitionTimes[a[1+i],a[2+i]]
  
  if( a[2+i] == "Resolved"){
    break
  }
}

rm(b)

sum(d)


aa = avro_issues %>%
  filter(key == "AVRO-2171") %>%
  select(created, priority, status)

aa[1,3]
markovchainSequence(timesteps, mcJIRA, aa$status, include.t0 = T) 

strptime(aa$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(d) *3600 *24)


resolvedPrediction2 <- function(
  now = "2018-02-01"
){ 
  aa = subset(avro_transitions, c(( avro_transitions$resolutiondate >= "2013-01-01" | is.na(avro_transitions$resolutiondate) == T ) & "2013-01-01" >= avro_transitions$created & avro_transitions$when <= "2013-01-01"))
  
  lijst <- data.frame(
    issue = unique(aa$key),
    predicted_resolution_date = as.POSIXct(character(length = length(unique(aa$key))), origin = "1970-01-01", format = "%Y-%m-%dT%H:%M:%OS%z")
  )
  
  for(i in 1:length(lijst[[1]])){
    
    a2 = aa %>%
      filter(key == lijst[[1]][i]) %>%
      select(status, created, priority, when, issue_type, from_status, to_status, key)
    
    a3 = a2 %>%
      filter(max(when) == when)
    
    if(is.na(a3$from_status) == T){
      next
    }
    
    if(a3$priority == prioriteit[1]){
      
      a = markovchainSequence(20, mcBlocker, a3$from_status, include.t0 = T)
      
      b = numeric(length = length(a))
      
      for(j in 0:(length(a)-2) ){
        
        b[j+1] = blockerTimes[a[1+j],a[2+j]]
        
        if( a[1+j] == "Resolved" & a[2+j] == "Closed"){
          break
        }
      }
      
      lijst[[2]][i] = strptime(a3$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
      
    } else if (a3$priority == prioriteit[2]){
      
      a = markovchainSequence(20, mcMajor, a3$from_status, include.t0 = T)
      
      b = numeric(length = length(a))
      
      for(j in 0:(length(a)-2) ){
        
        b[j+1] = majorTimes[a[1+j],a[2+j]]
        
        if( a[1+j] == "Resolved" & a[2+j] == "Closed"){
          break
        }
      }
      
      lijst[[2]][i] = strptime(a3$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
      
      
    } else if (a3$priority == prioriteit[3]){
      
      a = markovchainSequence(20, mcMajor, a3$from_status, include.t0 = T)
      
      b = numeric(length = length(a))
      
      for(j in 0:(length(a)-2) ){
        
        b[j+1] = majorTimes[a[1+j],a[2+j]]
        
        if( a[1+j] == "Resolved" & a[2+j] == "Closed"){
          break
        }
      }
      
      lijst[[2]][i] = strptime(a3$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
      
      
    } else if (a3$priority == prioriteit[4]){
      
      a = markovchainSequence(20, mcMajor, a3$from_status, include.t0 = T)
      
      b = numeric(length = length(a))
      
      for(j in 0:(length(a)-2) ){
        
        b[j+1] = majorTimes[a[1+j],a[2+j]]
        
        if( a[1+j] == "Resolved" & a[2+j] == "Closed"){
          break
        }
      }
      
      lijst[[2]][i] = strptime(a3$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
      
      
    } else if (a3$priority == prioriteit[5]){
      
      a = markovchainSequence(20, mcMajor, a3$from_status, include.t0 = T)
      
      b = numeric(length = length(a))
      
      for(j in 0:(length(a)-2) ){
        
        b[j+1] = majorTimes[a[1+j],a[2+j]]
        
        if( a[1+j] == "Resolved" & a[2+j] == "Closed"){
          break
        }
      }
      
      lijst[[2]][i] = strptime(a3$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
      
      
    }
    
  }
  
  return(lijst[complete.cases(lijst),])
  
}


# API draaien, NIET PORT 8787 instellen
gdd <- plumb("resolvedate.R")
gdd$run(host='0.0.0.0', port=8000)

#sites
#http://127.0.0.1:8080/api/issue/AVRO-2101/resolve-prediction
#http://127.0.0.1:8080/api/release/2018-01-01/resolved-since-now
#plumb(commandArgs()[4]); gdd$run(host='0.0.0.0', port=8000)
