library(dplyr)
library(markovchain)
library(lubridate)

load("DTMCJIRA.RData")
load("avroissues.RData")
load("transitionTimes.RData")

#* @get /api/issue/<issue>/resolve-prediction
resolvedPrediction <- function(
  issue # = 'AVRO-9999',
 # predicted_resolution_date = strftime(as.POSIXlt("1970-01-01 0:0:0", format = "%Y-%m-%d %H:%M:%S"), "%Y-%m-%dT%H:%M:%S%z")
){lijst <- list(
  issue=issue,
  predicted_resolution_date = character(length = 1))

if(issue %in% avro_issues$key == F) {
  return(404)
}
  
aa = subset(avro_issues, avro_issues$key == issue)

if(is.na(aa$resolutiondate) == F){
  lijst[[2]] = aa$resolutiondate
  return(lijst)
}

if(aa$priority == prioriteit[1] & aa$issue_type %in% long){
  a = markovchainSequence(20, mcBlockerL, aa$status, include.t0 = T)
  
  b = numeric(length = length(a))
  
  for(i in 0:(length(a)-2) ){
    
    b[i+1] = blockerTimesL[a[1+i],a[2+i]]
    
    if( a[1+i] == "Resolved" & a[2+i] == "Closed" | a[1+i] == "Closed" & a[2+i] == "Closed"){
      break
    }
  }
  
  lijst[[2]] = strptime(aa$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
  
  return(lijst)
} else if (aa$priority == prioriteit[1] & aa$issue_type %in% short) {
  a = markovchainSequence(20, mcCriticalS, aa$status, include.t0 = T)
  
  b = numeric(length = length(a))
  
  for(i in 0:(length(a)-2) ){
    
    b[i+1] = criticalTimesS[a[1+i],a[2+i]]
    
    if( a[1+i] == "Resolved" & a[2+i] == "Closed" | a[1+i] == "Closed" & a[2+i] == "Closed" ){
      break
    }
  }
  
  lijst[[2]] = strptime(aa$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
  

  
  return(lijst)
} else if (aa$priority == prioriteit[2] & aa$issue_type %in% long) {
  a = markovchainSequence(20, mcCriticalL, aa$status, include.t0 = T)
  
  b = numeric(length = length(a))
  
  for(i in 0:(length(a)-2) ){
    
    b[i+1] = criticalTimesL[a[1+i],a[2+i]]
    
    if( a[1+i] == "Resolved" & a[2+i] == "Closed" | a[1+i] == "Closed" & a[2+i] == "Closed" ){
      break
    }
  }
  
  lijst[[2]] = strptime(aa$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
  
  
  
  return(lijst)
} else if (aa$priority == prioriteit[2] & aa$issue_type %in% short) {
  a = markovchainSequence(20, mcCriticalS, aa$status, include.t0 = T)
  
  b = numeric(length = length(a))
  
  for(i in 0:(length(a)-2) ){
    
    b[i+1] = criticalTimesS[a[1+i],a[2+i]]
    
    if( a[1+i] == "Resolved" & a[2+i] == "Closed" | a[1+i] == "Closed" & a[2+i] == "Closed" ){
      break
    }
  }
  
  lijst[[2]] = strptime(aa$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
  
  
  
  return(lijst)
} else if (aa$priority == prioriteit[3] & aa$issue_type %in% long) {
  a = markovchainSequence(20, mcMajorL, aa$status, include.t0 = T)
  
  b = numeric(length = length(a))
  
  for(i in 0:(length(a)-2) ){
    
    b[i+1] = majorTimesL[a[1+i],a[2+i]]
    
    if( a[1+i] == "Resolved" & a[2+i] == "Closed" | a[1+i] == "Closed" & a[2+i] == "Closed"){
      break
    }
  }
  
  lijst[[2]] = strptime(aa$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
  
  return(lijst)
 } else if (aa$priority == prioriteit[3] & aa$issue_type %in% short) {
   a = markovchainSequence(20, mcMajorS, aa$status, include.t0 = T)
   
   b = numeric(length = length(a))
   
   for(i in 0:(length(a)-2) ){
     
     b[i+1] = majorTimesS[a[1+i],a[2+i]]
     
     if( a[1+i] == "Resolved" & a[2+i] == "Closed" | a[1+i] == "Closed" & a[2+i] == "Closed"){
       break
     }
   }
   
   lijst[[2]] = strptime(aa$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
   
   return(lijst)
 } else if (aa$priority == prioriteit[4] & aa$issue_type %in% long) {
  a = markovchainSequence(20, mcMinorL, aa$status, include.t0 = T)
  
  b = numeric(length = length(a))
  
  for(i in 0:(length(a)-2) ){
    
    b[i+1] = minorTimesL[a[1+i],a[2+i]]
    
    if( a[1+i] == "Resolved" & a[2+i] == "Closed" | a[1+i] == "Closed" & a[2+i] == "Closed"){
      break
    }
  }
  
  lijst[[2]] = strptime(aa$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
  
  return(lijst)
} else if (aa$priority == prioriteit[4] & aa$issue_type %in% short) {
  a = markovchainSequence(20, mcMinorS, aa$status, include.t0 = T)
  
  b = numeric(length = length(a))
  
  for(i in 0:(length(a)-2) ){
    
    b[i+1] = minorTimesS[a[1+i],a[2+i]]
    
    if( a[1+i] == "Resolved" & a[2+i] == "Closed" | a[1+i] == "Closed" & a[2+i] == "Closed"){
      break
    }
  }
  
  lijst[[2]] = strptime(aa$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
  
  return(lijst)
} else if (aa$priority == prioriteit[5] & aa$issue_type %in% long) {
  a = markovchainSequence(20, mcTrivialL, aa$status, include.t0 = T)
  
  b = numeric(length = length(a))
  
  for(i in 0:(length(a)-2) ){
    
    b[i+1] = trivialTimesL[a[1+i],a[2+i]]
    
    if( a[1+i] == "Resolved" & a[2+i] == "Closed" | a[1+i] == "Closed" & a[2+i] == "Closed"){
      break
    }
  }
  
  lijst[[2]] = strptime(aa$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
  
  return(lijst)
} else if (aa$priority == prioriteit[5] & aa$issue_type %in% short) {
  a = markovchainSequence(20, mcTrivialS, aa$status, include.t0 = T)
  
  b = numeric(length = length(a))
  
  for(i in 0:(length(a)-2) ){
    
    b[i+1] = trivialTimesS[a[1+i],a[2+i]]
    
    if( a[1+i] == "Resolved" & a[2+i] == "Closed" | a[1+i] == "Closed" & a[2+i] == "Closed"){
      break
    }
  }
  
  lijst[[2]] = strptime(aa$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
  
  return(lijst)
  }

}


#* @get /api/release/<now>/resolved-since-now
resolvedPrediction2 <- function(
  now
){ 
  aa = subset(avro_transitions, c(( avro_transitions$resolutiondate >= now | is.na(avro_transitions$resolutiondate) == T ) & now >= avro_transitions$created & avro_transitions$when <= now))
  
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
    
    if(is.na(a3$to_status) == T){
      next
    }
    
    if(a3$priority == prioriteit[1] & a3$issue_type %in% long){
      
      a = markovchainSequence(20, mcBlockerL, a3$to_status, include.t0 = T)
      
      b = numeric(length = length(a))
      
      for(j in 0:(length(a)-2) ){
        
        b[j+1] = blockerTimesL[a[1+j],a[2+j]]
        
        if( a[1+j] == "Resolved" & a[2+j] == "Closed"){
          break
        }
      }
      
      lijst[[2]][i] = strptime(a3$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
      
    } else if (a3$priority == prioriteit[1] & a3$issue_type %in% short){
      
      a = markovchainSequence(20, mcMajorS, a3$to_status, include.t0 = T)
      
      b = numeric(length = length(a))
      
      for(j in 0:(length(a)-2) ){
        
        b[j+1] = majorTimesS[a[1+j],a[2+j]]
        
        if( a[1+j] == "Resolved" & a[2+j] == "Closed"){
          break
        }
      }
      
      lijst[[2]][i] = strptime(a3$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
      
      
    } else if (a3$priority == prioriteit[2] & a3$issue_type %in% long){
      
      a = markovchainSequence(20, mcMajorL, a3$to_status, include.t0 = T)
      
      b = numeric(length = length(a))
      
      for(j in 0:(length(a)-2) ){
        
        b[j+1] = majorTimesL[a[1+j],a[2+j]]
        
        if( a[1+j] == "Resolved" & a[2+j] == "Closed"){
          break
        }
      }
      
      lijst[[2]][i] = strptime(a3$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
      
      
    } else if (a3$priority == prioriteit[2] & a3$issue_type %in% short){
      
      a = markovchainSequence(20, mcMajorS, a3$to_status, include.t0 = T)
      
      b = numeric(length = length(a))
      
      for(j in 0:(length(a)-2) ){
        
        b[j+1] = majorTimesS[a[1+j],a[2+j]]
        
        if( a[1+j] == "Resolved" & a[2+j] == "Closed"){
          break
        }
      }
      
      lijst[[2]][i] = strptime(a3$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
      
      
    } else if (a3$priority == prioriteit[3] & a3$issue_type %in% long){
      
      a = markovchainSequence(20, mcMajorL, a3$to_status, include.t0 = T)
      
      b = numeric(length = length(a))
      
      for(j in 0:(length(a)-2) ){
        
        b[j+1] = majorTimesL[a[1+j],a[2+j]]
        
        if( a[1+j] == "Resolved" & a[2+j] == "Closed"){
          break
        }
      }
      
      lijst[[2]][i] = strptime(a3$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
      
      
    } else if (a3$priority == prioriteit[3] & a3$issue_type %in% short){
      
      a = markovchainSequence(20, mcMajorS, a3$to_status, include.t0 = T)
      
      b = numeric(length = length(a))
      
      for(j in 0:(length(a)-2) ){
        
        b[j+1] = majorTimesS[a[1+j],a[2+j]]
        
        if( a[1+j] == "Resolved" & a[2+j] == "Closed"){
          break
        }
      }
      
      lijst[[2]][i] = strptime(a3$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
      
      
    } else if (a3$priority == prioriteit[4] & a3$issue_type %in% long){
      
      a = markovchainSequence(20, mcMajorL, a3$to_status, include.t0 = T)
      
      b = numeric(length = length(a))
      
      for(j in 0:(length(a)-2) ){
        
        b[j+1] = majorTimesL[a[1+j],a[2+j]]
        
        if( a[1+j] == "Resolved" & a[2+j] == "Closed"){
          break
        }
      }
      
      lijst[[2]][i] = strptime(a3$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
      
      
    } else if (a3$priority == prioriteit[4] & a3$issue_type %in% short){
      
      a = markovchainSequence(20, mcMajorL, a3$to_status, include.t0 = T)
      
      b = numeric(length = length(a))
      
      for(j in 0:(length(a)-2) ){
        
        b[j+1] = majorTimesL[a[1+j],a[2+j]]
        
        if( a[1+j] == "Resolved" & a[2+j] == "Closed"){
          break
        }
      }
      
      lijst[[2]][i] = strptime(a3$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
      
      
    } else if (a3$priority == prioriteit[5] & a3$issue_type %in% long){
      
      a = markovchainSequence(20, mcMajorL, a3$to_status, include.t0 = T)
      
      b = numeric(length = length(a))
      
      for(j in 0:(length(a)-2) ){
        
        b[j+1] = majorTimesL[a[1+j],a[2+j]]
        
        if( a[1+j] == "Resolved" & a[2+j] == "Closed"){
          break
        }
      }
      
      lijst[[2]][i] = strptime(a3$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
      
      
    }  else if (a3$priority == prioriteit[5] & a3$issue_type %in% short){
      
      a = markovchainSequence(20, mcMajorS, a3$to_status, include.t0 = T)
      
      b = numeric(length = length(a))
      
      for(j in 0:(length(a)-2) ){
        
        b[j+1] = majorTimesS[a[1+j],a[2+j]]
        
        if( a[1+j] == "Resolved" & a[2+j] == "Closed"){
          break
        }
      }
      
      lijst[[2]][i] = strptime(a3$created, "%Y-%m-%dT%H:%M:%OS%z") + (sum(b) *3600 *24)
      
    }
    
  }
  
  return(lijst[complete.cases(lijst),])
  
}
