
# functions

correctNetwork <- function(edf){
  # dirty but no choice than to do it manually
  # shitty encoding also
  
  # 
  edf = rbind(edf,c("PontdeSÃ¨vres","P.Auteuil",0.0))
  edf = rbind(edf,c("P.Auteuil","PontdeSÃ¨vres",0.0))
  
  # cut rocquencount <-> PAuteil into rocquencourt <-> Vaucresson <-> PAuteuil
  
  return(edf)
}

