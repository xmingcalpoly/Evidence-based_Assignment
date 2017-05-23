#PART 1 
#defining one fuction that can return one given year's dataframe, which will be used in part 2.
# n is the year
history<-function(n){         
  #Setting a string variable, which is used in paste function later to find the data souce online
  filestr="http://homepages.cae.wisc.edu/~dwilson/rsfc/history/howell/cf"
  year<-as.numeric(c(1960:2010)) #defing the variable year to record 1960 to 2010
  filename<-as.character() 
  #defing filename, in order to use paste function to construct filename vector
  #[2009] "http://homepages.cae.wisc.edu/~dwilson/rsfc/history/howell/cf2009gms.txt"
  #[2010] "http://homepages.cae.wisc.edu/~dwilson/rsfc/history/howell/cf2010gms.txt"
  j<-1960
  for (i in 1:length(year)) {
    filename[j]<-paste(filestr,year[i],"gms.txt", sep = "")
    j<-j+1
  }
  dataonline<-read.fwf (file=filename[n],c(11,28,4,28,3))  #load the data according to filename(n),n is year
  dataonline<-data.frame(lapply(dataonline, as.character), stringsAsFactors=FALSE) 
  #change the column of dataonlne into charcter.
  data_table<-as.data.frame(table(c(dataonline$V2,dataonline$V4)))
  #defing data_table, and using table function to record the times of games for one single team
  data_table$Var1<-as.character(data_table$Var1)
  omit_team<-as.character()
  #using omit_team to record the team whose the game times is less than 6
  j<-1
  for (i in 1:length(data_table$Var1)) {
    if(data_table$Freq[i] < 6){
      omit_team[j]<-data_table$Var1[i]
      j<-j+1
    }
  }
  #making the value of omit_team in dataonline NA
  for (i in 1:length(omit_team)){
    for (j in 1:length(dataonline$V2)){
      if (isTRUE(omit_team[i]==dataonline$V2[j])){   #home team
        dataonline$V2[j]<-NA
      }
      if (isTRUE(omit_team[i]==dataonline$V4[j])){  #away team
        dataonline$V4[j]<-NA
      }
    }
  }
  for (i in 1:length(dataonline$V1)){
    if(isTRUE(dataonline$V3[i]==dataonline$V5[i])){   # if tied, making the value of team NA
      dataonline$V3[i]=NA
      dataonline$V5[i]=NA
    }
  }
  dataonline<-na.omit(dataonline)  #getting the final raw dataframe
  rownames(dataonline)<-NULL    #organizing the order name of new dataonline 
  #calculate the total times of game for each team
  data_table<-as.data.frame(table(c(dataonline$V2,dataonline$V4)))
  total<-as.numeric(data_table$Freq)
  teams_levels<-levels(as.factor(c(dataonline$V2,dataonline$V4)))   #which teams are in one single season
  wins<-as.numeric()
  loses<-as.numeric()
  wins_home<-as.numeric()
  wins_away<-as.numeric()
  dataonline$V3<-as.numeric(dataonline$V3)
  dataonline$V5<-as.numeric(dataonline$V5)
  #computing the win times at home for each team
  for (i in 1:length(teams_levels)) {
    wins_home[i]<-0
    for (j in 1:length(dataonline$V2)) {
      if(isTRUE(teams_levels[i]==dataonline$V2[j])){
        if(dataonline$V3[j]>dataonline$V5[j]){
          wins_home[i]<-wins_home[i]+1
        }
      }
    }
  }
  #computing the win times at away for each team
  for (i in 1:length(teams_levels)) {
    wins_away[i]<-0
    for (j in 1:length(dataonline$V5)) {
      if(isTRUE(teams_levels[i]==dataonline$V4[j])){
        if(dataonline$V3[j]<dataonline$V5[j]){
          wins_away[i]<-wins_away[i]+1
        }
      }
    }
  }
  wins<-wins_home+wins_away   #calculating the winning times
  loses<-total-wins           #calculating the losses times
  df<-data.frame(teams=teams_levels,wins=wins,loses=loses)  #constructing the original df
  opponent<- vector("list", length(df$teams))   #adding opponent
  #setting opponent list
  for (i in 1:length(df$teams)) {
    k<-1
    for (j in 1:length(dataonline$V2)) {
      if(isTRUE(df$teams[i]==dataonline$V2[j])){
        opponent[[i]][k]<-which(df$teams==dataonline$V4[j])
        k<-k+1
      }
      if(isTRUE(df$teams[i]==dataonline$V4[j])){
        opponent[[i]][k]<-which(df$teams==dataonline$V2[j])
        k<-k+1
      }
    }
  }
  years<-numeric(length(teams_levels))  #constructing year column 
  for (i in 1:length(teams_levels)) {
    years[i]<-n
    }
  df<-data.frame(years=years,teams=teams_levels,wins=wins,losses=loses,opponents=I(opponent)) 
  #constructing the final df, adding list opponent column using opponents=I(opponent))
  return(df)      #return df
}
ncaaf_history<- data.frame(season = factor(),
                teams = factor(), 
                wins = numeric(), 
                losses = numeric(), 
                opponents = double())


#Using "for loop" to consctruct all year's dataframe together. It will run for 5 mins. 
for (i in 1960:2010) {
  temp<-history(i)
  ncaaf_history<-rbind(ncaaf_history,temp)
}
save(ncaaf_history, file = "ncaaf_history.rdata")


#PART 2
# constructing colley matrix for one given year by using the dataframe from part 1, and calculating the solution of ranking.
ncaaf<-function(n){
  df<-history(n)     #getting the data for a given year by using the history fuction
  num<-as.numeric(length(df$teams)) 
  rnames<-as.character(df$teams[1:length(df$teams)])   
  cnames<-as.character(df$teams[1:length(df$teams)])   
  colley_matrix<-matrix(data=0, nrow = length(df$teams),ncol= length(df$teams),dimnames = list(rnames, cnames), byrow = TRUE)
  i<-as.integer()     
  j<-as.integer()
  for (i in 1:length(df$teams)) {
    for (j in 1:length(df$opponents[[i]])) {
      opponents_No.<-as.integer(df$opponents[[i]][j])
      colley_matrix[i, opponents_No.]=colley_matrix[i, opponents_No.]-1 
    }
  }
  for (k in 1:length(df$teams)) {
    colley_matrix[k,k]=2+length(df$opponents[[k]])  
  }
  b<-vector("numeric", length(df$teams)) 
  for (i in 1:length(df$teams)) {
    b[i]<-1 + (df$wins[[i]]-df$losses[[i]])/2
  }
  x<-solve(colley_matrix, b)
  score_solution<-data.frame(team_name=rnames[1:length(df$teams)],score = unname(x))
  ranking<-score_solution[order(-score_solution$score),]
  solution<-data.frame(teams=ranking$team_name,scores=ranking$score)
  return(solution)
}
solutions<-ncaaf(2000)

  