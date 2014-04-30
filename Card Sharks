#Objective: Create a deck of cards as a data frame and deal 9 random hands for Texas Hold'em,
##deal the 5 center cards, rank the hands and record who wins,
###calculate winning percentage for all starting hands

#Create two lists, one for the numbers and one for the suits
y = c(2:14)
##y=c("Ace", 2:10,"Jack","Queen", "King")
suit = c("of Hearts","of Spades", "of Diamonds", "of Clubs")

#Combine as 52 row data.frame
deck= data.frame( rep(y,4), unlist(lapply(suit, function(suit) rep(suit,13))) )
names(deck)= c('number','suit')

##As list
#deck = sapply(suit, function(x) paste(y, x))

###As matrix
#dealt= matrix(sample(deck2,size=2*players,replace=FALSE),nrow=2, ncol=players)
#hand1= dealt[,1]
#msg= paste("Your hand:  ", hand1[1], " & ", hand1[2], sep="")
#show(msg)

#Delay dealing flop, turn, river
#show(paste("The flop: ", spread[1], ", ", spread[2], ", & ", spread[3], sep=""))
#show(paste("The turn: ", spread[1], ", ", spread[2], ", ", spread[3], ", & ", spread[4], sep=""))
#show(paste("The river: ", spread[1], ", ", spread[2], ", ", spread[3], ", ", spread[4], ", & ", spread[5], sep=""))


#Deal random hands and rank for winner, allows for ties
deal=function(deck){

dealt= deck[sample(rownames(deck),size=23,replace=F),]

spread = dealt[19:23,]

#Call hand ranking function
ranks = rankHands(dealt, spread)
hands = c(1:9)    
winHands = hands[ranks==max(ranks)]

#Store winning hands in data frame
startHands= data.frame(card1.number=numeric(), card1.suit=character(), card2.number=numeric(), card2.suit=character(), dealt = numeric(), win = numeric(), stringsAsFactors=FALSE)
for( i in hands) startHands= rbind(startHands, data.frame(card1 = dealt[(2*i-1),], card2 = dealt[(2*i),], dealt=1, win =0))
for( i in winHands) startHands[i,"win"] = 1
startHands
}

## Hand Ranking
rankHands = function(dealt, spread){

  ranks= c()
  hands = c( 1, 3, 5, 7, 9, 11, 13, 15, 17)
  #hand=data.frame(number=c(5,2,3,9,4,14,5), suit=c("S","H","H","D","H","H","H"))
  for (i in hands){
     rank = 0.0
     hand = rbind(dealt[c(i,(i+1)),],spread)
     hand = hand[order(hand[,"number"]),]
     
     high = max(hand[,"number"])/10
     high2= 0.0
     high3= 0.0
     
     #is pairs
     pairs = aggregate(suit~number, data=hand, FUN=length)
     kind2 = max(pairs[,2]) > 1
     if (kind2) {
       rank = 1
       high = max(pairs[ (pairs$suit>1), "number"])/10
       high2 = max(hand[(hand$number != (high*10)),"number"])/10
     }
     
     #is 2 pairs
     kind2x2 = sum(as.numeric(pairs[,2]>1)) > 1
     if (kind2x2) {
       rank = 2
       high =  max(pairs[ (pairs$suit>1), "number"])/10
       pairs2 = pairs[(pairs$number != (high*10)),]
       high2 = max(pairs2[(pairs2$suit>1),"number"])/10
       sub = hand[(hand$number != (high*10)),]
       high3 = max(sub[(sub$number != (high2*10)),"number"])/10 
     }
                   
     #is triple
     triples =aggregate(suit~number, data=hand, FUN=length)
     kind3 = max(triples[,2]) > 2
     if (kind3) {
       rank = 3
       high = max(triples[ (triples$suit>2), "number"])/10
       high2 = max(hand[(hand$number != (high*10)),"number"])/10
     }
     
     #is straight
     sequence = function(x, uniques){
       if (length(seq) < 5){
         if(uniques$number[x+1]-uniques$number[x] ==1) seq=c(seq, x+1)
         else if (uniques$number[x]==5 & length(seq)>3 & uniques$number[nrow(uniques)]==14) seq=c(seq, 7)
         else seq=c(x+1)
        }
       seq
}
     seq=c(1)
     uniques = hand[duplicated(hand$number)==F,]
     for( i in 1:(nrow(uniques)-1) )  {
       seq=sequence(i, uniques)
     }
     straight = length(seq) >4
     if (straight) {
       rank = 4
       high = max(hand[seq,"number"])/10
     }
     
     #compare = (hand[,"number"]- hand[3,"number"] ) - min(hand[,"number"]- hand[3,"number"] )
     #straight = sum(sapply(c(0,1,2,3,4), function(x) x %in% compare )) > 4
     #--------#
     #adjacent = 1 == (sapply(1:6, function(x) hand[,"number"][x+1]-hand[,"number"][x] ))
     #hand2 = hand[c(grep(T,adjacent),grep(T,adjacent)[length(grep(T,adjacent))]+1),]
     #adjacent2 = 1 == (sapply(1:(nrow(hand2)-1), function(x) (hand2[,"number"][x+1]- hand2[,"number"][x]) ))
     #straight = sum(as.numeric(adjacent2) ) > 3   
    
     
     #is flush
     suits = aggregate(number~suit, data=hand, FUN=length)
     flush = max(suits[,2]) > 4
     if(flush) {
       rank = 5
       high = max(hand[(hand$suit == suits[ (suits$number>3), 1]),"number"])/10
     }
     
     #is full house
     fullHouse = kind3 && kind2x2
     if (fullHouse) {
       rank = 6
       high = max(triples[ (triples$suit>2), "number"])/10 
       triples2 = triples[(triples$number != (high*10)),]
       high2 = max(triples2[(triples2$suit>1),"number"])/10
     }
     
     #is four of kind
     kind4 = max(aggregate(suit~number, data=hand, FUN=length)[,2]) > 3
     if (kind4) rank = 8     
     
     #is straight flush
     stFlush = flush & straight & max(aggregate(number~suit, data=hand[sapply(hand$number, function(x) x %in% hand[seq,"number"]),], FUN=length)[,2]) > 4
     if (stFlush) rank = 9
     
     #is royal flush,  c('14','13','12','11','10'), c('Ace','King','Queen','Jack','10')
     royal = stFlush && max(hand[seq,"number"]) ==14
     if (royal) rank = 10
       
     rank = rank + high/10 + high2/1000 + high3/100000
     ranks = c(ranks, as.numeric(rank))
  }
  ranks
}

#Create table to store hands
startHands= data.frame(card1.number=numeric(), card1.suit=character(), card2.number=numeric(), card2.suit=character(), dealt = numeric(), win = numeric(), stringsAsFactors=FALSE)

#Deal 5,000 games of Hold'em, rank hands and record winning hole cards
for (i in 1:5000){
  win_Hand = deal(deck=deck)
  startHands = rbind(startHands, win_Hand)
}

#Merge winning hands by number and suited or unsuited
library(plyr)
sum_wins = ddply(startHands, .(card1.number,card1.suit,card2.number,card2.suit), numcolwise(sum))
sums=sum_wins
sums[,7] = sapply(c(1:nrow(sums)), function(x) if(sums[x,1] == sums[x,3]) paste(sums[x,1], "'s",sep="") else paste(sums[x,c(1,3)][order(-sums[x,c(1,3)])], sep="", collapse=","))
sums[,8] = sapply(c(1:nrow(sums)), function(x) if(sums[x,2] == sums[x,4]) "suited" else "unsuited")

#Tally and find winning percentage for each hand
sums2 = ddply(sums, .(V7,V8), numcolwise(sum))
sums2 = sums2[,c(1,2,5,6)]
sums2[,5]= round(100*(sums2[,4]/sums2[,3]))
names(sums2)[c(1,2,5)]=c("Hole", "Suited", "%wins")
win=sums2[order(-sums2[,5]),]
par(las=2) # make label text perpendicular to axis
barplot(win[1:20,5], names.arg=paste(win[1:20,1], win[1:20,2], sep=" "), cex.names=.6, horiz=T, col=rainbow(20, s = 1, v = 1, start = .29, end = .98, alpha = .8))
win
