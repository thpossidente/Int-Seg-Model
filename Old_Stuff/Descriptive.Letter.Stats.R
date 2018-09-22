just.words <- list(
  'cat', 'bow','sip', 'rad','zen', 'two','rub', 'vex','fox', 'wry','vow', 'zag','quo', 'fry','the', 'pew','dug', 'keg','yak', 'tax',
  'jaw', 'who','lax', 'til','sin', 'mud','yap', 'orb','ply', 'cry','tom', 'coy','any', 'jot','she', 'gig','axe', 'icy','elm', 'owl',
  'gag', 'nun','jay', 'rye','apt', 'sty','lit', 'why','hue', 'use'
)

rowname <- list('a','b','c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z')
colname <- list('freq', 'freq.pos.1', 'freq.pos.2', 'freq.pos.3')
letter.statistics <- matrix(0, nrow = 26, ncol = 4, dimnames = list(rowname, colname)) 



for(i in 1:26){
  for(j in 1:length(just.words)){
    for(k in 1:nchar(just.words[[j]])){
      if(rowname[[i]] == substr(just.words[[j]], k, k)){
        letter.statistics[i,1] <- letter.statistics[i,1] + 1
        letter.statistics[i,k+1] <- letter.statistics[i, k+1] + 1
      }
    }
  }
}

print(letter.statistics)


