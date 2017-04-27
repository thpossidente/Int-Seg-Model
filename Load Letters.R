#install.packages('png')
library('png')
#install.packages('abind')
library('abind')

alphabet <- list(
  a <- as.vector(t(1-adrop(readPNG('AlphabetPNG/A.png')[,,1,drop=F], drop=3))),
  b <- as.vector(t(1-adrop(readPNG('AlphabetPNG/B.png')[,,1,drop=F], drop=3))),
  c <- as.vector(t(1-adrop(readPNG('AlphabetPNG/C.png')[,,1,drop=F], drop=3))),
  d <- as.vector(t(1-adrop(readPNG('AlphabetPNG/D.png')[,,1,drop=F], drop=3))),
  e <- as.vector(t(1-adrop(readPNG('AlphabetPNG/E.png')[,,1,drop=F], drop=3))),
  f <- as.vector(t(1-adrop(readPNG('AlphabetPNG/F.png')[,,1,drop=F], drop=3))),
  g <- as.vector(t(1-adrop(readPNG('AlphabetPNG/G.png')[,,1,drop=F], drop=3))),
  h <- as.vector(t(1-adrop(readPNG('AlphabetPNG/H.png')[,,1,drop=F], drop=3))),
  i <- as.vector(t(1-adrop(readPNG('AlphabetPNG/I.png')[,,1,drop=F], drop=3))),
  j <- as.vector(t(1-adrop(readPNG('AlphabetPNG/J.png')[,,1,drop=F], drop=3))),
  k <- as.vector(t(1-adrop(readPNG('AlphabetPNG/K.png')[,,1,drop=F], drop=3))),
  l <- as.vector(t(1-adrop(readPNG('AlphabetPNG/L.png')[,,1,drop=F], drop=3))),
  m <- as.vector(t(1-adrop(readPNG('AlphabetPNG/M.png')[,,1,drop=F], drop=3))),
  n <- as.vector(t(1-adrop(readPNG('AlphabetPNG/N.png')[,,1,drop=F], drop=3))),
  o <- as.vector(t(1-adrop(readPNG('AlphabetPNG/O.png')[,,1,drop=F], drop=3))),
  p <- as.vector(t(1-adrop(readPNG('AlphabetPNG/P.png')[,,1,drop=F], drop=3))),
  q <- as.vector(t(1-adrop(readPNG('AlphabetPNG/Q.png')[,,1,drop=F], drop=3))),
  r <- as.vector(t(1-adrop(readPNG('AlphabetPNG/R.png')[,,1,drop=F], drop=3))),
  s <- as.vector(t(1-adrop(readPNG('AlphabetPNG/S.png')[,,1,drop=F], drop=3))),
  t <- as.vector(t(1-adrop(readPNG('AlphabetPNG/T.png')[,,1,drop=F], drop=3))),
  u <- as.vector(t(1-adrop(readPNG('AlphabetPNG/U.png')[,,1,drop=F], drop=3))),
  v <- as.vector(t(1-adrop(readPNG('AlphabetPNG/V.png')[,,1,drop=F], drop=3))),
  w <- as.vector(t(1-adrop(readPNG('AlphabetPNG/W.png')[,,1,drop=F], drop=3))),
  x <- as.vector(t(1-adrop(readPNG('AlphabetPNG/X.png')[,,1,drop=F], drop=3))),
  y <- as.vector(t(1-adrop(readPNG('AlphabetPNG/Y.png')[,,1,drop=F], drop=3))),
  z <- as.vector(t(1-adrop(readPNG('AlphabetPNG/Z.png')[,,1,drop=F], drop=3)))
)

letters <- list(
  "A" = a,
  "B" = b,
  "C" = c,
  "D" = d,
  "E" = e,
  "F" = f,
  "G" = g,
  "H" = h,
  "I" = i,
  "J" = j,
  "K" = k,
  "L" = l,
  "M" = m,
  "N" = n,
  "O" = o,
  "P" = p,
  "Q" = q,
  "R" = r,
  "S" = s,
  "T" = t,
  "U" = u,
  "V" = v,
  "W" = w,
  "X" = x,
  "Y" = y,
  "Z" = z
)

words <- list(
  abc <- cbind(a,b,c),
  def <- cbind(d,e,f),
  ghi <- cbind(g,h,i),
  jkl <- cbind(j,k,l),
  mno <- cbind(m,n,o),
  pqr <- cbind(p,q,r), 
  stu <- cbind(s,t,u), 
  vwx <- cbind(v,w,x), 
  yz <- cbind(y,z)
  
)