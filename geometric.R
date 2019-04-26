theta = 0.5
x = 0
p_Dec = 0.5
p_Same = theta/2
p_Inc = 1 - p_Dec - p_Same
result <- matrix(data=NA,nrow=1,ncol=1)
Size <- c(1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 1000, 10000, 10000, 10000, 10000)

for(s in Size) {
  for(i in 1:s*1000) {
    check = runif(1, 0, 100)
    
    #Case x = 0
    if (x == 0) {
      if(check < 100*(p_Dec + p_Same)) {
        x = 0
      } else {
        x = 1
      }
    }
    
    #x is positive
    if (x != 0) {
      check = runif(1, 0, 100)
      if(check < 100*p_Dec) {
        x = x - 1
      } else {
        if (check >= 100*(p_Dec + p_Same)) {
          x = x + 1
        }
      }
    }
    
    if (i %% 1000 == 0) {
      if (is.element(NA, result)) {
        result = c(x)
      } else {
        result = array(c(result, x))
      }
    }
  }
  
  sample = length(result)
  geometric <- rgeom(sample, 0.5)
  
  if (max(table(geometric)) < max(table(result))) {
    maxY = max(table(result))
  } else {
    maxY = max(table(geometric))
  }
  
  mag = 10
  i = 1
  while (maxY %/% mag >= 10) {
    i = i + 1
    mag = 10^i
  }
  if (maxY %/% mag >= 4) {
    mag = mag * 10
  } else {
    mag = mag * 5
  }
  
  if (tail(sort(result), 1) < tail(sort(geometric), 1)) {
    maxX = tail(sort(geometric), 1)
  } else {
    maxX = tail(sort(result), 1)
  }
  
  par(mfrow=c(2,1))
  hist(geometric, breaks = tail(sort(geometric), 1),
       main = "Histogram of Geometric Distribution", xlim = c(0,maxX+1), 
       ylim = c(0,mag), sub = paste("Sample size:", toString(sample)), labels = TRUE, 
       xlab = "Number of Failures until First Success", right = F)
  hist(result, breaks = tail(sort(result), 1),
       main = "Histogram of Metropolis-Hastings Algorithm of Geometric Distribution",
       xlim = c(0,maxX+1), ylim = c(0,mag), sub = paste("Sample size:", toString(sample)),
       labels = TRUE, xlab = "Number of Failures until First Success", right = F)
}
