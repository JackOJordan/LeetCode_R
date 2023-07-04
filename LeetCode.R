# Problem 1 - Two Sum

nums <- c(2,3,4,5,6,7,8,9,10)
target <- 18
for(i in 1:length(nums)){
  test <- nums[i]
  if((target - nums[i]) %in% nums[-i]) {print(c(i, which(nums == (target - nums[i])))); break}
}

# Problem 2 - Add Two Numbers

l1 <- c(8,4,6,2,5,9,0,6,4,3); l2 <- c(5,8,3,5,7,5,0,4,3)
output <- c()
carry <- 0
for(i in 1:max(c(length(l1), length(l2)))){
  if(is.na(l1[i])) l1[i] <- 0
  if(is.na(l2[i])) l2[i] <- 0
  if(l1[i] + l2[i] + carry < 10){
    output[i] <- l1[i] + l2[i] + carry
  } else {
    output[i] <- l1[i] + l2[i] + carry - 10 
    carry <- 1
    if(i == max(c(length(l1), length(l2)))) output[i+1] <- carry
    next
  }
  carry <- 0
}
output

# Problem 3 - Longest Substring Without Repeating Characters

s <- "abcabcdklasbdlbsdlnvslsdbghshdfkhwlubdvhlkvshsdlvblsabdoglbrgljbjdgbbb"

# Note: Will need to change some stuff if the character \ appears in the string.

t <- unlist(strsplit(s, split = ""))

highestLength <- 1
index <- c()
test <- c()
for(j in 1:length(t)){
  if((length(t) - j) <= highestLength) break
  test <- c(t[j:(j + highestLength)])
  if(sum(duplicated(test)) == 0){
    for(k in (j + highestLength + 1):length(t)){
      test <- c(test, t[k])
      if(sum(duplicated(test)) > 0){
        highestLength <- length(test) - 1; index <- j; break
      }
    }
  }
}
highestLength
t[index:(index + highestLength - 1)]

# Problem 4 - Median of Two Sorted Arrays

a1 <- c(1,3,4,6,8,9,12,35,67,79,102); a2 <- c(2,3,4,5,6,7,8,9,10,11,23,89,105)

merged <- sort(c(a1,a2))

median <- c()
l <- length(merged)

if((l %% 2) == 0){
  median <- mean(c(merged[l/2], merged[(l/2)+1]))
} else {
  median <- merged[ceiling(l/2)]
}

median

# Problem 5 - Longest Palindromic Substring

s <- "kajdfklsbdbgbkdkbgbjbglkbdvl"

t <- unlist(strsplit(s, split = ""))

maxLength <- 0
index <- 0
for(i in 1:length(t)){
  if(length(t) - i < maxLength) break
  for(j in i:length(t)){
    test <- t[i:(j+maxLength)]
    if(paste(test, collapse = "") == paste(rev(test), collapse = "")) {maxLength <- length(test); index <- i}
  }
}

maxLength
t[index:(index + maxLength - 1)]

# Problem 6 - Zigzag Conversion

s <- "PAYPALISHIRING"
n <- 4   # numRows

t <- unlist(strsplit(s, split = ""))

out <- c()
if(n > 1){
  out <- c(out, t[seq(1, length(t), by = (2*n-2))])
  if(n > 2){
    for(i in 2:(n-1)){
      a <- seq(i,length(t), by = (2*n-2))
      b <- seq((2*n-2) - i + 2,length(t), by = (2*n-2))
      out <- c(out, t[sort(c(a,b))])
    }
  }
  out <- c(out, t[seq(n, length(t), by = (2*n-2))])
} else {
  out <- t
}

out <- paste(out, collapse = "")
out

# Problem 7 - Reverse Integer

test <- -93286275

if(test < 0) { 
  test2 <- -test
} else {
  test2 <- test
}
test2 <- unlist(strsplit(as.character(test2), split = ""))

if(test < 0) {
  test2 <- as.numeric(paste(c("-", rev(test2)), collapse = ""))
} else {
  test2 <- as.numeric(paste(rev(test2), collapse = ""))
}

if(as.numeric(test2) > (2^31 - 1) | as.numeric(test2) < -(2^31)) test2 <- 0

test2

# Problem 8 - String to Integer (atoi)

s <- "    -48959557587897fh099"

test <- unlist(strsplit(s, split = ""))

if(test[1] == " "){
  for(i in 1:length(test)){
    if(!(test[i] == " ")) {test <- test[-(1:(i-1))]; break}
  }
}

neg <- FALSE
if(test[1] == "-") {neg <- TRUE; test <- test[-1]}

if(!is.na(as.numeric(test[i]))){
  for(i in 1:length(test)){
    if(is.na(as.numeric(test[i]))) {test <- as.numeric(paste(test[1:(i-1)], collapse = "")); break}
  }
} else {
  test <- 0
}

if(test > (2^31) | (test == (2^31) && !neg)){
  test <- as.numeric(unlist(strsplit(as.character(test), split = "")))
  newTest <- c()
  for(i in 1:length(test)){
    newTest <- c(newTest, test[i])
    if(as.numeric(paste(newTest, collapse = "")) > 2^31) {newTest <- newTest[-length(newTest)]; break}
  }
  test <- as.numeric(paste(newTest, collapse = ""))
}

if(neg) test <- -test

test

# Problem 9 - Palindrome Number

x <- -983757389

palindrome <- FALSE

if(as.character(x) == paste(rev(unlist(strsplit(as.character(x), split = ""))), collapse = "")) palindrome <- TRUE

palindrome

# Problem 10 - Regular Expression Matching

s <- "sfffjmrt"
p <- "s.*t"

if(grep(p, s) == 1){
  TRUE
} else FALSE

# Problem 11 - Container With Most Water

height <- c(1,8,6,2,5,4,8,3,7)

l <- length(height)
maxHeight <- 1

for(i in 1:(l-1)){
  factor <- maxHeight/height[i]
  # Minimum distance needed to beat current max. height
  if((l - i - factor) > 0){
    for(j in (i+1+floor(factor)):l){
      maxHeight <- max(c(maxHeight, (j-i)*min(c(height[i], height[j]))))
    }
  }
}

maxHeight

# Problem 12 - Integer to Roman

numerals <- c(1000,500,100,50,10,5,1)
names(numerals) <- c("M","D","C","L","X","V","I")

intToRom <- function(integer){
  l <- length(numerals)
  numeral <- c()
  for(i in 1:l){
    q <- ceiling(l/2) - ceiling(i/2)
    while(integer >= numerals[i]){
      # Writes all numerals of a certain type
      integer <- integer - as.integer(numerals[i])
      numeral <- c(numeral, names(numerals[i]))
    }
    if((numerals[i] - integer) <= 10^(q-1) && 0 < (numerals[i] - integer)){
      # Performs subtraction if needed
      integer <- integer - (as.integer(numerals[i]) - 10^(q-1));
      numeral <- c(numeral, names(numerals[2*ceiling(i/2)+1]),names(numerals[i]))
    }
  }
  numeral <- paste(numeral, collapse = "")
  numeral
}
intToRom(499)

# Problem 13 - Roman to Integer

numerals <- c(1000,500,100,50,10,5,1)
names(numerals) <- c("M","D","C","L","X","V","I")

s <- "MCDXCVIII"

test <- unlist(strsplit(s, split = ""))
for(i in 1:length(numerals)){
  vals <- which(test == names(numerals[i]))
  test[vals] <- as.integer(numerals[i])
}
test <- as.integer(test)
for(i in 1:(length(test)-1)){
  if(test[i] < test[i+1]){
    test[i] <- -test[i]
  }
}
sum(test)
test

# Problem 14 - Longest Common Prefix

strs <- c("flower","flow","flight")
strs2 <- c("dog","racecar","car")
strs3 <- c(rep(c("abcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyzabcdefghijklmnopqrstuvwxyz"), 200), "abc", "abcde")

commonPrefix <- function(string){
  l <- length(string)
  prefix <- unlist(strsplit(string[1], split = ""))
  # Initial prefix is the first word
  for(i in 2:l){
    test <- unlist(strsplit(string[i], split = ""))
    # Compare each word to the current prefix
    lt <- length(test)
    lp <- length(prefix)
    if(prefix[1] == test[1] && lp > 1){
    # If the first letter matches, check the rest
      for(j in 2:lt){
        if(!(prefix[j] == test[j])){
          prefix <- prefix[1:(j-1)];
          break
        }
        # When the first mismatch occurs, change the prefix to the shorter form
        if(j == min(c(lp,lt))){
          prefix <- prefix[1:j];
          break
        }
        # If no mismatches occurred and one of the prefix/test is finished, change the prefix to the shorter one
      }
    } else if(prefix[1] == test[1] && lp == 1){
      next
      # If the first (and only) letter matches, go to the next word
    } else {
      prefix <- c("");
      break
      # If the first letter doesn't match, then it's game over already
    }
  }
  paste(prefix, collapse = "")
}

commonPrefix(strs)
commonPrefix(strs2)
commonPrefix(strs3)
