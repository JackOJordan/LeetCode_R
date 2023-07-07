# Problem 1 - Two Sum - Under one second

nums <- c(1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,1,2,3,4,5,6,7,8,9,10,11,12)
target <- 23
for(i in 1:length(nums)){
  test <- nums[i]
  if((target - nums[i]) %in% nums[-i]) {print(c(i, which(nums == (target - nums[i])))); break}
}

# Problem 2 - Add Two Numbers - Under one second

l1 <- c(5, 2, 5, 0, 8, 5, 1, 5, 6, 1, 3, 3, 4, 1, 2, 9, 4, 5, 9, 6, 7, 4, 7, 4, 0, 1, 8, 1, 2, 5, 2, 8, 4, 0, 8, 5, 0, 4, 0, 6, 0, 2, 2, 8, 3, 0, 7, 1, 2, 7, 9, 8, 5, 4, 3, 2, 3, 6, 0, 3, 5, 1, 1, 7, 0, 6, 4, 0, 4, 6, 3, 6, 9, 7, 2, 7, 3, 5, 6, 9, 5, 2, 7, 9, 0, 0, 1, 7, 1, 5, 6, 8, 1, 9, 7, 5, 0, 6, 7, 2); l2 <- c(0, 3, 1, 2, 8, 7, 2, 7, 5, 6, 3, 0, 9, 7, 5, 1, 2, 9, 1, 3, 6, 6, 5, 1, 7, 0, 4, 8, 0, 7, 4, 7, 2, 1, 2, 6, 5, 1, 7, 5, 7, 3, 6, 9, 2, 5, 5, 0, 7, 4, 9, 7, 7, 2, 9, 4, 7, 2, 7, 8, 4, 5, 3, 5, 2, 8, 3, 3, 0, 8, 1, 3, 2, 6, 9, 2, 9, 0, 4, 0, 3, 7, 9, 3, 9, 3, 4, 3, 3, 9, 5, 8, 7, 6, 5, 7, 9, 8, 4, 7)
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

# Problem 3 - Longest Substring Without Repeating Characters - Under one second

# s <- "abcabcdklasbdlbsdlnvslsdbghshdfkhwlubdvhlkvshsdlvblsabdoglbrgljbjdgbbb"

# Note: Will need to change some stuff if the character \ appears in the string.

setwd("C:/Users/jackj.LAPTOP-U1V11TR1/Documents/R Projects")
stringConnect <- file(description = "LeetCode3.txt", open = "r", blocking = TRUE)
string <- readLines(stringConnect)

t <- unlist(strsplit(string, split = ""))

l <- length(t)
highestLength <- 1
index <- c()
test <- c()
for(j in 1:l){
  if((l - j) <= highestLength) break
  # Stop checking if there aren't enough characters left to beat the highest
  test <- c(t[j:(j + highestLength)])
  # For each character, check if the following substring will beat the highest
  if(sum(duplicated(test)) == 0){
    # Only do this for highest length substrings
    for(k in (j + highestLength + 1):l){
      test <- c(test, t[k])
      # Add one extra character at a time
      if(sum(duplicated(test)) > 0){
        highestLength <- length(test) - 1; index <- j; break
      }
      # When a duplicated value is found, stop checking
    }
  }
}
highestLength
t[index:(index + highestLength - 1)]

# Problem 4 - Median of Two Sorted Arrays - Under one second

# a1 <- c(1,3,4,6,8,9,12,35,67,79,102); a2 <- c(2,3,4,5,6,7,8,9,10,11,23,89,105)

setwd("C:/Users/jackj.LAPTOP-U1V11TR1/Documents/R Projects")
a1Connect <- file(description = "LeetCode4a1.txt", open = "r", blocking = TRUE)
a1 <- readLines(a1Connect)
a1 <- as.numeric(unlist(strsplit(a1, split = ", ")))
a2Connect <- file(description = "LeetCode4a2.txt", open = "r", blocking = TRUE)
a2 <- readLines(a2Connect)
a2 <- as.numeric(unlist(strsplit(a2, split = ", ")))

merged <- sort(c(a1,a2))

median <- c()
l <- length(merged)

if((l %% 2) == 0){
  median <- mean(c(merged[l/2], merged[(l/2)+1]))
} else {
  median <- merged[ceiling(l/2)]
}

median

# Problem 5 - Longest Palindromic Substring - Under one second

s <- "iohdghlabcdefghijklmnopqrstuvwxykjbnlkjfdklffffffffkdfopeifklsppowihdfdsvbsubdvhdbvjkbkjnoifebpejgvmpweflkndgkajdfklsbdbgbkdkbgbjbglkbdvllvdbklgbjbgbkdkbgbdbslplkjadggsdkljvbjvjhoiphsofdjkglgdjfkldhfkiohdghlabcdefghijklmnopqrstuvwxykjbnlkjfdklffffffffkdfopeifklsppowihdfdsvbsubdvhdbvjkbkjnoifebpejgvmpweflkndgkajdfklsbdbgbkdkbgbjbglkbdvllvdbklgbjbgbkdkbgbdbslplkjadggsdkljvbjvjhoiphsofdjkglgdjfkldhfkiohdghlabcdefghijklmnopqrstuvwxykjbnlkjfdklffffffffkdfopeifklsppowihdfdsvbsubdvhdbvjkbkjnoifebpejgvmpweflkndgkajdfklsbdbgbkdkbgbjbglkbdvllvdbklgbjbgbkdkbgbdbslplkjadggsdkljvbjvjhoiphsofdjkglgdjfkldhfkiohdghlabcdefghijklmnopqrstuvwxykjbnlkjfdklffffffffkdfopeifklsppowihdfdsvbsubdvhdbvjkbkjnoifebpejgvmpweflkndgkajdfklsbdbgbkdkbgbjbglkbdvllvdbklgbjbgbkdkbgbdbslplkjadggsdkljvbjvjhoiphsofdjkglgdjfkldhfkiohdghlabcdefghijklmnopqrstuvwxykjbnlkjfdklffffffffkdfopeifklsppowihdfdsvbsubdvhdbvjkbkjnoifebpejgvmpweflkndgkajdfklsbdbgbkdkbgbjbglkbdvllvdbklgbjbgbkdkbgbdbslplkjadggsdkljvbjvjhoiphsofdjkglgdjfkldhfk"

t <- unlist(strsplit(s, split = ""))

l <- length(t)
maxLength <- 0
index <- 0
for(i in 1:l){
  if(l - i < maxLength) break
  ends <- which(t == t[i])
  ends <- ends[ends > (i + maxLength)]
  for(j in ends){
    test <- t[i:j]
    if(!(FALSE %in% (test == rev(test)))) {maxLength <- length(test); index <- i}
  }
}

maxLength
paste(t[index:(index + maxLength - 1)], collapse = "")

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

# Problem 14 - Longest Common Prefix - Under one second

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

# Problem 15 - 3Sum

input <- c(1,2,3,-1,-2,-3,4,4)

l <- length(input)
winners <- list()
for(i in 1:(l-2)){
  for(j in (i+1):(l-1)){
      if(-(input[i] + input[j]) %in% input[(j+1):l]){
        winners <- append(winners, list(sort(c(input[i], input[j], -(input[i] + input[j])))))
      }
      # Systematically choose two values; if their inverse appears later in the input, put them all in a list
  }
}

winners <- winners[!duplicated(winners)]
winners

# Problem 16 - 3Sum Closest

nums <- c(100,5,120,1,102837,-19,3248,309785,9236598245,5,1,2,-12,-18,8)
target <- 0

l <- length(nums)
closest <- abs((nums[1] + nums[2] + nums[3]) - target)
# Distance away from target
answer <- closest
for(i in 1:(l-2)){
  if(answer == target) break
  for(j in (i+1):(l-1)){
    ijDist <- target - (nums[i] + nums[j])
    # How far away is (i + j) from the target?
    testVals <- nums[(j+1):l]
    # Later values to check
    distances <- abs(testVals - ijDist)
    # How far away each sum is from the target
    if(min(abs(distances)) < closest){
      closest <- abs(nums[i] + nums[j] + testVals[which(abs(distances) == min(abs(distances)))]);
      answer <- nums[i] + nums[j] + testVals[which(abs(distances) == min(abs(distances)))]
    }
    # If a distance is closer to the target, update the closest value
  }
}
answer

# Problem 17 - Letter Combinations of a Phone Number

letters <- list(c(),c("a","b","c"),c("d","e","f"),c("g","h","i"),c("j","k","l"),
                c("m","n","o"),c("p","q","r","s"),c("t","u","v"),c("w","x","y","z"))
digits <- "3745"
digits <- as.numeric(unlist(strsplit(digits, split = "")))
output <- c()
for(i in 1:length(letters[[digits[1]]])){
  for(j in 1:length(letters[[digits[2]]])){
    for(k in 1:length(letters[[digits[3]]])){
      for(l in 1:length(letters[[digits[4]]])){
        output <- c(output, paste(c(letters[[digits[1]]][i],letters[[digits[2]]][j],letters[[digits[3]]][k],letters[[digits[4]]][l]), collapse = ""))
      }
    }
  }
}

# Problem 18 - 4Sum

nums <- c(1,0,-1,0,-2,2,3)
target <- 5

l <- length(nums)
winners <- list()
for(i in 1:(l-3)){
  for(j in (i+1):(l-2)){
    for(k in (j+1):(l-1)){
      testVals <- nums[(k+1):l]
      # Systematically choose 3 numbers, then test the rest
      distances <- target - (nums[i] + nums[j] + nums[k] + testVals)
      # Distance from the target when summing each number
      if(0 %in% distances){
        winners <- append(winners, list(sort(c(nums[i],nums[j],nums[k],(target - (nums[i] + nums[j] + nums[k]))))))
      }
    }
  }
}
winners <- winners[!duplicated(winners)]
# Remove duplicated answers
winners

# Problem 19 - Remove Nth Node From End of List

head <- c(1,2,3,4,5); n <- 2
head[-(length(head) - n + 1)]

# Problem 20 - Valid Parentheses

test <- c("(","(",")")
length(test[test == "("])

bool <- 0

if(!(length(test[test == "("]) == length(test[test == ")"]))) bool <- FALSE
if(!(length(test[test == "{"]) == length(test[test == "}"]))) bool <- FALSE
if(!(length(test[test == "["]) == length(test[test == "]"]))) bool <- FALSE
# There should be the same number of open/close brackets of each type



