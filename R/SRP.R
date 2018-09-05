
#' @importFrom wordVectors read.vectors
#' @export
wordVectors::read.vectors

#' @importFrom wordVectors write.binary.word2vec
#' @export
wordVectors::write.binary.word2vec


# The R package performs no memoization, unlike python or javascript. This means it's much slower:
# suitable for exploring an existing SRP dataset, but not for creating a new one.

standard_text_weights = function(string, length_out=160) {
  if (length_out>160) {
    start = standard_text_weights(string,length_out=160)
    rest = standard_text_weights(paste0(string,"_"),length_out=length_out-160)
    return(c(start,rest))
  }
  if(length_out<160) {
    start = standard_text_weights(string,length_out=160)
    return(start[1:length_out])
  }
  
  # Take the SHA1 hash of the string
  hashed = digest::digest(string,algo="sha1",serialize = F,raw=T)
  
  # Break that down into a series of bits.
  # R's as.integer produces 32-bit little-endian integers:
  # What we need (and what's in python)
  # is 8-bit big-endian. A quick matrix transform and decomposition
  # accomplishes that here.
  bits = as.vector(sapply(as.integer(hashed),intToBits)[8:1,])
  
  # Create a vector of all 0s of the same length
  null = raw(length(bits))
  # Do some arithmetic on the logical values to return [-1, 1] instead of [0, 1].
  return(as.numeric(bits!=null)*2-1)
}

tokenize = function(string) {
  string = stringr::str_to_lower(string)
  string = stringr::str_replace_all(string, "#", " ")
  string = stringr::str_replace_all(string, "[0-9]", "#")
  string = stringr::str_extract_all(string, "[\\w#]+")
  string
}

#' Generate the SRP encoding of a string
#'
#' @param words A string or vector of strings. If standardize is TRUE, this will be tokenized for you.
#' @param counts The number of times each string occurs. If NULL (the default), each string is assumed to occur once.
#' @param length_out The desired dimensionality
#' @param standardize Whether to perform standard tokenization. 
#' @param log Whether to use a log transform.
#'
#' @return A vector giving the 
#' 
#' @export
#'
#' @examples
#' 
#' #The two functions below return the same amount. The first takes longer to run but less time to write.
#' stable_transform("oh happy happy day", length_out = 10)
#' stable_transform(c("oh", "happy", "day"), counts = c(1, 2, 1), length_out = 10)
#' 
stable_transform = function(words,counts=NULL,standardize=T,log=T,length_out=160) {
  if (length(words)==1 && is.null(counts)) {counts = rep(1, length(words))}
  if (standardize) {
    wordlist = tokenize(words)
    countlist= lapply(1:length(counts), function (i) {rep(counts[i], length(wordlist[[i]]))})
    words = unlist(wordlist)
    counts = unlist(countlist)
  }
  if (log) {
    counts = counts/sum(counts)
    counts = log(counts*1e05)
    counts[counts<0] = 0
  }
  weights = sapply(words,standard_text_weights, length_out = length_out)
  value = as.vector(weights %*% counts)
  return(value)
}
