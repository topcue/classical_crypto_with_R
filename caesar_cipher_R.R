## Caesar Cipher
#############################################

# setwd("/Users/topcue/myR")
rm(list=ls())

#################### Caesar Cipher ####################
rm(list=ls())

islower <- function(ch, key) {
	return (ifelse(is.na(match(ch, letters)), FALSE, TRUE))
}

isupper <- function(ch, key) {
	return (ifelse(is.na(match(ch, LETTERS)), FALSE, TRUE))
}

CaesarEncrypt <- function(ch, key) {
	if(isupper(ch)) {
		return (LETTERS[(match(ch, LETTERS) + key - 1) %% 26 + 1 ])
	} else if(islower(ch)) {
		return (letters[(match(ch, letters) + key - 1) %% 26 + 1 ])
	} else {
		return (ch)
	}
}

CaesarDecrypt <- function(ch, key) {
	if(isupper(ch)) {
		return (LETTERS[(match(ch, LETTERS) - key - 1) %% 26 + 1 ])
	} else if(islower(ch)) {
		return (letters[(match(ch, letters) - key - 1) %% 26 + 1 ])
	} else {
		return (ch)
	}
}

enc_key <- 3
my_msg1 <- "Hi, ecnrypt me!"
my_vector <- strsplit(my_msg1, split="")[[1]] # vector of list
print(my_vector)

# enc
enc_str <- sapply(my_vector, function(ch) { CaesarEncrypt(ch, enc_key) } )
print(enc_str)
enc_msg <- paste(enc_str, collapse="")
print(enc_msg)

# dec
dec_str <- sapply(enc_str, function(ch) { CaesarDecrypt(ch, enc_key) } )
print(dec_str)
dec_msg <- paste(dec_str, collapse="")
print(dec_msg)

# EOF
