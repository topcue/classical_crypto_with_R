## Subst Cipher
#############################################

# setwd("/Users/topcue/myR")
rm(list=ls())

#################### Subst Cipher ####################
rm(list=ls())

islower <- function(ch, key) {
	return (ifelse(is.na(match(ch, letters)), FALSE, TRUE))
}
isupper <- function(ch, key) {
	return (ifelse(is.na(match(ch, LETTERS)), FALSE, TRUE))
}

enc_key <- sample(letters, 26)

SubstEnc <- function(ch, key) {
	if(islower(ch)) {
		return (key[match(ch, letters)])
	} else if (isupper(ch)) {
		return (toupper( key[match(tolower(ch), letters)] ))
	} else {
		return (ch)
	}
}

SubstDec <- function(ch, key) {
	if(islower(ch)) {
		return (letters[match(ch, key)])
	} else if (isupper(ch)) {
		return (toupper( letters[match(tolower(ch), key)] ))
	} else {
		return (ch)
	}
}

my_msg1 <- "Hi, ecnrypt me!"
my_vector <- strsplit(my_msg1, split="")[[1]] # vector of list
print(my_vector)

# enc
enc_str <- sapply(my_vector, function(ch) { SubstEnc(ch, enc_key) } )
print(enc_str)
enc_msg <- paste(enc_str, collapse="")
print(enc_msg)

# dec
dec_str <- sapply(enc_str, function(ch) { SubstDec(ch, enc_key) } )
print(dec_str)
dec_msg <- paste(dec_str, collapse="")
print(dec_msg)

#################### Caesar Cipher ####################

# EOF

