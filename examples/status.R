
# create statusvector

status <- deducorrect:::status(5)
status[1:5] <- c("invalid",NA,"corrected","valid","partially")

# 
which(status < "valid")







