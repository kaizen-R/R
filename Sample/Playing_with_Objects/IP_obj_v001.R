# File: IP_obj_v001.R
# Just some demo code, does not pretend to be well organized here.
# Author/Date: Check GitHub metadata :)

# Several IP objects already exist:
# RIP46, iptools...

# Let's follow one of the many guidelines from:
#  "Clean Code: A Handbook of Agile Software Craftsmanship"
# by: Robert C. Martin
# Namely:
#   Functions should be short and do only one thing.
# This will increase number of function calls, I'll have to check impact on speed, though, in R.
# Anyway, and so maybe it makes sense to separate the test in another function here:
is_valid_ip_string_format <- function(ip = NA)  {
  is.character(ip) && grepl("^((25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)(\\.|$)){4}", ip)
} ## Implicit returns and implicit TRUE / FALSE eval

# Quickly check if this does what we want
is_valid_ip_string_format("10.0.0.1")
is_valid_ip_string_format("10.0.0,1")
is_valid_ip_string_format("unexpected text")
is_valid_ip_string_format(NA)
is_valid_ip_string_format(2)


# The next bit (functions ip2long & long2ip) was copied from:
#  www.wiley.com/go/datadrivensecurity
# A recommended book for the subject matter of the Kaizen-R blog, btw.

library("bitops")

# This next function is quite "computer-geeky":
# bit-wise shifting is equivalent to multiplying by 2^8, etc.
ip2long <- function(ip) {
  ips <- unlist(strsplit(ip, ".", fixed = TRUE))
  octet <- function(x, y) bitOr(bitShiftL(x, 8), y)
  Reduce(octet, as.integer(ips))
}

long2ip <- function(longip) {
  octet <- function(nbits) bitAnd(bitShiftR(longip, nbits), 0xFF)
  paste(Map(octet, c(24, 16, 8, 0)), sep = "", collapse = ".")
}


## NOTE:
# An clearer version, for those less used to manipulating bits and bytes,
# with less bits manipulations and stuff, could look like this:
ip2long_vSimple <- function(ip = NA) {
  # Extract each byte, separated by ".", and keep it as integers:
  ip_bytes <- as.integer(unlist(strsplit(ip, ".", fixed = TRUE)))
  # Transform into a numeric of up to 32 bits, e.g. an integer:
  2 ^ 24 * ip_bytes[1] + 2 ^ 16 * ip_bytes[2] + 2 ^ 8 * ip_bytes[3] + ip_bytes[4]
}

## Anyhow:
# Now let's see what would happen with say a dataframe with one column of IP addresses.
# But of course, the dataframe will have weird values.
# Me, I like to either "break", or set NA where things won't go my way...
# Let's suppose we go the "NA" way, then tests would look like these:

library("testthat")

# I'll keep this in an object (function) for easier reuse later on...
runtests <- function() {
  test_that("testing different IP manipulations for numeric save", {
    # Quick test:
    expect_equal(is.numeric(ip2long("10.0.0.1")), TRUE)
    expect_equal(ip2long("10.0.0.1"), 167772161)
    expect_equal(long2ip(ip2long("10.0.0.1")), "10.0.0.1")
    
    # Let's test some extreme, legal values:
    expect_equal(ip2long("255.255.255.255"), 4294967295)
    expect_equal(long2ip(ip2long("255.255.255.255")), "255.255.255.255")
    
    expect_equal(ip2long("0.0.0.0"), 0)
    expect_equal(long2ip(ip2long("0.0.0.0")), "0.0.0.0")
    
    # Now let's stress it a bit more...:
    # case empty variable
    expect_equal(ip2long(), as.integer(NA))
    expect_equal(long2ip(), as.character(NA))
    expect_equal(ip2long(NA), as.integer(NA))
    expect_equal(long2ip(NA), as.character(NA))
    expect_equal(ip2long(NULL), as.integer(NA))
    expect_equal(long2ip(NULL), as.character(NA))
    expect_equal(ip2long(""), as.integer(NA))
    
    # Case invalid inputs:
    #expect_equal(long2ip(ip2long("unexpected text")), as.character(NA))
    expect_equal(long2ip(ip2long("10.0.0,1")), as.character(NA))
    expect_equal(long2ip("wrong input by user"), as.character(NA))
    # reading csv often times will gather IPs as factors, not text:
    expect_equal(long2ip(ip2long(as.factor("10.0.0.1"))), as.character(NA))
    
    # 32 bits bit shifts will do that to you - should be out of bound:
    expect_equal(long2ip(ip2long("10.0.0.256")), as.character(NA))
    expect_equal(long2ip(ip2long("256.0.0.1")), as.character(NA))
    expect_equal(long2ip(ip2long("-10.0.0.0")), as.character(NA))
  })
}

print(runtests())
# ALMOST what we would have wanted so far...
# And with no validity checks whatsoever!!
# ----


# Let's do a new version of the function before we go any further, adding validity tests:
ip2long <- function(ip = NA) {
  if (is_valid_ip_string_format(ip)) {
    ips <- unlist(strsplit(ip, ".", fixed = TRUE))
    octet <- function(x, y) bitOr(bitShiftL(x, 8), y)
    Reduce(octet, as.integer(ips)) # Remember, Implicit return...
  } else
    return(as.integer(NA))
}

long2ip <- function(longip = NA) {
  if (is.numeric(longip) && !is.na(longip) && (longip <= 4294967295) && (longip >= 0)) {
    octet <- function(nbits)
      bitAnd(bitShiftR(longip, nbits), 0xFF)
    paste(Map(octet, c(24, 16, 8, 0)), sep = "", collapse = ".")
  } else
    return(as.character(NA))
}

# Let's check how we fared now:
print(runtests())

# So I added checks, that brought essentially no value as default behaviour was OK.
# In turn, I made things much slower (almost twice as slow on my machine... tests deleted here.)

# Where is the bottleneck? Well clearly I added a function call with a grepl in it, so not fast...
# In the future, I'll have to test what speed profiling has to offer...


# An IP address can also be public or private.
# As such, let's create objects for IP addresses.

# Let's create IP objects now, we'll use reference objects.
IP_obj <- setRefClass(
  Class = "IP_obj", 
  fields = list(.ipnum = "numeric", .is_private = "logical"),
  methods = list(
    initialize = function(ip_string) {
      ips <- unlist(strsplit(ip_string, ".", fixed = TRUE))
      octet <- function(x, y) bitOr(bitShiftL(x, 8), y)
      .self$.ipnum <- Reduce(octet, as.integer(ips))
      
      .self$.is_private <- calculate_private()
    },
    get_ip_string = function() {
      octet <- function(nbits) bitAnd(bitShiftR(.self$.ipnum, nbits), 0xFF)
      paste(Map(octet, c(24, 16, 8, 0)), sep = "", collapse = ".")
    },
    calculate_private = function() {
      if((.self$.ipnum > 2886729728 && .self$.ipnum < 2886737919) || #172.16.0.0/12
         (.self$.ipnum > 167772160 && .self$.ipnum < 184549375) || # 10.0.0.0/8
         (.self$.ipnum > 3232235520 && .self$.ipnum < 3232301055) || # 192.168.0.0/16
         (.self$.ipnum > 2851995648 && .self$.ipnum < 2852061183)) # 169.254.0.0/16
        return(TRUE)
      FALSE
    },
    is_private_ip = function() {
      .self$.is_private
    }
  )
)

# Let's create a new instance of the Class IP_obj:
ip1 <- IP_obj("10.0.0.2")
ip1$is_private_ip()

# Now let's work with lists of objects for a sec, as a demo:
vector_of_ips <- c("10.0.1.1", "10.0.1.2", "192.168.1.1", "badip", "1.1.1.1", "2.2.2.2")
# Let's keep only valid IP addresses for the exercise:
vector_of_ips <- vector_of_ips[unlist(sapply(vector_of_ips, is_valid_ip_string_format))]
# Create new IP_obj objects for each string:
list_of_ip_objects <- lapply(vector_of_ips, IP_obj)
list_of_ip_objects
# Now for this list we can try and access methods for each object:
sapply(list_of_ip_objects, function(x){ x$get_ip_string() })
barplot(table(sapply(list_of_ip_objects, function(x){ x$is_private_ip() })))

# That's all folks for today...
