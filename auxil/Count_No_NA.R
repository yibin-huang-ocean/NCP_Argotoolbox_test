
# Count the data without NAN
Count_No_NA <- function ( data ) {
  nubmer_NA <- length (data)- sum (is.na(data))
  return (nubmer_NA )
}
