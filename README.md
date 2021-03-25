# VectorParseDate
Parse a vector dates without knowing the format of the dates.


## Problem Solved

Several files are coming in from many places and they are all similar but each file has different date formats. but in any given file the date formats are consistent. Think 30 Hospitals feeding in data from their internal system via excel or csv export.

## Usage 
### Install
`remotes::install_github("hswerdfe/VectorParseDate")`
### use
```
dts = c("03/03/92", "03/21/94", "03/02/99", "03/07/02")
vector_parse_dates(dts)
```
