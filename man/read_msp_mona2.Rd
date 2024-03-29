% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_msp_mona2.R
\name{read_msp_mona2}
\alias{read_msp_mona2}
\title{read_msp_mona2}
\usage{
read_msp_mona2(file, threads = 3)
}
\arguments{
\item{file}{The vector of names of ms2 files. MS2 file must be msp. The msp data must from MoNA.}

\item{threads}{The number of threads}
}
\value{
Return ms2 data. This is a list.
}
\description{
fix some bugs of metID::read_msp_mona, for some .msp file from MoNA database, have more than one Synon information, will cause the error: "duplicated rownames." In this version, the 1st Synon will be retained, and the rest were removed.
}
\details{
---------------------------------------------------------------------------
}
\references{
https://github.com/tidymass/metid/blob/0ee18148a383f6df1cd7464ef8642e577a8e1b70/R/read_write_msp.R by shenxt1990@outlook.com in tidymass, metID.
}
\author{
Shawn wang
\email{shawnwang2016@126.com}
}
