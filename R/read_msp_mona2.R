#' ---------------------------------------------------------------------------
#' @title read_msp_mona2
#' @description fix some bugs of metID::read_msp_mona, for some .msp file from MoNA database, have more than one Synon information, will cause the error: "duplicated rownames." In this version, the 1st Synon will be retained, and the rest were removed.
#' @author Shawn wang
#' \email{shawnwang2016@@126.com}
#' @param file The vector of names of ms2 files. MS2 file must be msp. The msp data must from MoNA.
#' @param threads The number of threads
#' @importFrom dplyr mutate group_by top_n rename ungroup
#' @importFrom magrittr %>%
#' @importFrom future plan
#' @importFrom furrr future_map
#' @importFrom readr read_lines
#' @importFrom purrr map
#' @importFrom progressr progressor with_progress
#' @return Return ms2 data. This is a list.
#' @references https://github.com/tidymass/metid/blob/0ee18148a383f6df1cd7464ef8642e577a8e1b70/R/read_write_msp.R by shenxt1990@@outlook.com in tidymass, metID.
#' @export

# sxtTools::setwd_project()
# setwd("other_files/all_ms2_database/mona/2021_6_10/")
# x = read_msp_mona2(file = "MoNA-export-LC-MS-MS_Spectra.msp")
read_msp_mona2 = function (file, threads = 3) {
  cat(crayon::green("Reading msp data from MoNA...\n"))
    msp.data <- readr::read_lines(file, progress = FALSE)
    if (tail(msp.data, 1) == "") {
      msp.data = msp.data[-length(msp.data)]
    }
    msp.data[msp.data == ""] = rep(c("END IONS", "BEGIN IONS"),
                                   sum(msp.data == "")/2)
    msp.data = c("BEGIN IONS", msp.data, "END IONS")
    begin_idx = which(msp.data == "BEGIN IONS")
    end_idx = which(msp.data == "END IONS")
    future::plan("multisession", workers = threads)
    get_msp_data = function(begin_idx,end_idx) {
      p <- progressor(steps = length(begin_idx))
      msp.data = furrr::future_map2(.x = begin_idx, .y = end_idx,
                                    .f = function(idx1, idx2) {
                                      p()
                                      Sys.sleep(.001)
                                      temp = msp.data[c(idx1:idx2)]
                                      temp = temp[temp != "BEGIN IONS"]
                                      temp = temp[temp != "END IONS"]
                                      if (length(temp) == 0) {
                                        return(NULL)
                                      }
                                      info = temp[grep(":", temp, value = FALSE)]
                                      info = stringr::str_split(info, ":", n = 2) %>%
                                        do.call(rbind, .) %>%
                                        as.data.frame() %>%
                                        dplyr::group_by(V1) %>%
                                        dplyr::top_n(1,V2) %>%
                                        ungroup %>%
                                        dplyr::rename(info = V1,  value = V2) %>%
                                        as.data.frame()
                                      spec = temp[-grep(":", temp, value = FALSE)]
                                      spec = stringr::str_split(spec, " ") %>%
                                        do.call(rbind, .) %>%
                                        as.data.frame() %>%
                                        dplyr::rename(mz = V1, intensity = V2) %>%
                                        dplyr::mutate(mz = as.numeric(mz),
                                                      intensity = as.numeric(intensity)) %>%
                                        as.data.frame()
                                      list(info = info, spec = spec)
                                    })
    }
    with_progress({
      msp.data <- get_msp_data(begin_idx = begin_idx,end_idx = end_idx)
    })
  ms2 <- msp.data
  cat(crayon::green("Done.\n"))
  ms2
}
