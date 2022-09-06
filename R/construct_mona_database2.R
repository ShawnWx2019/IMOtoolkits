##------------------------------------------------------------------------------
#' @title Construct public MS2 database from MoNA with msp format.
#' @description fix some bugs of metID::construct_mona_database2.
#' @author Shawn Wang
#' \email{shawnwang2016@@126.com}
#' @param file The file name of MassBank or MoNA database (mgf format).
#' @param only.remain.ms2 Only remain the metabolites with MS2 spectra?
#' @param path Work directory.
#' @param version The version of you database. Default is 0.0.1.
#' @param source The source of your database.
#' @param link Website link of the source.
#' @param creater Creater name. For example, Xiaotao Shen.
#' @param email email address.
#' @param rt Do the metabolites have RT information or not?. If not, set it as FALSE.
#' @param threads The number of threads
#' @importFrom magrittr %>%
#' @importFrom crayon red yellow green bgRed
#' @importFrom stringr str_detect str_extract
#' @importFrom readr cols
#' @importFrom pbapply pblapply
#' @return A databaseClass object.
#' @seealso The example and demo data of this function can be found
#' \url{https://tidymass.github.io/metid/articles/metid.html}
#' @export
construct_mona_database2 = function(
    file, only.remain.ms2 = TRUE, path = ".", version = "0.0.1",
    source = "MoNA", link = "https://mona.fiehnlab.ucdavis.edu/",
    creater = "Xiaotao Shen", email = "shenxt1990@163.com", rt = FALSE,
    threads = 5
) {
  mona_database = read_msp_mona2(file = file)

  #> Issue1: set rownames of mona_database[[i]]$info
  mona_database = purrr::map(mona_database,function(x){
    x$info = data.frame(
      row.names = x$info$info,
      value = x$info$value
    )
    db = list(info = x$info,
              spec = x$spec)
    return(db)
  })


  all_metabolite_names = purrr::map(mona_database, function(x) {
    rownames(x$info)
  }) %>% unlist() %>% unique()
  metabolite_info = mona_database %>% purrr::map(function(x) {
    x = as.data.frame(x$info)
    new_x = x[, 1]
    names(new_x) = rownames(x)
    new_x = new_x[all_metabolite_names]
    names(new_x) = all_metabolite_names
    new_x
  }) %>% do.call(rbind, .) %>% as.data.frame()
  colnames(metabolite_info) = all_metabolite_names
  if (isTRUE(only.remain.ms2)) {
    if ("MS1" %in% metabolite_info$Spectrum_type) {
      remain_idx = which(metabolite_info$Spectrum_type == "MS2")
      metabolite_info = metabolite_info[remain_idx, ]
      mona_database = mona_database[remain_idx]
    }
  }

  metabolite_info =
    metabolite_info %>%
    dplyr::select(
      Compound.name = Name,
      mz = ExactMass, Formula,
      MoNA.ID = `DB#`,
      dplyr::everything()
    )
  metabolite_info =
    metabolite_info %>%
    dplyr::mutate(
      Lab.ID = paste("MoNA", seq_len(nrow(metabolite_info)), sep = "_"),
      RT = NA,
      CAS.ID = NA,
      HMDB.ID = NA,
      KEGG.ID = NA,
      mz.pos = NA,
      mz.neg = NA,
      Submitter = "MoNA",
      Family = NA,
      Sub.pathway = NA,
      Note = NA) %>%
    dplyr::select(
      Lab.ID,
      Compound.name,
      mz,
      RT,
      CAS.ID,
      HMDB.ID,
      KEGG.ID,
      Formula,
      mz.pos,
      mz.neg,
      Submitter,
      Family,
      Sub.pathway,
      Note,
      dplyr::everything()
    )
  #> Issue2: Collision_energy
  if(!"Collision_energy"%in%colnames(metabolite_info)) {
    metabolite_info$Collision_energy = NA
  }

  metabolite_info$Collision_energy[is.na(metabolite_info$Collision_energy)] = "not_available"
  metabolite_info$Collision_energy[metabolite_info$Collision_energy == ""] = "not_available"

  #> Issue3: Ion_mode
  metabolite_info =
    metabolite_info %>%
    mutate(Ion_mode =
             case_when(
               str_detect(Ion_mode,regex("P",ignore_case = T)) ~ "P",
               str_detect(Ion_mode,regex("N",ignore_case = T)) ~ "N"
             )
    )

  positive_idx = which(metabolite_info$Ion_mode == "P")
  negative_idx = which(metabolite_info$Ion_mode == "N")
  Spectra.positive = mona_database[positive_idx]
  Spectra.negative = mona_database[negative_idx]
  names(Spectra.positive) = metabolite_info$Lab.ID[positive_idx]
  names(Spectra.negative) = metabolite_info$Lab.ID[negative_idx]
  Spectra.positive = purrr::map2(.x = Spectra.positive, .y = metabolite_info$Collision_energy[positive_idx],
                                 .f = function(x, y) {
                                   x = x$spec
                                   x = list(x)
                                   names(x) = y
                                   x
                                 })
  Spectra.negative = purrr::map2(.x = Spectra.negative, .y = metabolite_info$Collision_energy[negative_idx],
                                 .f = function(x, y) {
                                   x = x$spec
                                   x = list(x)
                                   names(x) = y
                                   x
                                 })
  database.info <- list(Version = version, Source = source,
                        Link = link, Creater = creater, Email = email, RT = rt)
  spectra.info <- as.data.frame(metabolite_info)
  rm(list = "metabolite_info")
  Spectra <- list(Spectra.positive = Spectra.positive, Spectra.negative = Spectra.negative)
  database <- new(Class = "databaseClass", database.info = database.info,
                  spectra.info = spectra.info, spectra.data = Spectra)
  database@database.info$RT <- ifelse(all(is.na(database@spectra.info$RT)),
                                      FALSE, TRUE)
  message(crayon::bgRed("All done!\n"))
  return(database)
}
