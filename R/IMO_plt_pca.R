#' PCA plot for massdataset.
#' @param obj mass_dataset; generated by tidymass.
#' @param tag character; MUST be one of the column names of sample_info, and case sensetive!
#' @param center logic; run ?PCAtools::pca in console.
#' @param scale logic; run ?PCAtools::pca in console.
#' @param removeVar float; run ?PCAtools::pca in console.
#' @param interactive logic; if true, generated a interactive 3D pca plot, if not, generated a 2d pca plot.
#' @param showImg logic; if true, print image on screen, if false, do not print.
#' @importFrom magrittr %>%
#' @importFrom massdataset extract_expression_data extract_sample_info
#' @importFrom tibble as_tibble column_to_rownames
#' @importFrom PCAtools pca biplot
#' @importFrom plotly plot_ly add_trace layout
#' @importFrom crayon green bold italic red yellow
#' @export
#' @return a list contains pca result and plots
#' @references https://github.com/kevinblighe/PCAtools

IMO_plt_pca = function(obj,tag,center = T,scale = T,removeVar = .1,interactive = F,showImage = T) {
  ## massages.
  msg_yes = green$bold$italic
  msg_no = red$bold$italic
  msg_warning = yellow$bold$italic
  ## run pca with PCAtools
  if(class(obj) != "mass_dataset") {
    return();
    message(msg_no("error: Only obj which generated by massdataset accepted! Please check your input." ))
  } else {
    tbl_for_pca <-
      obj %>%
      extract_expression_data()
    tbl_info <-
      obj %>%
      extract_sample_info() %>%
      as_tibble() %>%
      column_to_rownames('sample_id')
    col_tag = colnames(tbl_info)
    if (tag%in%col_tag) {
      col = tbl_info %>% select(tag) %>% pull(tag)
      obj_tbl <- PCAtools::pca(
        mat  = tbl_for_pca %>% select(rownames(tbl_info)),
        metadata = tbl_info,
        center = center,
        scale = scale,
        removeVar = removeVar
      )
      if (isTRUE(interactive)) {
        plt_pca = plot_ly() %>%
          add_trace(
            x = obj_tbl$rotated$PC1,y = obj_tbl$rotated$PC2, z = obj_tbl$rotated$PC3,
            type = 'scatter3d',mode = 'markers',color = col,hovertext = rownames(tbl_info)
          ) %>%
          layout(
            title = list(text = "3D PCA"),
            legend = list(title = list(text = tag)),
            scene = list(
              xaxis = list(title = "PC1"),
              yaxis = list(title = "PC2"),
              zaxis = list(title = "PC3")
            )
          )
      } else {
        plt_pca = biplot(
          pcaobj = obj_tbl,
          colby = tag,
          ellipse = T,
          lab = "",
          legendPosition = 'top'
        )
      }
      if (isTRUE(showImage)){
        print(plt_pca)
      }
      out_put = list(
        pca = obj_tbl,
        plot = plt_pca
      )
      return(out_put)
    } else {
      message(msg_no(paste0("error: make sure ",tag," is one of column name of sample_info in the mass_dataset, case sensetive!")))
    }
  }
}
