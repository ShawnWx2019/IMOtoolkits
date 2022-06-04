library(shiny)
library(bslib)
library(bs4Dash)
library(shinyjqui)
library(thematic)
x1_tmp <- read.csv(
  '~/My_Repo/F-box/02.metadata/exp_mat.csv'
  )
x2_tmp <- read.csv(
  '~/My_Repo/F-box/02.metadata/sample_info2.csv'
)
imo_demo_data<- list(
  exp_mat = x1_tmp,
  sample_info = x2_tmp
)


# homepage ----------------------------------------------------------------

homepage <- tabPanel(
  shinyjs::useShinyjs(),
  title = 'Home page',
  icon = icon('home'),
  fluidRow(
    ## Introduction
    column(6,offset = .5,
           class = "p-3 border", class =  "rounded",
           HTML(
             "
          <div id = 'header' style='text-align:center;'>
          <h1>What can I do ?</h1>
          </div>
          <hr style='border-top: dotted 4px green;'> </hr>
          <div id = 'content' style='float:left'>
          This app is developed for downstream data analysis and visualization of <font style='bold' color = 'green'>Untargeted metabolomics, pseudo-targeted metabolomics or widely targeted metabolomics</font> data based on LC-MS (GC-MS).
          <h3> Data cleaning process </h3>
          Based on <a href='https://www.tidymass.org/'>Tidymass</a> and <a href='https://slfan2013.github.io/SERRF-online/'>SERRF</a>

          <ol>
          <li> Missing value imputation.<br></li>
          <li> Sample outlier detection.<br></li>
          <li> Batch effect alignment.<br></li>
          <li> QC based normalization.<br></li>
          </ol>

          <h3> Pathway analysis </h3>
          Based on <a href='https://www.tidymass.org/'>Tidymass</a> and <a href='https://guangchuangyu.github.io/software/clusterProfiler/'> ClusterProfiler</a>
          <ol>
          <li> KEGG pathway analysis.<br></li>
          <li> Plantcyc pathway analysis.<br></li>
          </ol>

          <h3> Visualization </h3>
          Based on <a href='https://jokergoo.github.io/ComplexHeatmap-reference/book/'>ComplexHeatmap</a> and <a href='https://bioconductor.org/packages/devel/bioc/vignettes/PCAtools/inst/doc/PCAtools.html'>PCAtools</a>
          <ol>
          <li> Principal component analysis (PCA) of each data cleaning stage.<br></li>
          <li> RSD before and after normalization.<br></li>
          <li> Interactive heatmap of total/difference accumulated metabolites.<br></li>
          <li> KEGG pathway bar/dot plot.<br></li>
          <li> Heatmap of metabolites belongs to interested Pathway.<br></li>
          </ol>

          <h3> Output tables </h3>
          Based on <a href='https://www.tidymass.org/'>Tidymass</a> and <a href='https://guangchuangyu.github.io/software/clusterProfiler/'> ClusterProfiler</a>
          <ol>
          <li> Metabolites accumulation profile matrix for WGCNA.<br></li>
          <li> Pathway annotation result.. <br></li>
          <li> Pathway analysis result. <br></li>

          </ol>

          <h3> Todo list </h3>
          <ol>
          <li> Different accumulation analysis.<br></li>
          <li> More method of interactive pathway analysis visulization <br></li>
          <li> ... <br></li>

          </ol>
          </div>
          "
           )
    ),
    column(6,offset = .5,
           class = "p-3 border", class =  "rounded",
           HTML(
             "
          <div id = 'header' style='text-align:center;'>
          <h1>Brief structure </h1>
          </div>
          <hr style='--dashed-filled:6px;size:5px --dashed-open:5px;color=red;'> </hr>
          <br>
          <br>
          <br>
          "
           ),
          shinyjqui::jqui_resizable(
            img(
              src = "https://shawnmagic-1257599720.cos.ap-chengdu.myqcloud.com/MDAtoolkits.svg",
              height = 300,
              weight = 800
            )
          )
    )
  )
)

# Demo panel ---------------------------

Demo<- tabPanel(
  shinyjs::useShinyjs(),
  thematic::thematic_shiny(),
  title = 'Check Input',
  icon = icon('tasks'),
  tabsetPanel(
    tabPanel(
      title = 'Description',
      icon = icon('book'),

      fluidRow(
        column(4,
               class = "p-3 border", class =  "rounded",
      HTML(
          "
          <div id = 'header' style='text-align:center;'>
          <h1>Accumulation matrix! </h1>
          </div>
          <hr style='border-top: dotted 4px green;'> </hr>
          <div id = 'content' style='float:left'>
          Two tables <font color=red>MUST</font> be provided, one is the metabolite peak area matrix, and the other is the sample information.


          </div>
          "
          )
          ),
      column(8,
             DT::dataTableOutput('demo_exp_mat')
      )
      ),
      fluidRow(
        column(4,
               class = "p-3 border", class =  "rounded",
               HTML(
                 "
          <div id = 'header' style='text-align:center;'>
          <h1>Data preparing! </h1>
          </div>
          <hr style='border-top: dotted 4px green;'> </hr>
          <div id = 'content' style='float:left'>
          Two tables <font color=red>MUST</font> be provided, one is the metabolite peak area matrix, and the other is the sample information.
          </div>
          "
               )
        ),
        column(8,
               DT::dataTableOutput('demo_sample_info')
        )
      ),
    ),
    tabPanel(
      title = 'Demo',
      icon = icon('file')
    ),
    tabPanel(
      title = 'Upload your data',
      icon = icon("upload")
    ),
    tabPanel(
      title = 'Preview',
      icon = icon("check")
    )
  )
)


# ui ----------------------------------------------------------------------


ui <- navbarPage(
  theme = bslib::bs_theme(version=4),
  title = div(
    tags$img(
      src="https://shawnmagic-1257599720.cos.ap-chengdu.myqcloud.com/picgo/202205301016150.png",
      height = '60',
      weigh = '60',
      style = 'margin:20px 20px"'
    ),
    HTML(paste(tags$strong(style="font-size:25px;", "Metabolomics"),
               tags$span(style="font-size:12px;class:italy;color:green","v 0.0.1.2205"))),
  collapsible = T,
  id = 'navbar'),
  homepage,
  Demo
 )


# server ------------------------------------------------------------------


server <- function(input, output, session) {
  bslib::bs_themer()
  # Your application server logic
  output$demo_exp_mat = DT::renderDataTable(
    imo_demo_data$exp_mat,
    extensions = 'Buttons',
    options = list(
      dom = 'Bfrtip',
      deferRender = TRUE,
      scrollY = 400,
      scrollX = T,
      scroller = TRUE,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ),
    class = 'display'
  )
  output$demo_sample_info = DT::renderDataTable(
    imo_demo_data$sample_info,
    extensions = 'Buttons',
    options = list(
      dom = 'Bfrtip',
      deferRender = TRUE,
      scrollY = 400,
      scrollX = T,
      scroller = TRUE,
      buttons = c('copy', 'csv', 'excel', 'pdf', 'print')
    ),
    class = 'display'
  )
}

shinyApp(ui,server)
