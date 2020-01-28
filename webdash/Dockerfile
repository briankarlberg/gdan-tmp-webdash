from rocker/shiny-verse:3.6.1

RUN R -e "install.packages(c('BiocManager'), repos='https://cran.rstudio.com')" && \
    R -e "BiocManager::install(c('shinydashboard', 'shinycssloaders', 'iheatmapr', 'plotly', 'rlist', 'shinyWidgets', 'foreach', 'viridis', 'scales', 'DT'))" && \
    R -e "devtools::install_github(repo='bmeg/grip', subdir='gripql/R')"
