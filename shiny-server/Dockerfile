from rocker/shiny-verse:3.6.1

RUN R -e "install.packages(c('BiocManager'), repos='https://cran.rstudio.com')" && \
    R -e "BiocManager::install(c('shinydashboard', 'shinycssloaders', 'iheatmapr', 'plotly', 'rlist', 'shinyWidgets', 'foreach', 'viridis', 'scales', 'DT', 'data.table'))" && \
    R -e "devtools::install_github(repo='bmeg/grip', subdir='gripql/R')"

RUN rm -rf /srv/shiny-server/*
COPY ./populate_env.sh /usr/bin/populate_env.sh
CMD ["/usr/bin/populate_env.sh"]
