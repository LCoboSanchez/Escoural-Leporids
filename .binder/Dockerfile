rrtools::use_analysis()FROM rocker/binder:4.2.2
LABEL maintainer='Lucía Cobo-Sánchez <lcsanchez@ualg.pt>'
USER root
COPY . ${HOME}
RUN chown -R ${NB_USER} ${HOME}
USER ${NB_USER}

RUN wget https://github.com/LCoboSanchez/Escoural-Leporids/raw/master/DESCRIPTION && R --vanilla -s -e "options(repos = list(CRAN = 'http://cran.rstudio.com'));  install.packages('renv'); renv::restore()"

RUN rm DESCRIPTION.1; exit 0
