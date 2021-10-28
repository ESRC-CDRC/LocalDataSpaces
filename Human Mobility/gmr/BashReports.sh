#!/usr/bin/bash
cd '/media/maurizio/BETA_Seagate/work/UCL/Local Data Spaces/LDS_data_package/LA/gmr/'
declare -a arrayChartfiles=`(find . -type f -name 1.charts.preparation.R|grep -v 'commonfiles')`
declare -a arrayRmdfiles=`(find . -type f -name 2.knit.report.R|grep -v 'commonfiles')`
for i in "${arrayChartfiles[@]}"; do
    #cd dirname "${chartfile}"
    VAR="${i}"
    echo $VAR
    DIR=${VAR%/*}
    echo $DIR
    cd "$DIR"
    #Rscript "${VAR##*/}"
    #Rscript "${chartfile}"
    done
    
    
    &&
for rmdfile in "${arrayRmdfiles[@]}"; do
    #cd dirname "${chartfile}"
    VAR="${rmdfile}"
    #echo $VAR
    DIR=${VAR%/*}
    #echo $DIR
    cd $DIR
    Rscript "${VAR##*/}" \;
    rm -rf Rplots.pdf
    #Rscript "${chartfile}"
    done

for dir in ./*;
  do 
     [ -d "$dir" ] && cd "$dir" && echo "Entering into $dir and installing packages"
  done;