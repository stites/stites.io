https://smyl.es/how-to-fix-ubuntudebian-apt-get-404-not-found-package-repository-errors-saucy-raring-quantal-oneiric-natty/
sprint | sed -E -e "s/'//g;s/(SEQ-[0-9 \-]+)/+\1 project:bina due:10th/g;" -l | xargs -L 1 echo task add
task add +SEQ3028 project:bina due:10th Integrate Somatic report into Portal Frontend
task add +SEQ3090 project:bina due:10th run monitor route in angular
task add +SEQ3029 project:bina due:10th Prepare somatic reports for packaging/distribution
task add +SEQ3088 project:bina due:10th statically serve angular libraries on box
task add +SEQ2978 project:bina due:10th Run jobs file browser breaks when you dismiss the modal before it loads
task add +SEQ2890 project:bina due:10th Integrate New QC to WGS Workflow
task add +SEQ3104 project:bina due:10th Fix resource graphs formating
task add +SEQ3105 project:bina due:10th Single page quality report doesnt download, file is missing
task add +SEQ3106 project:bina due:10th Changes to new output browser
task add +SEQ3107 project:bina due:10th Job object doesnt use full gray box
task add +SEQ3108 project:bina due:10th Per task detail popups dont show on status monitor page
task add +SEQ3109 project:bina due:10th Unnecessary scroll bar at bottom of QC
task add +SEQ3110 project:bina due:10th X axis labels on overlapping indel count graphs are cut off
task add +SEQ3112 project:bina due:10th Make all links use the pointer cursor (define style for ngclick )
task add +SEQ3111 project:bina due:10th fix link
task add +SEQ3089 project:bina due:10th automate embedding version numbers into UI
task add +SEQ3080 project:bina due:10th generate fastqc reports in interactive charts
task add +SEQ2913 project:bina due:10th navbar in filebrowser should handle infinitely long pathnames


for f in $(find . -type f | cut -c 3-); do
  sed -i.bak -E -e "1s/^/<script type=\"text\/template\" id=\"#$f\">/';
  echo '</script>' >> $f;
done
gs | tail -n 4 | cut -f 2 -d ':'  | head -n 2 | xargs -n 1 git checkout
