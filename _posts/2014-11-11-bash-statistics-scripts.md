---
layout: post
title: Various Scripts for Bash Statistics
---

{{ page.title }}
================

<p class="meta">11 Nov 2014 - Redwood City</p>

    =================
    $ sort -n allPass.qual | uniq -c > tmp
    $ sed -n '$=' tmp
    17917


    cut -f 3 test.sorted.gatkDP.sample_interval_summary
    tab is default delimninator


    #too long:
    $ sed -i.bak -E -e 's/^([0-9]+)$/\1\.0/g;s/([0-9]+\.[0-9])[0-9]+/\1/g' allPass.qual
    $ sort -n allPass.qual | uniq -c > tmp
    $ sed -i.bak -E 's/([0-9]+) (-?[0-9]+\.?[0-9]*)/\[\2\,\1\]\,/g' tmp
    $ sed -n '$=' tmp
    5335
    $ cat tmp > allPass.qual
    $ echo ']' >> allPass.qual
    $ echo '[' > tmp
    $ cat allPass.qual >> tmp
    $ cat tmp > allPass.qual
    $ rm tmp

    ## ------------------
    find . -type f > a.txt
    while read file; do if [[ "$file" = */* ]]; then mkdir -p "${file%/*}"; fi; touch "$file"; done < a.txt
    ## ------------------
    sed -i.bak -E -e 's/8ab89ba0-demo/8ab89ba0-0c23-4ef1-8f13-c4e1c5deda4d/g;' job.json
    function job {
      # grep a UUID: grep -Ei '[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}'
      binaPwd=$(cd .. && pwd)
      currentJobId=$(pwd | cut -c $((${#binaPwd}+2))-)
      newJobId=$1
      cd ..
      mv $currentJobId $newJobId
      cd $_
      sed -i.bak -E "s/$currentJobId/$newJobId/g;" job.json
    }
    job 8ab89ba0-0000-1111-2222-333333333333


    for vid in 01_Introduction 02_Omit_Needless_Names 03_Separating_Mutation_from_Calculation 04_Recognize_Pure_Function 05_Separate_Functions_from_Rules 06_Currying_Exercise 07_Currying_Exercise_Hints 08_Currying_Exercise_Solutions_1_and_2; do
      num=${vid%%[a-zA-Z_]*}
      mv hc_fjs_${num}.mp4 ${vid}.mp4;
    done



    tput bel
    ps aux | grep grunt | sed -E  "s/`whoami` +([0-9]+).*/\1/" | xargs -n 1 echo
