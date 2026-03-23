# test case works

    Code
      cat(paste0(result, collapse = "\n"))
    Output
      JOB
          MESSAGES    aermet_messages.txt
          REPORT      report.aermet
          CHK_SYNTAX  
          NOPRINT     
          DEBUG       debug.aermet
      
      SURFACE
          DATA        aermet.sfc EXTRACT 
          LOCATION    123456 50.123 -175.123 7 10
          EXTRACT     surface.csv
          XDATES      2020/01/01 TO 2022/12/31
          QAOUT       surface.qa
          AUDIT       ALL
          RANGE       TEMP -30 <= 50 -999
          NO_MISSING  TEMP
          ASOS1MIN    asos1min.csv
      
      UPPERAIR
          DATA        aermet.upr EXTRACT 
          LOCATION    654321 50.234 -175.234 7 
          EXTRACT     upperair.csv
          XDATES      2020/01/01 TO 2022/12/31
          QAOUT       surface.qa
          AUDIT       ALL
          RANGE       TEMP -30 <= 50 -999
          NO_MISSING  TEMP
          MODIFY      ALL
      
      ONSITE
          DATA      aermet.ons OL
          LOCATION  234567 50.012 -175.012 7 10
          READ      1 TEMP
          FORMAT    1 FREE
      
      METPREP
          NWS_HGT    A 10
          NWS_HGT    B 10
          NWS_HGT    C 10
          NWS_HGT    D 10
          NWS_HGT    E 10
          NWS_HGT    F 10
          SECTOR     1 0 45
          SECTOR     2 45 90
          SECTOR     3 90 135
          SECTOR     4 135 180
          SECTOR     5 180 225
          SECTOR     6 225 270
          SECTOR     7 270 315
          SECTOR     8 315 0
          SECTOR     9 360 45
          FREQ_SECT  ANNUAL 1 2020 2021 2022
          SITE_CHAR  1 1 0.2 4 1
          OUTPUT     aermet.out
          PROFILE    aermet.pfl

