# test case works

    Code
      cat(paste0(result, collapse = "\n"))
    Output
      JOB
          MESSAGES    aermet_messages.txt
          REPORT      aermet_report.log
          CHK_SYNTAX  
          DEBUG       aermet_debug.log
      
      SURFACE
          DATA        aermet_surface.txt EXTRACT 
          LOCATION    123456 50.123 -175.123 7 10
          EXTRACT     surface.csv
          XDATES      2020/01/01 TO 2022/12/31
          QAOUT       surface.qa
          AUDIT       ALL
          RANGE       TEMP -60 <= 60 -999
          RANGE       RH 0 <= 100 -999
          NO_MISSING  RH
          ASOS1MIN    asos1min.csv
      
      UPPERAIR
          DATA        aermet_upperair.txt EXTRACT 
          LOCATION    654321 50.234 -175.234 7 
          EXTRACT     upperair.csv
          XDATES      2020/01/01 TO 2022/12/31
          QAOUT       upperair.qa
          AUDIT       ALL
          RANGE       TEMP -80 <= 60 -999
          RANGE       RH 0 <= 100 -999
          NO_MISSING  RH
          MODIFY      ALL
      
      ONSITE
          DATA      aermet_onsite_prog.txt OL
          LOCATION  234567 50.012 -175.012 7 10
          READ      1 TEMP
          READ      2 RH
          FORMAT    1 FREE
          FORMAT    2 FREE
      
      METPREP
          OUTPUT     aermet.sfc
          PROFILE    aermet.pfl
      **    --|instrument|height (m)|--
          NWS_HGT    TEMP 10
          NWS_HGT      RH 10
      ** Define Sectors: Time (t = 1-4), Space (s = 1-2)
          FREQ_SECT  SEASONAL 2 
      **          --|s|sta|end|-- (degrees)
          SECTOR     1   0 180
          SECTOR     2 180 360
      **          --|t|s|albd|bowr|srfr|--
          SITE_CHAR  1 1 0.80 0.12 2.00
          SITE_CHAR  1 2 0.20 0.18 0.80
          SITE_CHAR  2 1 0.70 0.13 1.80
          SITE_CHAR  2 2 0.25 0.17 0.90
          SITE_CHAR  3 1 0.60 0.14 1.50
          SITE_CHAR  3 2 0.30 0.16 1.00
          SITE_CHAR  4 1 0.75 0.13 1.00
          SITE_CHAR  4 2 0.22 0.18 0.85
                     

