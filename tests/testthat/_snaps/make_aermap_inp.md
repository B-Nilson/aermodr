# test case works

    Code
      cat(paste0(result, collapse = "\n"))
    Output
      CO STARTING
         TITLEONE  AERMAP Run
         DATATYPE  DEM 
         DATAFILE  "aermap.dem"  
         ANCHORXY  0 0  500000 5400000 10 0
         RUNORNOT  RUN
      CO FINISHED
      
      SO STARTING
         LOCATION  S1 POINT 50 50 10
         LOCATION  S2 POINT 150 150 15
      SO FINISHED
      
      RE STARTING
         DISCCART  0 0  
         DISCCART  100 100  
         DISCCART  200 200  
      RE FINISHED
      
      OU STARTING
         RECEPTOR  "aermap.rec"
         SOURCLOC  "aermap.src"
      OU FINISHED

