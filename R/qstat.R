##' @import data.table


if(F){
  ## example - not run
  source("/data/home/delff/wdirs/suitcase/R/qstat.R")
  dtj <- qstat()
  dtj[,.N,by=state]
  dtj[state=="qw",.N,.()]


}

qstat <- function(n=1,sleep=5){

  library(data.table)

  qstatOne <- function(){

    # 1. Capture the output as a text vector
    raw_output <- system2("qstat", args = "-f", stdout = TRUE)

    # 2. Filter out decorative line fragments and headers
    # Removes empty rows, delimiter lines, and the "PENDING JOBS" header
    clean_lines <- raw_output[
      raw_output != "" & 
        !grepl("^#", trimws(raw_output)) & 
        !grepl("PENDING JOBS", raw_output) &
        !grepl("^\\-+$",raw_output) & 
        !grepl("^queuename",raw_output) & 
        !grepl("^.+@ip\\-",raw_output) 
    ]

    #### search for something like this to find open nodes
    ## all.q@ip-10-134-158-40.ec2.int BIP   0/16/16        0.86     lx-amd64 

    if(!length(clean_lines)) {
      message("No jobs found.")
      return(invisible(NULL))
    }

    # 3. Read the cleaned space-separated rows into a data.table
    # fread handles varying amounts of whitespace automatically
    dt_jobs <- fread(
      text = paste(clean_lines, collapse = "\n"), 
      header = FALSE,
      fill = TRUE
    )

    # 4. Standardize and assign the correct column names
    # Columns match the standard SGE/UGE qstat layout
    setnames(dt_jobs, c("job_id", "prior", "name", "user", "state", "submit_date", "submit_time", "slots"))

    # 5. Clean up types (Ensure job_id is stored cleanly as an integer)
    dt_jobs[, job_id := as.integer(job_id)]

    ## collapse info by jobnumber 
    dt_jobs <- dt_jobs[,.(slots=sum(slots),nodes=.N),by=.(job_id,prior,name,user,state,submit_date,submit_time)]

    # 6. Parse submission date and time into a POSIXct column
    dt_jobs[, submit_ptime := as.POSIXct(paste(submit_date, submit_time), format = "%m/%d/%Y %H:%M:%S")]

    dt_jobs[,time_elapsed := as.numeric(Sys.time()- submit_ptime,units="mins")]
    ## dt_summary <- dt_jobs[,.(.N,
    ##                          sprintf("%5.2f (%5.2f-%5.2f)",
    ##                                  median(time_elapsed),min(time_elapsed),max(time_elapsed))),by="state"]

    

    ### todo include number of slots, median+range
    
    dt_summary <- dt_jobs[,.(.N,
                             slots.m=median(slots),slots.min=min(slots),slots.max=max(slots),
                             median=median(time_elapsed),min=min(time_elapsed),max=max(time_elapsed))
                         ,by=c("state")]

    dt_summary[,slots.c := sprintf("%d (%d-%d)",slots.m,slots.min,slots.max)]

    dts.l <- melt(dt_summary,measure.vars=c("median","min","max"))
    dts.l <- dts.l[,value.c := round(value,2),by=variable]
    dts.w <- dcast(dts.l,state+N+slots.c~variable, value.var="value.c")
    
    dts.w[,elapsed := sprintf("%s (%s-%s)",
                              format_secs(median*60),format_secs(min*60),format_secs(max*60))]

    NMdata:::message_dt(dts.w [,.(state,N,slots.c,elapsed)])

    invisible(dt_jobs[])
  }

  if(n==1) sleep <- 0
  for(i in 1:n){
    qstatOne()
    Sys.sleep(sleep)
  }
}
