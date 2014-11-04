# Puts both Lawmakers datasets into one CSV file
# http://www.thelawmakers.org/

setwd("~/Documents/Blog/Living-in-the-Sprawl/Lawmakers")

library("dplyr", lib.loc="/Library/Frameworks/R.framework/Versions/3.0/Resources/library")

law1 <- read.csv("LEPData93to110Congresses.csv")
# dim: [1] 8017   54
law2 <- read.csv("LEPData111to112Congresses.csv")
# dim: [1] 900  38

intersect(colnames(law1),colnames(law2)) # same columns between law1 and law2
# [1] "thomas_num"  "thomas_name" "icpsr"       "congress"   
# [5] "year"        "st_name"     "cd"          "dem"        
# [9] "majority"    "elected"     "female"      "votepct"    
# [13] "dwnom1"      "deleg_size"  "speaker"     "chair"      
# [17] "subchr"      "power"       "budget"      "seniority"  
# [21] "maj_leader"  "min_leader"  "ss_bills"    "ss_aic"     
# [25] "ss_abc"      "ss_pass"     "ss_law"      "s_bills"    
# [29] "s_aic"       "s_abc"       "s_pass"      "s_law"      
# [33] "c_bills"     "c_aic"       "c_abc"       "c_pass"     
# [37] "c_law"       "les" 

law <- rbind_all(list(law1,law2))

write.csv(law, file="lawmakers.csv")