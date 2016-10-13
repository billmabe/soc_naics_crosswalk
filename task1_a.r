setwd("G:\\NSFY\\Tasks1_2\\")
require(readr)
require(dplyr)
require(tidyr)
require(reshape2)
require(stringr) #to pad CIP with leading zeros; neither formatC nor sprintf worked properly on this list
# BUT: Beware of the possible scientific notation issue mentioned in this SO post:
#http://stackoverflow.com/questions/5812493/adding-leading-zeros-using-r
require(ggplot2)
library(WriteXLS)

# 1. READ IN DATA

# A. Read industry occupation data
io <- read_csv("IO_Data_Table_all.csv", col_types = list(
	"Projected Employment" = col_character(),	#Figure out why these 2 lines of code are needed. Hint trying running this program with out them.
	"Estimated Employment" = col_character()
	))
str(io)
dim(io)
colnames(io) <- c("state","naics","industry","soc","occupation","period","estEmp","projEmp","pctAllJobsinInd","pctAllJobsinOcc")
io_soc <- io %>% 
	group_by(soc) %>%
		slice(1:1) %>%
		select(soc, occupation)
dim(io_soc)

# B. Read in cip - 2, 3, and 4 digit soc files as a list and all related employment and wage info

### The CIP - SOC 2, 3, and 4 digit data are not clean. Many data read in pitfalls.
# 1. CIP values are missing leading zeros.
# 2. There are asterisks in the file. These are difficult to handle as NA
# 3. Numbers that are > 999 have a comma separator in them and they are in quotes.
# R reads these records as character. The commas have to be removed using regular expressions
# and then the variables have to be converted to numeric.
# 4. The variables are inconsistently named, which is bad practice.
# I would think readr could handle this better by explicitly specifying na 
# values, but it does not seem to be doing that so well.
# 5. These issues and the cleaning required to handle them see to be causing R
# to think that the names attribute for cip_soc_n is NULL, which is a bad thing.

filenames <- list.files("G:\\NSFY\\Tasks1_2\\CIPSOC\\", pattern = "*.csv")
cip_soc_n <- lapply(filenames, function(x) {
				read_csv(x, na = c("", "NA"), col_types = list(
				CIP = col_character(),
				PS_Participant_UNDECLARED_CTE_MAJOR_13 = col_character(),
				PS_Participant_DECLARED_CTE_MAJOR_13 = col_character(),
				PS_Concentrator_13 = col_character(),
				PS_Completer_13 = col_character(),
				PS_Participant_UNDECLARED_CTE_MAJOR_14 = col_character(),
				PS_Participant_DECLARED_CTE_MAJOR_14 = col_character(),
				PS_Concentrator_14 = col_character(),
				PS_Completer_14 = col_character(),
				PS_Participant_UNDECLARED_CTE_MAJOR_15 = col_character(),
				PS_.Participant_DECLARED_CTE_MAJOR_15 = col_character(),
				PS_Concentrator_15 = col_character(),
				PS_Completer_15 = col_character()
				))}
				)
#head(cip_soc_n[[1]],50) ## NOTE: leading zeros are missing from the cipcodes

### Add leading zeros to CIP
cip_soc_n <- lapply(cip_soc_n, function(x) {
			data.frame(CIP = str_pad(x$CIP, 6, pad = "0"), x[, -1], stringsAsFactors = FALSE)
			})
names(cip_soc_n) <- c("soc2","soc3","soc4")

WriteXLS("cip_soc_n", "cip_soc_234.xlsx",
col.names = TRUE, AdjWidth = TRUE, BoldHeaderRow = TRUE, FreezeRow = 1)


head(cip_soc_n[[1]])
table(cip_soc_n[[1]]$CIP)
table(cip_soc_n[[1]]$PS_Concentrator_14)
table(is.na(cip_soc_n[[1]]$PS_Concentrator_14 == TRUE)) ## Number of Missing values for PS_Concentrator

cip_soc_n <- lapply(cip_soc_n, function(x) {
					data.frame(
						x[,1:10], 
						PS_Concentrator_14 = gsub( ",", "", x$PS_Concentrator_14),
						stringsAsFactors = FALSE
						)
					})
head(cip_soc_n[[1]])
table(cip_soc_n[[1]]$CIP)
table(cip_soc_n[[1]]$PS_Concentrator_14)
table(is.na(cip_soc_n[[1]]$PS_Concentrator_14 == TRUE)) ## NUmber of Missing values for PS_Concentrator

cip_soc_n[[1]][,11] <- as.integer(unlist(cip_soc_n[[1]][,11]))
cip_soc_n[[2]][,11] <- as.integer(unlist(cip_soc_n[[2]][,11]))
cip_soc_n[[3]][,11] <- as.integer(unlist(cip_soc_n[[3]][,11]))

str(cip_soc_n[[1]])
table(cip_soc_n[[1]]$CIP)
table(cip_soc_n[[1]]$PS_Concentrator_14)
table(is.na(cip_soc_n[[1]]$PS_Concentrator_14 == TRUE)) ## NUmber of Missing values for PS_Concentrator

cip_soc_n <- lapply(cip_soc_n, function(x) x %>% filter(substr(SOC, 3, 6) != '0000'))

### Create list of only programs that have wages >= $12.64 / hour
cip_soc_n_1264 <- lapply(cip_soc_n, function(x) {
							x[x$H_PCT10 >= 12.64, ]
							})

#head(cip_soc_n[[1]]) ## problem resolved



###---------------------------------------------------------------------------------------------------------------


### 2. CLEAN AND MANAGE THE DATA 

# A. Clean the io data

### (i) add column names
colnames(io) <- c("state","naics","industry","soc","occupation","period","estEmp","projEmp","pctAllJobsinInd","pctAllJobsinOcc")
#colnames(io) <- c(naics,industry,soc,occupation,period,estEmp,projEmp) example for class
#2nd line of code is wrong because it tries to assign OBJECTS to create a vector of names for io.

###-------------

### (ii) Remove the commas before trying to convert to numeric
#Show with and without name changes.
io <- data.frame(
	io[,2:5], 
	estEmp = gsub( ",", "", io$estEmp), 
	projEmp = gsub( ",", "", io$projEmp), 
	io[,9:10],stringsAsFactors = FALSE)

###-------------

### (iii) Convert estEmp and projEmp to integer -- for class: try doing the below first (before removing the commas)
io$estEmp <- as.integer(io$estEmp)
io$projEmp <- as.integer(io$projEmp)
str(io)

#All in one line:
#io <- data.frame(io[,1:5], estEmp = as.integer(gsub( ",", "", io$estEmp)), projEmp = as.integer(gsub( ",", "", io$projEmp)), stringsAsFactors = FALSE)

### (iv) Liswise delete

dim(io)
io <- io[complete.cases(io) == TRUE, ]
dim(io)

###-------------

### (v) Remove industry - occupations with employment < 10

#io_gt10 <- io[estEmp > 10, ] returns an error, because there is no object named estEmp, only io$estEmp
io_gt10 <- io[io$projEmp > 10, ]
io_gt10$kis <- ifelse(substr(io_gt10$naics,1,2) == '23', "Construction",
				ifelse(substr(io_gt10$naics,1,2) %in% c('31','32','33'), "Manufacturing",
				ifelse(substr(io_gt10$naics,1,2) %in% c('48','49'), "TLD",
				ifelse(substr(io_gt10$naics,1,2) == '51', "Information",
				ifelse(substr(io_gt10$naics,1,2) == '52', "Finance",
				ifelse(substr(io_gt10$naics,1,2) == '62', "Healthcare",
				ifelse(substr(io_gt10$naics,1,2) == '72', "Hospitality", 
				ifelse(substr(io_gt10$naics,1,3) %in% c('334','339') | substr(io_gt10$naics,1,4) %in% c('5413','5416','5417','5419','621'), "Life Sciences","Non-Key Industry"
				))))))))
io_gt10$kis_or_not <- ifelse(io_gt10$kis == "Non-Key Industry", "Not Key Industry", "Key Industry")
#io_gt10 <- io_gt10[io_gt10$kis != "", ] totals by occupation are not global; they are just within the 7 key industries.
table(io_gt10$kis_or_not)
dim(io_gt10)

###-------------

### (vi) Create new variables -- 

#Jobs in Occupation X in Industry Y as Percent of All Jobs Occupation X | We're now able to compare % employed in occ across industries

soc_emp_in_occind <-
	io_gt10 %>%
		group_by(kis,soc) %>% #employment in each industry + occupation combination
			summarize(
			nbr_of_jobs_in_occind = sum(projEmp)
			)

soc_emp_in_occ <- 
	soc_emp_in_occind %>% 
		group_by(soc) %>%  #employment in each occupation
			mutate(
			nbr_of_jobs_in_occ = sum(nbr_of_jobs_in_occind)
			)

soc_emp_in_ind <- 			
	soc_emp_in_occ %>%
		group_by(kis) %>%  #employment in each industry
			mutate(nbr_of_jobs_in_ind = sum(nbr_of_jobs_in_occind))%>%
			mutate(pct_jobs_in_occ = (nbr_of_jobs_in_occind/nbr_of_jobs_in_occ),
			pct_jobs_in_ind = (nbr_of_jobs_in_occind/nbr_of_jobs_in_ind))%>%
			mutate(max_pct_of_all_jobs_in_occ = max(pct_jobs_in_occ),
			max_pct_of_all_jobs_in_ind = max(pct_jobs_in_ind)) %>%
			mutate(percent_of_max_jobs_in_occ = round((pct_jobs_in_occ/max_pct_of_all_jobs_in_occ)*1000),
			percent_of_max_jobs_in_ind = round((pct_jobs_in_ind/max_pct_of_all_jobs_in_ind)*1000))%>%
			mutate(percent_of_jobs = ifelse(percent_of_max_jobs_in_occ >= percent_of_max_jobs_in_ind, percent_of_max_jobs_in_occ, percent_of_max_jobs_in_ind))
#### May be able to do a different calculation, based % jobs in occupation

### Check distribution of max jobs in each occ and each ind to make sure they are evenly distributed
#table(soc_emp_in_ind$kis, soc_emp_in_ind$max_pct_of_all_jobs_in_ind)
#table(soc_emp_in_ind$soc, soc_emp_in_ind$max_pct_of_all_jobs_in_occ)

#Needs to be done by industry!! :
socind_list <- list()
for(j in soc_emp_in_ind$kis) {
	socind_list[[j]] <- soc_emp_in_ind[soc_emp_in_ind$kis == j, ]
	}
str(socind_list)

### SET Thresholds
## Could NOT figure out how to do this as a nested list: si_list <- lapply(socind_list, lapply, function(x) {
Construction <- list()
for(i in 1:1000) {
	Construction[[i]] <- data.frame(socind_list[[1]][socind_list[[1]]$percent_of_jobs >= i, ], threshold = i)
}
Finance <- list()
for(i in 1:1000) {
	Finance[[i]] <- data.frame(socind_list[[2]][socind_list[[2]]$percent_of_jobs >= i, ], threshold = i)
}
Healthcare <- list()
for(i in 1:1000) {
	Healthcare[[i]] <- data.frame(socind_list[[3]][socind_list[[3]]$percent_of_jobs >= i, ], threshold = i)
}
Hospitality <- list()
for(i in 1:1000) {
	Hospitality[[i]] <- data.frame(socind_list[[4]][socind_list[[4]]$percent_of_jobs >= i, ], threshold = i)
}
Information <- list()
for(i in 1:1000) {
	Information[[i]] <- data.frame(socind_list[[5]][socind_list[[5]]$percent_of_jobs >= i, ], threshold = i)
}
LifeSciences <- list()
for(i in 1:1000) {
	LifeSciences[[i]] <- data.frame(socind_list[[6]][socind_list[[6]]$percent_of_jobs >= i, ], threshold = i)
}
Manufacturing <- list()
for(i in 1:1000) {
	Manufacturing[[i]] <- data.frame(socind_list[[7]][socind_list[[7]]$percent_of_jobs >= i, ], threshold = i)
}
NonKeyIndustry <- list()
for(i in 1:1000) {
	NonKeyIndustry[[i]] <- data.frame(socind_list[[8]][socind_list[[8]]$percent_of_jobs >= i, ], threshold = i)
}
TLD <- list()
for(i in 1:1000) {
	TLD[[i]] <- data.frame(socind_list[[9]][socind_list[[9]]$percent_of_jobs >= i, ], threshold = i)
}

all_10 <- list(Construction[[100]], Finance[[100]], Healthcare[[100]], Hospitality[[100]], 
Information[[100]], LifeSciences[[100]], Manufacturing[[100]], TLD[[100]])

all_25 <- list(Construction[[250]], Finance[[250]], Healthcare[[250]], Hospitality[[250]], 
Information[[250]], LifeSciences[[250]], Manufacturing[[250]], TLD[[250]])

all_37.5 <- list(Construction[[375]], Finance[[375]], Healthcare[[375]], Hospitality[[375]], 
Information[[375]], LifeSciences[[375]], Manufacturing[[375]], TLD[[375]])

all_10_soc <- lapply(all_10, function(x) merge(x, io_soc, by = "soc"))
all_25_soc <- lapply(all_25, function(x) merge(x, io_soc, by = "soc"))
all_37.5_soc <- lapply(all_37.5, function(x) merge(x, io_soc, by = "soc"))

all_10_df <- do.call(rbind, all_10_soc)
all_25_df <- do.call(rbind, all_25_soc)
all_37.5_df <- do.call(rbind, all_37.5_soc)

write.csv(all_10_df, file = "all_10_soc.csv", row.names = FALSE)
write.csv(all_25_df, file = "all_25_soc.csv", row.names = FALSE)
write.csv(all_37.5_df, file = "all_375_soc.csv", row.names = FALSE)


### I now have nine lists -- one per key industry sector plus on for all industries combined that are not key industries.

curve_list <- list(Construction, Finance, Healthcare, Hospitality, Information, LifeSciences, Manufacturing, TLD, NonKeyIndustry)

#Remove NULL list elements
curve_list2 <- curve_list[!sapply(curve_list, is.null)]
head(curve_list2[[8]][[600]])

#### Now I need to use SOC to get the CIP and the number of students enrolled from the enrollment file.

#Prepare curve_list2 for merge with CIP - three-digit SOC file.
#Add a SOC.3 variable to curve_list2
curve_list2 <- lapply(curve_list2, lapply, function(x) {
									data.frame(x, SOC.3 = substr(x$soc, 1, 3), stringsAsFactors = FALSE)})
head(curve_list2[[8]][[600]])

### PREPARE THE cip_soc_n data to be merged with the curve_list data ------------------------------------------

### Create a list of the cip-soc3_all and cip-soc3_1264
cip_soc_3 <- list(cip_soc_n[[2]][, c(1,2,4,5,6)],cip_soc_n_1264[[2]][, c(1,2,4,5,6)])
#Recode SOC.3 from integer to character
cip_soc_3 <- lapply(cip_soc_3, function(x) data.frame(x[1:3], SOC.3 = as.character(x[,4]), x[5], stringsAsFactors = FALSE))
###The lapply initially screwed up SOC.3.  Note the subtle difference in how I have to refer to the 4th variable versus the 5th variable!!!
names(cip_soc_3) <- c("all","living_wage")
str(cip_soc_3)
# Create a string variable that is a concatenation of CIP and SOC.3
cip_soc_3_by_cipsoc3 <- lapply(cip_soc_3, function(x) data.frame(x, cipsoc3 = paste0(x$CIP, x$SOC.3), stringsAsFactors = FALSE))
# De-duplicate by the concatenation of CIP and SOC.3
cip_soc_3_by_cipsoc3 <- lapply(cip_soc_3_by_cipsoc3, function(x) x[!duplicated(x[c("cipsoc3")]),])
str(cip_soc_3_by_cipsoc3)
#Create a cip_soc_3_unique date frame to calculate total enrollment by CIP
cip_soc_3_unique <- lapply(cip_soc_3, function(x) x[!duplicated(x[c("CIP")]),])
dim(cip_soc_3_unique)
str(cip_soc_3_by_cipsoc3)
### Get a correct total of all secondary students enrolled in CTE in 2013 - 2014
cip_soc_3_tot <- lapply(cip_soc_3_unique, function(x) {
	x %>%
		mutate(total_enrollment = sum(S_Students_14, na.rm = TRUE))%>%
		select(CIP, total_enrollment)
		})
str(cip_soc_3_tot)
##Use lapply to merge a list with a data frame (Not Map or mapply!)
cip_soc_3_merge <- lapply(cip_soc_3_by_cipsoc3, function(x) left_join(x, cip_soc_3_tot$all, by = "CIP"))
str(cip_soc_3_merge)
str(cip_soc_3_by_cipsoc3) 

cip_soc_3_merge_all <- left_join(cip_soc_3_merge[[1]], cip_soc_3_merge[[2]], by = c("CIP","SOC.3"))%>%
						select(CIP, S_Students_14.x, SOC.3, SOC.x, total_enrollment.x, S_Students_14.y)
str(cip_soc_3_merge_all)

curve_list3 <- lapply(curve_list2, lapply, function(x) merge(x, cip_soc_3_merge_all, by = "SOC.3"))

head(curve_list2[[7]][[400]],30)
head(curve_list3[[7]][[400]],30)
str(curve_list3[[7]][[400]])

curve_list4 <- lapply(curve_list3, lapply, function(x) x[!duplicated(x[c(15)]),])

###-----------------------------------------------------------------------------------------------------------

### DETOUR: Create list of CIP - SOC - NAICS for DOE
#a <- do.call(rbind, curve_list4)
#b <- do.call(rbind, a)
#d <- b[b$threshold %% 10 == 0, c(1,2,3, 13:15)]
#Convert threshold and percent of jobs to percent:
#d$threshold <- d$threshold/10 
#d$percent_of_jobs <- d$percent_of_jobs/10

#d10 <- d[d$threshold == 10, ]
#d25 <- d[d$threshold == 25, ]
#d37 <- d[d$threshold == 37, ]


###-------------------------------------------------------------------------------------------------------------
### INSERT THRESHOLD IN LINE BELOW
#cip_industry_at_agreed_threshold <- d[d$threshold >                ,]
###-------------------------------------------------------------------------------------------------------------

str(curve_list4[[7]][[400]]) #98 
length(table(curve_list4[[7]][[400]]$CIP)) #98 -- both match so the de-dup worked

### Apply a function to the NESTED list: 
curve_list5 <- lapply(curve_list4, lapply, function(x) {
												(data.frame(threshold = x$threshold/10, 
												percent_all = sum(x$S_Students_14.x, na.rm = TRUE)/x$total_enrollment,
												percent_1264 = sum(x$S_Students_14.y, na.rm = TRUE)/x$total_enrollment,
												stringsAsFactors = FALSE))})
head(curve_list5[[7]][[400]])

### Do I reshape before or after generating curve_df1 ? After

curve_df1 <- list()
for(i in 1:length(curve_list5)){
	curve_df1[[i]] <- do.call(rbind, curve_list5[[i]])
	}
head(curve_df1[[1]])
			
curve_df2 <- lapply(curve_df1, function(x) unique(x))
head(curve_df2[[1]])

#Create data frame with all industries together
all_industries_together <- lapply(curve_df2, function(x) x[, -3]) #Can I get industry name in this step
names(all_industries_together) <- c("Construction", "Finance", "Healthcare", "Hospitality", "Information", "LifeSciences", "Manufacturing", "TLD", "NonKeyIndustry")
nms <- names(all_industries_together)

for(i in 1:length(all_industries_together)) {
					all_industries_together[[i]] <- 
					data.frame(all_industries_together[[i]][,1:2], 
					Industry = nms[i], stringsAsFactors = FALSE)
				}


all_industries_together1 <- data.frame(Map(c, all_industries_together[[1]],
									all_industries_together[[2]],
									all_industries_together[[3]],
									all_industries_together[[4]],
									all_industries_together[[5]],
									all_industries_together[[6]],
									all_industries_together[[7]],
									all_industries_together[[8]]))

str(all_industries_together1)
### Need to have industry name on this before I reshape the data								
 

### Reshape the data
industries_separate <- lapply(curve_df2, function(x) melt(x, id.vars = "threshold"))
industries_together <- melt(all_industries_together1, id.vars = c("threshold", "Industry"))

### 1. PLOT INDUSTRIES_SEPARATE

### Use ggplot2 to plot the results for industries_separate:
names(industries_separate) <- c("Construction", "Finance", "Healthcare", "Hospitality", "Information", "LifeSciences", "Manufacturing", "TLD", "NonKeyIndustry")
nms <- names(industries_separate)

plot1 <- lapply(industries_separate, function(x) {
			ggplot(data=x, aes(x=threshold, y=value, group = variable, color = variable)) + 
				geom_smooth(linetype="solid", size=2.5) + 
				theme(axis.text=element_text(size=14),
				axis.title=element_text(size=18),
				plot.title=element_text(size=20,face="bold")) +
				xlab("Closeness of CTE Program to Key Industry Sector") + 
				ylab("Percent of NJ Students Enrolled in CTE Program Tied to Demand Industries")
				})

plot2 <- list()
for(i in 1:length(plot1)) {
			plot2[[i]] <- plot1[[i]] + ggtitle(paste0(nms[i], " Industry"))
			}

names(all_industries_together) <- c("Construction", "Finance", "", "", "", "LifeSciences", "", "", "NonKeyIndustry")
	
ggsave(filename = "Construction.pdf", plot = plot2[[1]], height = 10, width = 16)
ggsave(filename = "Finance.pdf", plot = plot2[[2]], height = 10, width = 16)
ggsave(filename = "Healthcare.pdf", plot = plot2[[3]], height = 10, width = 16)
ggsave(filename = "Hospitality.pdf", plot = plot2[[4]], height = 10, width = 16)
ggsave(filename = "Information.pdf", plot = plot2[[5]], height = 10, width = 16)
ggsave(filename = "LifeSciences.pdf", plot = plot2[[6]], height = 10, width = 16)
ggsave(filename = "Manufacturing.pdf", plot = plot2[[7]], height = 10, width = 16)
ggsave(filename = "TLD.pdf", plot = plot2[[8]], height = 10, width = 16)
	
### 2. PLOT INDUSTRIES_TOGETHER
plot3 <- ggplot(data=industries_together, aes(x=threshold, y=value, group = Industry, color = Industry)) + 
				geom_smooth(linetype="solid", size=2.5) + 
				theme(axis.text=element_text(size=14),
				axis.title=element_text(size=18),
				plot.title=element_text(size=20,face="bold")) +
				ggtitle("Students Trained in Programs Associated with NJ Key Industry Sectors ") +
				xlab("Closeness of CTE Program to Industry") + 
				ylab("Proportion of NJ Students Enrolled in CTE Program Tied to Key Industries")
plot3
ggsave(filename = "All Industries.pdf", plot = plot3, height = 10, width = 16)







# 3. Clean and manage the CIPSOC_enr_occemp_wage data

###Re-write as a function to accommodate the creation of the SECONDARY AND THE POSTSECONDARY FILES.

names(cip_soc_n) <- c("soc2","soc3","soc4")
cip_soc_n <- lapply(cip_soc_n, setNames, nm= c("CIP","CIP_Title", "S_Students_13","S_Students_14","SOC.n","SOC", "SOC_Title", "NumberEmployed","job_zone", "H_PCT10","PS_Concentrator_14"))
str(cip_soc_n)

### A. CREATE sec_only

## To generate the PS outcomes file, replace S_Students_14 with PS_Concentrator_14
## and change the name of the file to be written in the very last line of code

sec_only <- lapply(cip_soc_n, function(x) x[x$S_Students_14 > 0, ])
sec_cip_only <- lapply(sec_only, function(x) x[!duplicated(as.integer(x$CIP)), ])
sec_cip_only_rank <- lapply(sec_cip_only, function(x) {
						x %>% 
							mutate(enrollment_percentile = percent_rank(S_Students_14))
						})
sec_only_unique_cipsoc <- lapply(sec_only, function(x) {
						x %>% 
							group_by(CIP,SOC) %>%
							slice(1:1) %>%
							group_by(CIP) %>%
								summarize(total_employment = sum(NumberEmployed, na.rm=TRUE),
								entry_wage = mean(H_PCT10, na.rm = TRUE))%>%
								mutate(employment_percentile = percent_rank(total_employment))
						})

sec_pctenr_pctemp <- Map(function(x, y) merge(x, y, by = "CIP"), x = sec_cip_only_rank, y = sec_only_unique_cipsoc)

sec_pctenr_pctemp_diff <- lapply(sec_pctenr_pctemp, function(x) {
									x %>% 
										mutate(supply_index = enrollment_percentile - employment_percentile) %>%
										arrange(supply_index) %>%
										select(CIP, CIP_Title, S_Students_14, SOC.n, total_employment, entry_wage, enrollment_percentile, employment_percentile, supply_index)
									})
final_sec_supply <- sec_pctenr_pctemp_diff %>%
Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="CIP"), .)

final_sec_supply <- final_sec_supply[, c(1:8, 12:16, 20:24, 9,17,25)]
names(final_sec_supply) <- c("CIP", "CIP_Title", "Nbr_Enrolled_2014",
							"SOC_2","total_employment_2","entry_wage_2",
							"enrollment_percentile_2","employment_percentile_2",
							"SOC_3","total_employment_3","entry_wage_3",
							"enrollment_percentile_3","employment_percentile_3",
							"SOC_4","total_employment_4","entry_wage_4",
							"enrollment_percentile_4","employment_percentile_4",
							"supply_index_2", "supply_index_3", "supply_index_4")
head(final_sec_supply)
write.csv(final_sec_supply, file = "final_sec_supply.csv", row.names= FALSE)

final_sec_supply_4ind <- final_sec_supply %>%
								select(CIP, CIP_Title, Nbr_Enrolled_2014, supply_index_2, supply_index_3, supply_index_4) %>%
								mutate(supply_index_combined = supply_index_2 + supply_index_3 + supply_index_4) %>%
								arrange(supply_index_combined)
write.csv(final_sec_supply_4ind, file = "final_sec_supply_4ind.csv", row.names = FALSE)
