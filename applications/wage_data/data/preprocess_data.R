library(ggplot2)
library(httr)

# general API manual: https://www.census.gov/content/dam/Census/data/developers/api-user-guide/api-guide.pdf

# all PUMS documentation: https://www.census.gov/programs-surveys/acs/technical-documentation/pums/documentation.html
# info about PUMS dataset: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/ACS2018_PUMS_README.pdf

# all variables info: https://www2.census.gov/programs-surveys/acs/tech_docs/pums/data_dict/PUMS_Data_Dictionary_2018.pdf
# explore variables: https://data.census.gov/mdat/#/search?ds=ACSPUMS1Y2018

#request = "https://api.census.gov/data/2018/acs/acs5/pums?get=PWGTP,NPF,PERNP,WAGP,NOC,WKHP,ENG,ESR,COW,SEX,AGEP,RAC1P,YOEP,WAOB,NATIVITY,WKW,FPARC,MAR,OCCP,POWSP,SCHL,CIT&ucgid=0400000US01,0400000US02,0400000US04,0400000US05,0400000US06,0400000US08,0400000US09,0400000US10,0400000US11,0400000US12,0400000US13,0400000US15,0400000US16,0400000US17,0400000US18,0400000US19,0400000US20,0400000US21,0400000US22,0400000US23,0400000US24,0400000US25,0400000US26,0400000US27,0400000US28,0400000US29,0400000US30,0400000US31,0400000US32,0400000US33,0400000US34,0400000US35,0400000US36,0400000US37,0400000US38,0400000US39,0400000US40,0400000US41,0400000US42,0400000US44,0400000US45,0400000US46,0400000US47,0400000US48,0400000US49,0400000US50,0400000US51,0400000US53,0400000US54,0400000US55,0400000US56"

vars = "PWGTP,NPF,PERNP,WAGP,NOC,WKHP,ENG,ESR,COW,SEX,AGEP,RAC1P,YOEP,WAOB,NATIVITY,WKW,FPARC,MAR,POWSP,SCHL,CIT,HISP,ADJINC,SOCP,OCCP,NAICSP,INDP"

#geography="0400000US01,0400000US02,0400000US04,0400000US05,0400000US06,0400000US08,0400000US09,0400000US10,0400000US11,0400000US12,0400000US13,0400000US15,0400000US16,0400000US17,0400000US18,0400000US19,0400000US20,0400000US21,0400000US22,0400000US23,0400000US24,0400000US25,0400000US26,0400000US27,0400000US28,0400000US29,0400000US30,0400000US31,0400000US32,0400000US33,0400000US34,0400000US35,0400000US36,0400000US37,0400000US38,0400000US39,0400000US40,0400000US41,0400000US42,0400000US44,0400000US45,0400000US46,0400000US47,0400000US48,0400000US49,0400000US50,0400000US51,0400000US53,0400000US54,0400000US55,0400000US56"
geographies=c("0400000US01","0400000US02","0400000US04","0400000US05","0400000US06","0400000US08","0400000US09","0400000US10",
              "0400000US11","0400000US12","0400000US13","0400000US15","0400000US16","0400000US17","0400000US18","0400000US19",
              "0400000US20","0400000US21","0400000US22","0400000US23","0400000US24","0400000US25","0400000US26","0400000US27",
              "0400000US28","0400000US29","0400000US30","0400000US31","0400000US32","0400000US33","0400000US34","0400000US35",
              "0400000US36","0400000US37","0400000US38","0400000US39","0400000US40","0400000US41","0400000US42","0400000US44",
              "0400000US45","0400000US46","0400000US47","0400000US48","0400000US49","0400000US50","0400000US51","0400000US53",
              "0400000US54","0400000US55","0400000US56")

year = "2018"
dataset = "acs/acs1/pums" #american community survey, 1 year, public microdata sample (PUMS)

# df = data.frame()
# for(var in strsplit(vars, ',')[[1]]){ #somehow requests return different number of datapoints so it does not work
#   print(var)
#   
#   request = paste("https://api.census.gov/data/", year, "/", dataset,
#                   "?get=", var,
#                   sep='')
# 
#   r <- GET(request)
#   print(r$status_code)
#   content = content(r)
# 
#   p = length(content[[1]])
#   n = length(content)-1
# 
#   M = matrix(unlist(content), ncol=p, byrow=TRUE)
#   tmp = data.frame(M[2:nrow(M),])
#   colnames(tmp) = M[1,]
# 
#   if(nrow(df) == 0){
#     df = tmp
#   } else {
#     df= cbind(df, tmp)
#   }
# }

df = data.frame()
for(i in 1:length(geographies)){
  geography = geographies[i]
  print(geography)

  request = paste("https://api.census.gov/data/", year, "/", dataset,
                 "?get=", vars,
                 "&ucgid=", geography,
                 sep='')

  r <- GET(request)
  print(r$status_code)
  content = content(r)

  p = length(content[[1]])
  n = length(content)-1

  M = matrix(unlist(content), ncol=p, byrow=TRUE)
  tmp = data.frame(M[2:nrow(M),])
  colnames(tmp) = M[1,]

  df = rbind(df, tmp)
}

save(df, file='data/raw_data/raw_data.Rdata')
##################################################################################################
load('data/raw_data/raw_data.Rdata')
#n = nrow(df)
#p = ncol(df)
#idx = sample(1:n, 100000, replace=FALSE)

wage = data.frame(weight=as.numeric(as.character(df$PWGTP)))
#qplot(wage$weight)

library(stringr)
wage$state = factor(str_pad(as.character(df$ST), 2, pad='0'))

#############################
wage$sex = c('male', 'female')[1+(df$SEX=='2')]
wage$sex = factor(wage$sex)
#sum(wage$sex=='male')/n
#sum((wage$sex=='male') * wage$weight)/sum(wage$weight)

wage$age = as.numeric(as.character(df$AGEP))
#qplot(wage$age)
#qplot(wage$age, fill=wage$sex, alpha=I(0.4), geom='density')#, weight=wage$weight)
#sum(wage$age[wage$sex=='male']*wage$weight[wage$sex=='male'])/sum(wage$weight[wage$sex=='male'])
#sum(wage$age[wage$sex=='female']*wage$weight[wage$sex=='female'])/sum(wage$weight[wage$sex=='female'])

wage$race = c('white',
              'black',
              'AIAN',
              'AIAN',
              'AIAN',
              'asian',
              'NHOPI',
              'other',
              'mix')[as.numeric(as.character(df$RAC1P))]
wage$race = factor(wage$race)
#qplot(wage$race)
#sum((wage$race=='black')*wage$weight)/sum(wage$weight) #where are hispanics? other or white?
#library(vcd)
#s = structable(wage$sex, wage$race)
#s
#s[2,]/colSums(s)

wage$hispanic_origin = factor(c('no', 'yes')[1+(df$HISP!='1')])

wage$nativity = c('native', 'foreign-born')[as.numeric(as.character(df$NATIVITY))]
wage$nativity = as.factor(wage$nativity)
#sum((wage$nativity=='foreign-born' & wage$sex=='male') * wage$weight) / sum((wage$nativity=='foreign-born') * wage$weight)

wage$citizenship = as.numeric(as.character(df$CIT))

########################
wage$marital = c('married',
              'widowed',
              'divorced',
              'separated',
              'never married')[as.numeric(as.character(df$MAR))]
wage$marital = factor(wage$marital)
#s=structable(wage$marital~wage$sex)
#s
#s[2,]/colSums(s)
#sum((wage$marital=='widowed' & wage$sex=='male') * wage$weight) / sum((wage$marital=='widowed') * wage$weight)

wage$family_size = as.numeric(as.character(df$NPF))
#qplot(factor(family_size), log(salary), data=wage[idx,], geom='boxplot')

wage$children = as.numeric(as.character(df$NOC))
#qplot(factor(children), log(salary), data=wage[idx,], geom='boxplot')

########################
wage$english_level = as.numeric(as.character(df$ENG)) #4 not at all, 0 native
#qplot(english_level, data=wage[idx,])

wage$education_level = as.numeric(as.character(df$SCHL))
wage$education_level[wage$education_level == 0] = NA
#qplot(education_level, data=wage[idx,], binwidth=1)
wage$education = c('No schooling completed',
                    'Nursery school, preschool',
                    'Kindergarten',
                    'Grade 1',
                    'Grade 2',
                    'Grade 3',
                    'Grade 4',
                    'Grade 5',
                    'Grade 6',
                    'Grade 7',
                    'Grade 8',
                    'Grade 9',
                    'Grade 10',
                    'Grade 11',
                    '12th grade - no diploma',
                    'Regular high school diploma',
                    'GED or alternative credential',
                    'Some college, but less than 1 year',
                    '1 or more years of college credit, no degree',
                    "Associate's degree",
                    "Bachelor's degree",
                    "Master's degree",
                    "Professional degree beyond a bachelor's degree",
                    'Doctorate degree')[wage$education_level]

##########################
wage$earnings = as.numeric(as.character(df$PERNP))
#qplot(wage$earnings)

wage$salary = as.numeric(as.character(df$WAGP))
wage$salary[wage$salary == -1] = NA
#qplot(wage$salary)
#qplot(salary, earnings, data=wage[idx,]) + geom_abline()

wage$hours_worked = as.numeric(as.character(df$WKHP))

wage$weeks_worked = c(52,
                     49,
                     47,
                     39,
                     26,
                     0)[as.numeric(df$WKW)]
#qplot(weeks_worked, data=wage[idx,])

wage$employment_status = c(NA,
                      'employed',
                      'not at work',
                      'unemployed',
                      'employed',
                      'not at work',
                      'not in labor force')[1+as.numeric(as.character(df$ESR))]
wage$employment_status = factor(wage$employment_status)
#table(wage$employment_status)

wage$employer = c(NA,
                 'for-profit company',
                 'non-profit company',
                 'government',
                 'government',
                 'government',
                 'self-employed',
                 'self-employed',
                 'self-employed',
                 NA)[1+as.numeric(as.character(df$COW))]
wage$employer = factor(wage$employer)
#table(wage$employer)
#qplot(employer, log(salary), data=wage[idx,], geom='boxplot')

######
wage$occupation = str_pad(as.character(df$OCCP), 4, pad='0')
wage$occupation[wage$occupation=='0009'] = NA

#convert to SOC code, the one provided in SOCP does not work but gives '*' whenever there is many candidates for one OCCP category
library(readxl)
url = 'https://www2.census.gov/programs-surveys/acs/tech_docs/pums/code_lists/ACSPUMS2018CodeLists.xls'
sheet_name = 'Occupation'
GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
table = read_excel(tf, sheet=sheet_name, col_names=FALSE)

wage$occupation_description = ''
for(i in 1:nrow(table)){
  print(i)
  key = as.character(table[i, 1])
  if(is.na(key) || length(key)==0){
    next
  }
  wage$occupation_description[wage$occupation == key] = table[i,3]
  wage$occupation[wage$occupation == key] = as.character(table[i,2])
}
wage$occupation = factor(wage$occupation)
####

####
industry = str_pad(as.character(df$INDP), 4, pad='0')
industry[wage$industry=='0169'] = NA

url = 'https://www2.census.gov/programs-surveys/acs/tech_docs/pums/code_lists/ACSPUMS2018CodeLists.xls'
sheet_name = 'Industry'
GET(url, write_disk(tf <- tempfile(fileext = ".xls")))
table = read_excel(tf, sheet=sheet_name, col_names=FALSE)

wage$industry_description = ''
wage$industry = ''
for(i in 1:nrow(table)){
  print(i)
  key = as.character(table[i, 3])
  if(is.na(key) || length(key)==0){
    next
  }
  wage$industry_description[industry == key] = table[i,2]
  wage$industry[industry == key] = as.character(table[i,5])
}
wage$industry = factor(wage$industry)
########

wage$place_of_work = str_pad(as.character(df$POWSP), 3, pad='0')
wage$place_of_work[wage$place_of_work=='000'] = NA
wage$place_of_work = factor(wage$place_of_work)

wage$economic_region = ''
wage$economic_region[wage$place_of_work %in% c('009', '023', '025', '033', '044', '050')] = 'New England'
wage$economic_region[wage$place_of_work %in% c('010', '011', '024', '034', '036', '042')] = 'Mideast'
wage$economic_region[wage$place_of_work %in% c('017', '018', '026', '039', '055')] = 'Great Lakes'
wage$economic_region[wage$place_of_work %in% c('019', '020', '027', '029', '031', '038', '046')] = 'Plains'
wage$economic_region[wage$place_of_work %in% c('001', '005', '012', '013', '021', '022', '028', '037', '045', '047', '051', '054')] = 'Southeast'
wage$economic_region[wage$place_of_work %in% c('004', '035', '040', '048')] = 'Southwest'
wage$economic_region[wage$place_of_work %in% c('008', '016', '040', '048', '030', '049', '056')] = 'Rocky Mountain'
wage$economic_region[wage$place_of_work %in% c('002', '006', '015', '032', '041', '053')] = 'Far West'
wage$economic_region[as.numeric(as.character(wage$place_of_work)) > 56] = 'Abroad'
wage$economic_region[is.na(wage$place_of_work)] = NA
wage$economic_region = factor(wage$economic_region)

#############################################################
ordering = c(
  'sex',
  'age',
  
  'race',
  'hispanic_origin',
  'citizenship',
  'nativity',
  
  'marital',
  'family_size',
  'children',
  'state',
  
  'education',
  'education_level',
  'english_level',
  
  'salary',
  'hours_worked',
  'weeks_worked',
  'employment_status',
  
  'occupation',
  'occupation_description',
  'industry',
  'industry_description',
  'employer',
  'place_of_work',
  'economic_region',
  
  'weight'
)

wage = wage[, ordering]
save(wage, file='data/datasets/wage_data.Rdata')
