
library(haven)
library(readxl)
library(data.table)
library(psych)
# load the data


occ_1990_2010_xwalk <- fread("ref/usa_00017.csv")
occ_1990_2010_xwalk[,.(OCC, OCC2010)] %>% unique() -> occ_1990_2010_xwalk
setnames(occ_1990_2010_xwalk, "OCC", "OCC1990")

occ_2010_soc_xwalk <- fread("ref/usa_00018.csv")
occ_2010_soc_xwalk[,.(OCC, OCCSOC)] %>% unique() -> occ_2010_soc_xwalk
setnames(occ_2010_soc_xwalk, "OCC", "OCC2010")

occ_1990_soc_xwalk <- merge(occ_1990_2010_xwalk, occ_2010_soc_xwalk, by = "OCC2010")

########################

# now create skills dataset
skills_2009 <- read.delim('/Users/hyork/Documents/projects/occupation/ref/db_14_0 2009.7/Skills.txt') %>% data.table()
skills_2013 <-  read.delim('/Users/hyork/Documents/projects/occupation/ref/db_18_0_2013.7/Skills.txt') %>% data.table()
skills_2018 <- read_excel("/Users/hyork/Documents/projects/occupation/ref/db_22_2_excel 2018.2/Skills.xlsx") %>% data.table()
skills_2009[, year := 2009]
skills_2013[, year := 2013]
skills_2018[, year := 2018]
setnames(skills_2018, names(skills_2018), gsub(" ", ".", names(skills_2018), fixed = T))
skills_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
skills1 <- rbindlist(list(skills_2009, skills_2013, skills_2018), fill = T)
skills1 <- skills1[, .(O.NET.SOC.Code, Element.Name, Scale.ID, Data.Value, Standard.Error, year)]
skills1[, Element.Name := paste0("skl_", Element.Name)]
# add abilities
abilities_2009 <- read.delim('/Users/hyork/Documents/projects/occupation/ref/db_14_0 2009.7/Abilities.txt') %>% data.table()
abilities_2013 <-  read.delim('/Users/hyork/Documents/projects/occupation/ref/db_18_0_2013.7/Abilities.txt') %>% data.table()
abilities_2018 <- read_excel("/Users/hyork/Documents/projects/occupation/ref/db_22_2_excel 2018.2/Abilities.xlsx") %>% data.table()
abilities_2009[, year := 2009]
abilities_2013[, year := 2013]
abilities_2018[, year := 2018]
setnames(abilities_2018, names(abilities_2018), gsub(" ", ".", names(abilities_2018), fixed = T))
abilities_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
abilities <- rbindlist(list(abilities_2009, abilities_2013, abilities_2018), fill = T)
abilities <- abilities[, .(O.NET.SOC.Code, Element.Name, Scale.ID, Data.Value, Standard.Error, year)]
abilities[, Element.Name := paste0("abl_", Element.Name)]

# add knowledge
knowledge_2009 <- read.delim('/Users/hyork/Documents/projects/occupation/ref/db_14_0 2009.7/Knowledge.txt') %>% data.table()
knowledge_2013 <-  read.delim('/Users/hyork/Documents/projects/occupation/ref/db_18_0_2013.7/Knowledge.txt') %>% data.table()
knowledge_2018 <- read_excel("/Users/hyork/Documents/projects/occupation/ref/db_22_2_excel 2018.2/Knowledge.xlsx") %>% data.table()
knowledge_2009[, year := 2009]
knowledge_2013[, year := 2013]
knowledge_2018[, year := 2018]
setnames(knowledge_2018, names(knowledge_2018), gsub(" ", ".", names(knowledge_2018), fixed = T))
knowledge_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
knowledge <- rbindlist(list(knowledge_2009, knowledge_2013, knowledge_2018), fill = T)
knowledge <- knowledge[, .(O.NET.SOC.Code, Element.Name, Scale.ID, Data.Value, Standard.Error, year)]
knowledge[, Element.Name := paste0("knl_", Element.Name)]

# add work activities
workactivities_2009 <- read.delim('/Users/hyork/Documents/projects/occupation/ref/db_14_0 2009.7/Work Activities.txt') %>% data.table()
workactivities_2013 <-  read.delim('/Users/hyork/Documents/projects/occupation/ref/db_18_0_2013.7/Work Activities.txt') %>% data.table()
workactivities_2018 <- read_excel("/Users/hyork/Documents/projects/occupation/ref/db_22_2_excel 2018.2/Work Activities.xlsx") %>% data.table()
workactivities_2009[, year := 2009]
workactivities_2013[, year := 2013]
workactivities_2018[, year := 2018]
setnames(workactivities_2018, names(workactivities_2018), gsub(" ", ".", names(workactivities_2018), fixed = T))
workactivities_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
workactivities <- rbindlist(list(workactivities_2009, workactivities_2013, workactivities_2018), fill = T)
workactivities <- workactivities[, .(O.NET.SOC.Code, Element.Name, Scale.ID, Data.Value, Standard.Error, year)]
workactivities[, Element.Name := paste0("act_", Element.Name)]

# add work styles
workstyles_2009 <- read.delim('/Users/hyork/Documents/projects/occupation/ref/db_14_0 2009.7/Work Styles.txt') %>% data.table()
workstyles_2013 <-  read.delim('/Users/hyork/Documents/projects/occupation/ref/db_18_0_2013.7/Work Styles.txt') %>% data.table()
workstyles_2018 <- read_excel("/Users/hyork/Documents/projects/occupation/ref/db_22_2_excel 2018.2/Work Styles.xlsx") %>% data.table()
workstyles_2009[, year := 2009]
workstyles_2013[, year := 2013]
workstyles_2018[, year := 2018]
setnames(workstyles_2018, names(workstyles_2018), gsub(" ", ".", names(workstyles_2018), fixed = T))
workstyles_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
workstyles <- rbindlist(list(workstyles_2009, workstyles_2013, workstyles_2018), fill = T)
workstyles <- workstyles[, .(O.NET.SOC.Code, Element.Name, Scale.ID, Data.Value, Standard.Error, year)]
workstyles[, Element.Name := paste0("sty_", Element.Name)]

# add work context
context_2009 <- read.delim('/Users/hyork/Documents/projects/occupation/ref/db_14_0 2009.7/Work Context.txt') %>% data.table()
context_2013 <-  read.delim('/Users/hyork/Documents/projects/occupation/ref/db_18_0_2013.7/Work Context.txt') %>% data.table()
context_2018 <- read_excel("/Users/hyork/Documents/projects/occupation/ref/db_22_2_excel 2018.2/Work Context.xlsx") %>% data.table()
context_2009[, year := 2009]
context_2013[, year := 2013]
context_2018[, year := 2018]
setnames(context_2018, names(context_2018), gsub(" ", ".", names(context_2018), fixed = T))
context_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
context <- rbindlist(list(context_2009, context_2013, context_2018), fill = T)
context <- context[, .(O.NET.SOC.Code, Element.Name, Scale.ID, Data.Value, Standard.Error, year)]
context <- context[Scale.ID %in% c("CX", "CT")]
context[, Element.Name := paste0("ctx_", Element.Name)]

# add work values
values_2009 <- read.delim('/Users/hyork/Documents/projects/occupation/ref/db_14_0 2009.7/Work Values.txt') %>% data.table()
values_2013 <-  read.delim('/Users/hyork/Documents/projects/occupation/ref/db_18_0_2013.7/Work Values.txt') %>% data.table()
values_2018 <- read_excel("/Users/hyork/Documents/projects/occupation/ref/db_22_2_excel 2018.2/Work Values.xlsx") %>% data.table()
values_2009[, year := 2009]
values_2013[, year := 2013]
values_2018[, year := 2018]
setnames(values_2018, names(values_2018), gsub(" ", ".", names(values_2018), fixed = T))
values_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
values <- rbindlist(list(values_2009, values_2013, values_2018), fill = T)
values <- values[, .(O.NET.SOC.Code, Element.Name, Scale.ID, Data.Value, year)]
values <- values[Scale.ID %in% c("EX")]
values[, Element.Name := paste0("vlu_", Element.Name)]

# add education training, etc
education_2009 <- read.delim('/Users/hyork/Documents/projects/occupation/ref/db_14_0 2009.7/Education, Training, and Experience.txt') %>% data.table()
education_2013 <-  read.delim('/Users/hyork/Documents/projects/occupation/ref/db_18_0_2013.7/Education, Training, and Experience.txt') %>% data.table()
education_2018 <- read_excel("/Users/hyork/Documents/projects/occupation/ref/db_22_2_excel 2018.2/Education, Training, and Experience.xlsx") %>% data.table()
education_2009[, year := 2009]
education_2013[, year := 2013]
education_2018[, year := 2018]
setnames(education_2018, names(education_2018), gsub(" ", ".", names(education_2018), fixed = T))
education_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
education <- rbindlist(list(education_2009, education_2013, education_2018), fill = T)
education <- education[,.(Data.Value = stats::weighted.mean(Category, Data.Value)), by = .(O.NET.SOC.Code, Element.Name, Scale.ID, year)]
education <- education[, .(O.NET.SOC.Code, Element.Name, Scale.ID, Data.Value, year)]
education[, Element.Name := paste0("edu_", Element.Name)]


# # add tasks
# task_2009 <- read.delim('../ref/db_14_0 2009.7/Task Ratings.txt') %>% data.table()
# task_2013 <-  read.delim('../ref/db_18_0_2013.7/Task Ratings.txt') %>% data.table()
# task_2018 <- read_excel("../ref/db_22_2_excel 2018.2/Task Ratings.xlsx") %>% data.table()
# task_2009[, year := 2009]
# task_2013[, year := 2013]
# task_2018[, year := 2018]
# setnames(task_2018, names(task_2018), gsub(" ", ".", names(task_2018), fixed = T))
# task_2018[, `O.NET.SOC.Code` := `O*NET-SOC.Code`]
# task <- rbindlist(list(task_2009, task_2013, task_2018), fill = T)
# task <- task[, .(O.NET.SOC.Code, Task, Scale.ID, Data.Value, Standard.Error, year)]
# task <- task[Scale.ID %in% c("IM")]
# task[, Element.Name := paste0("tsk_", Task)]
# task <- task[!is.na(Task)]
#
skills <- rbindlist(list(skills1, knowledge, abilities, workstyles, 
                         workactivities,context, values, education), fill = T)
#skills <- rbindlist(list(skills, knowledge, abilities, workstyles, workactivities), fill = T)

# standardize
skills[Scale.ID == "LV", Data.Value := Data.Value/7]
skills[Scale.ID == "EX", Data.Value := (Data.Value-1)/6]
skills[Scale.ID == "IM", Data.Value := (Data.Value-1)/4]
skills[Scale.ID == "CX", Data.Value := (Data.Value-1)/4]
skills[Scale.ID == "CT", Data.Value := (Data.Value-1)/2]
skills[Scale.ID == "RW", Data.Value := (Data.Value-1)/9]
skills[Scale.ID == "PT", Data.Value := (Data.Value-1)/7]
skills[Scale.ID == "OJ", Data.Value := (Data.Value-1)/7]
skills[Scale.ID == "RL", Data.Value := (Data.Value-1)/11]

skills[Element.Name %like% "sty_", Scale.ID := "LV"]
skills[Element.Name %like% "ctx", Scale.ID := "LV"]
skills[Element.Name %like% "vlu", Scale.ID := "LV"]
skills[Element.Name %like% "edu_", Scale.ID := "LV"]

#
skills <- skills[Scale.ID == "LV"]
# reformat onet codes to merge
skills[,Element.Name2 := paste0(substr(Element.Name, 1, 15), "_\n", str_sub(Element.Name, -15, -1))]
skills_xwalk <- skills[,.(Element.Name2, Element.Name)] %>% unique()

#fwrite(skills_xwalk, "../ref/skills_xwalk.csv")

skills[,Element.Name := paste0(substr(Element.Name, 1, 15), "_\n", str_sub(Element.Name, -15, -1))]

skills[, OCCSOC := gsub("-", "", substr(O.NET.SOC.Code,1,7))]
skills[, Standard.Error := as.numeric(Standard.Error)]
skills <- skills[,.(Data.Value = mean(Data.Value),
                    Standard.Error = mean((Standard.Error))), by = .(Element.Name,Scale.ID, OCCSOC)]



########################

####################################################
occ_1990_soc_xwalk[!OCCSOC %in% unique(skills$OCCSOC) & !is.na(OCCSOC), unique(OCCSOC)] -> fixes
for(c.occsoc in fixes){
  keeper <- "continue"
  for(i in 1:4){
    # print(c.occsoc)
    # print(i)
    if(keeper == "continue"){
      fixes_substr <- str_sub(c.occsoc,1,-1-i)
      candidates <- unique(skills$OCCSOC)
      matches <- candidates[str_sub(candidates, 1, -1-i) == fixes_substr]
      if(length(matches > 1)){
        temp <- skills[OCCSOC %in% matches]
        temp[, OCCSOC := c.occsoc]
        temp <- temp[,.(Data.Value = mean(Data.Value),
                        Standard.Error = mean(Standard.Error)), by = .(Element.Name, Scale.ID, OCCSOC)]
        skills <- rbind(temp, skills, fill = T)
        keeper <- "stop"
      }
    }
  }
}
# #create a map of all children
# parents <- data.table(parents = unique(skills$OCCSOC)[unique(skills$OCCSOC) %like% "X$|0$"])
# parents_out <- data.table()
# for(c.parent in parents$parents){
#     substr_parent <- gsub("0$|00$|000$|0000$|X$|XX$|XXX$|XXXX$", "", c.parent, perl = TRUE)
#     children <- data.table(parents = c.parent, children = unique(skills$OCCSOC)[unique(skills$OCCSOC) %like% paste0("^", substr_parent)])
#     temp <- merge(parents, children, allow.cartesian = T)
#     if(nrow(temp)>1){
#         parents_out <- rbind(parents_out, temp, fill = T)
#     }
# }
# parents_out[, sub :=  gsub("0$|00$|000$|0000$|X$|XX$|XXX$|XXXX$", "", parents, perl = TRUE)]
# parents_out[, level := 6-nchar(sub)]
# 
# # see how many children occsocs aren't in every year
# acs[,.(year, OCCSOC)] %>% unique -> all_children
# 
# # get least common set
# all_children[year == 2009, unique(OCCSOC)][all_children[year == 2009, unique(OCCSOC)] %in%
#                                                all_children[year == 2013, unique(OCCSOC)] & 
#                                                all_children[year == 2009, unique(OCCSOC)] %in% 
#                                                all_children[year == 2018, unique(OCCSOC)]] -> common_set
# 
# children_fixes <- parents_out[!children %in% common_set]
# children_fixes[,N := .N, by = .(children)]
# children_fixes[,maxN := max(N), by = .(children)]
# children_fixes[,maxlevel := max(level), by = .(children)]
# 
# children_fixes <- children_fixes[ N == 1 | level == maxlevel]
# 
# children_fixes <- children_fixes[!duplicated(children_fixes$children)]
#     
# acs <- merge(acs, children_fixes[,.(parents, children)], 
#              by.x = "OCCSOC", by.y = "children", all.x = T)
# acs[!is.na(parents), OCCSOC := parents]
#standardize by percent
skills <- skills[OCCSOC %in% unique(occ_1990_soc_xwalk$OCCSOC)]
skills[,Element.Name := paste0(Element.Name,".", Scale.ID)]

skills_wide <- dcast(skills, OCCSOC   ~Element.Name, value.var = "Data.Value")



####################################################
vars <- names(skills_wide)[names(skills_wide) %like% ".LV"]



skills_wide <- merge(skills_wide, occ_1990_soc_xwalk, by = "OCCSOC")

skills_final <-skills_wide[,lapply(.SD, mean), by = .(OCC1990), .SDcols = vars]

inputs <- skills_final[,.SD,.SDcols = names(skills_final)[names(skills_final) %like% ".LV"]]
inputs <- inputs[,.SD,.SDcols = names(inputs)[!names(inputs) %like% "Job|Apprent"]]

fact_anal_orig <- fa(inputs, 10, rotate = "varimax", scores = "regression")

scores_out <- fact_anal_orig$scores %>% data.table()
scores_out[, OCC1990 := skills_final$OCC1990]
