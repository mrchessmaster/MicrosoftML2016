library(dplyr)

ratio <- function(vec) {
  vec <- vec[!is.na(vec)]
  return (sum(vec == T) / length(vec))
}

#training <- read.csv("datasense_census_microdata_training.csv")
training = read.csv("train_train.csv")[-1]
validation = read.csv("train_validation.csv")[-1]
train_test_set = read.csv("train_test_set.csv")[-1]
dev = training

# logistic regression attempt:

mod <- glm(morethan60kyr ~ ., data = dev, family= binomial)
prob = predict(mod, train_test_set, type = "response")
sum(train_test_set$morethan60kyr == (prob > 0.5))/length(prob)

# 9 bach
# 10 university above bach
# 11 degree med, dentent dsfn
# 12 master
# 13 doctorate

# logistic combined with filtering
mod <- glm(morethan60kyr ~ ., data = dev, family= binomial)
prob = predict(mod, train_test_set, type = "response")
indices <- train_test_set[train_test_set$citizen_Other == 1,]
indices <- indices[indices$kol_French == 1,]

prob[c(185,535,3402)] <- 0
sum(train_test_set$morethan60kyr == (prob > 0.5))/length(prob)



#eliminate one feature:
for (i in c(1:5,7:12)) {
  temp = dev[,-i]
  mod <- glm(morethan60kyr ~ ., data = temp, family = binomial)
  prob = predict(mod, train_test_set, type = "response")
  print(sum(train_test_set$morethan60kyr == (prob > 0.5))/length(prob))
}

dev_temp = dev[,-2]
# eliminate second feature
for (i in c(1:4,6:12)) {
  temp = dev_temp[,-i]
  mod <- glm(morethan60kyr ~ ., data = temp, family = binomial)
  prob = predict(mod, validation, type = "response")
  print(sum(validation$morethan60kyr == (prob > 0.5))/length(prob))
}

dev_temp = dev_temp[,-1]

## 73.18 at best






# random forest attempt:

form = names(dev)
form = form[!form %in% c("morethan60kyr")]
form = paste(form,collapse = "+")
form = as.formula(paste("morethan60kyr",form,sep="~"))

#rf = randomForest(form,dev,ntree=1,importance = T)





## checking random criteria
#dev2=dev %>% filter(mfs_Humanities_and_related_fields == 1) %>% filter(citizen_Other == 1)%>% filter(sex_female == 1)%>% filter(kol_French == 1)

dev1=dev %>% filter(citizen_Other == 1) %>% filter(kol_Neither_EnglishFrench == 1)
dev1=dev %>% filter(citizen_Other == 1) %>% filter(kol_English == 1)
dev1=dev %>% filter(citizen_Other == 1) %>% filter(kol_French == 1)
dev1=dev %>% filter(citizen_Other == 1) %>% filter(kol_Both_EnglishFrench == 1)
dev1=dev %>% filter(citizen_Canada_birth == 1) %>% filter(kol_Neither_EnglishFrench == 1)
dev1=dev %>% filter(citizen_Canada_birth == 1) %>% filter(kol_English == 1)
dev1=dev %>% filter(citizen_Canada_birth == 1) %>% filter(kol_French == 1)
dev1=dev %>% filter(citizen_Canada_birth == 1) %>% filter(kol_Both_EnglishFrench == 1)
dev1=dev %>% filter(citizen_Canada_naturalization == 1) %>% filter(kol_Neither_EnglishFrench == 1)
dev1=dev %>% filter(citizen_Canada_naturalization == 1) %>% filter(kol_English == 1)
dev1=dev %>% filter(citizen_Canada_naturalization == 1) %>% filter(kol_French == 1)
dev1=dev %>% filter(citizen_Canada_naturalization == 1) %>% filter(kol_Both_EnglishFrench == 1)

dev1=dev %>% filter(citizen_Other == 1) %>% filter(hdgree == 9)
dev1=dev %>% filter(citizen_Canada_birth == 1) %>% filter(hdgree == 13) %>% filter(sex_male == 1) %>% filter(kol_Both_EnglishFrench == 1) # 78.4
dev1=dev %>% filter(citizen_Canada_birth == 1) %>% filter(hdgree == 13) %>% filter(sex_male == 1) %>% filter(kol_Both_EnglishFrench == 1) %>% filter(cma_Ottawa_Gatineau == 1)# 84
dev1=dev %>% filter(citizen_Canada_birth == 1) %>% filter(hdgree == 13) %>% filter(sex_male == 1) %>% filter(kol_Both_EnglishFrench == 1) %>% filter(cma_Vancouver == 1) # 100
dev1=dev %>% filter(citizen_Canada_birth == 1) %>% filter(hdgree == 13) %>% filter(sex_male == 1) %>% filter(kol_Both_EnglishFrench == 1) %>% filter(cma_Vancouver == 0 & cma_Ottawa_Gatineau == 0) # 76
dev1=dev %>% filter(citizen_Canada_naturalization == 1) %>% filter(hdgree == 13) %>% filter(sex_male == 1) %>% filter(kol_Both_EnglishFrench == 1) # 78.4

dev1=dev %>% filter(citizen_Canada_naturalization == 1) %>% filter(hdgree == 12) %>% filter(sex_male == 1) %>% filter(kol_Both_EnglishFrench == 1) %>% filter(cma_Ottawa_Gatineau == 1)# 95
dev1=dev %>% filter(citizen_Canada_naturalization == 1) %>% filter(hdgree == 13) %>% filter(sex_male == 1) %>% filter(kol_Both_EnglishFrench == 1) %>% filter(cma_Vancouver == 1) #100
dev1=dev %>% filter(citizen_Canada_naturalization == 1) %>% filter(hdgree == 13) %>% filter(sex_male == 1) %>% filter(kol_Both_EnglishFrench == 1) %>% filter(cma_Vancouver == 0 & cma_Ottawa_Gatineau == 0) 
dev1=dev %>% filter(citizen_Canada_naturalization == 1) %>% filter(hdgree == 13) %>% filter(sex_male == 1) %>% filter(cma_Vancouver == 1) 

dev1=dev %>% filter(cip_Mathematics_Computer_InformationSciences == 1) %>% filter(citizen_Canada_birth == 1) %>% filter(sex_male == 1) %>% filter(hdgree == 10)

dim(dev1)[1]
ratio(dev1$morethan60kyr)


check_filters <- function(dev,CASEID0 = -1,hhsize0 = -1,agegrp0 = -1,hdgree0 = -1,hrswrk0 = -1,morethan60kyr0 = -1,cma_Brantford_Guelph_Barrie0 = -1,cma_Calgary0 = -1,cma_Edmonton0 = -1,cma_GreaterSudbury_GrandSudbury_ThunderBay0 = -1,cma_Halifax0 = -1,
                          cma_Hamilton0 = -1,cma_Kelowna_Abbotsford0 = -1,cma_Kingston_Peterborough0 = -1,cma_Kitchener0 = -1,cma_London0 = -1,cma_Moncton_SaintJohn0 = -1,cma_Montreal0 = -1,cma_Oshawa0 = -1,cma_Ottawa_Gatineau0 = -1,cma_Quebec0 = -1,cma_Regina_Saskatoon0 = -1,
                          cma_Sherbrooke_TroisRivieres0 = -1,cma_StCatharines_Niagara0 = -1,cma_Toronto0 = -1,cma_Vancouver0 = -1,cma_Victoria0 = -1,cma_Windsor0 = -1,cma_Winnipeg0 = -1,cma_other0 = -1,pr_Alberta0 = -1,pr_BritishColumbia0 = -1,pr_Manitoba0 = -1,
                          pr_NewBrunswick0 = -1,pr_Newfoundland_Labrador0 = -1,pr_NorthernCanada0 = -1,pr_NovaScotia0 = -1,pr_Ontario0 = -1,pr_PrinceEdwardIsland0 = -1,pr_Quebec0 = -1,pr_Saskatchewan0 = -1,marst_Divorced0 = -1,marst_Legally_married0 = -1,marst_Separated_married0 = -1,
                          marst_Single0 = -1,marst_Widowed0 = -1,sex_female0 = -1,sex_male0 = -1,citizen_Canada_birth0 = -1,citizen_Canada_naturalization0 = -1,citizen_Other0 = -1,fol_Both_EnglishFrench0 = -1,fol_English0 = -1,fol_French0 = -1,fol_Neither_EnglishFrench0 = -1,
                          kol_Both_EnglishFrench0 = -1,kol_English0 = -1,kol_French0 = -1,kol_Neither_EnglishFrench0 = -1,attsch_Attended0 = -1,attsch_Not_Attended0 = -1,cip_Agriculture_NatResources_Conservation0 = -1,cip_Architecture_Eingeering_andRelatedTechnologies0 = -1,cip_Business_Management_PublicAdministration0 = -1,cip_Education0 = -1,cip_Health_Parks_Recreation_Fitness0 = -1,
                          cip_Humanities0 = -1,cip_Mathematics_Computer_InformationSciences0 = -1,cip_Other0 = -1,cip_Personal_Protective_TransportationServices0 = -1,cip_Physical_LifeSciences_Technologies0 = -1,cip_Social_BehaviouralSciences_Law0 = -1,cip_Visual_PerformingArts_CommunicationsTechnologies0 = -1,locstud_Alberta0 = -1,locstud_British_Columbia0 = -1,locstud_Eastern_Asia0 = -1,locstud_Europe0 = -1,
                          locstud_Manitoba0 = -1,locstud_New_Brunswick0 = -1,locstud_Newfoundland_and_Labrador0 = -1,locstud_Nova_Scotia0 = -1,locstud_Ontario0 = -1,locstud_Other_Americas0 = -1,locstud_Other_countries_and_regions0 = -1,locstud_Prince_Edward_Island0 = -1,locstud_Quebec0 = -1,locstud_Saskatchewan0 = -1,locstud_Southeast_and_Southern_Asia0 = -1,
                          locstud_United_States_of_America0 = -1,mtnno_Aboriginal_languages0 = -1,mtnno_All_other_languages0 = -1,mtnno_Arabic0 = -1,mtnno_Austro_Asiatic_languages0 = -1,mtnno_Chinese_languages0 = -1,mtnno_Dravidian_languages0 = -1,mtnno_Finno_Ugric_Languages0 = -1,mtnno_German0 = -1,mtnno_Greek0 = -1,mtnno_Indo_Iranian_Languages0 = -1,
                          mtnno_Italian0 = -1,mtnno_Niger_Congo_languages_and_other_African0 = -1,mtnno_No_non_official_language0 = -1,mtnno_Other_Afro_Asiatic_languages0 = -1,mtnno_Other_European_languages0 = -1,mtnno_Other_Germanic_Languages0 = -1,mtnno_Other_Slavic_Languages0 = -1,mtnno_Other_South_East_Asian_languages0 = -1,mtnno_Panjabi0 = -1,mtnno_Polish0 = -1,mtnno_Portuguese0 = -1,
                          mtnno_Russian0 = -1,mtnno_Spanish0 = -1,mtnno_Tagalog0 = -1,mtnno_Ukrainian0 = -1,mfs_Agricultural__biological__nutritional__and_food_sciences0 = -1,mfs_Applied_science_technologies_and_trades0 = -1,mfs_Commerce__management_and_business_administration0 = -1,mfs_Educational__recreational_and_counselling_services0 = -1,mfs_Engineering_and_applied_sciences0 = -1,mfs_Fine_and_applied_arts0 = -1,mfs_Health_professions_and_related_technologies0 = -1,
                          mfs_Humanities_and_related_fields0 = -1,mfs_Mathematics__computer_and_physical_sciences0 = -1,mfs_Social_sciences_and_related_fields0 = -1,powst_No_fixed_address0 = -1,powst_Worked_at_home0 = -1,powst_Worked_in_a_different_census_municipality_within0 = -1,powst_Worked_in_a_different_county0 = -1,powst_Worked_in_a_different_province0 = -1,powst_Worked_in_census_municipality_of_residence0 = -1,powst_Worked_outside_Canada0 = -1,cow_Paid_worker_Originally_self_employed_with_paid_help__incorporated0 = -1,
                          cow_Paid_worker_Originally_self_employed_without_paid_help__incorporated0 = -1,cow_Paid_worker_Working_for_wages__salary__tips_or_commission0 = -1,cow_Self_employed_with_paid_help__not_incorporated0 = -1,cow_Self_employed_without_paid_help__not_incorporated0 = -1,cow_Unpaid_family_workers_in_a_family_business0 = -1,naics_Accommodation_and_food_services0 = -1,naics_Administrative_and_support__waste_management_and0 = -1,naics_Agriculture__forestry__fishing_and_hunting0 = -1,naics_Arts__entertainment_and_recreation0 = -1,naics_Construction0 = -1,naics_Educational_services0 = -1,
                          naics_Finance_and_insurance0 = -1,naics_Health_care_and_social_assistance0 = -1,naics_Information_and_cultural_industries0 = -1,naics_Management_of_companies_and_enterprises0 = -1,naics_Manufacturing0 = -1,naics_Mining_and_oil_and_gas_extraction0 = -1,naics_Other_services_except_public_administration0 = -1,naics_Professional__scientific_and_technical_services0 = -1,naics_Public_administration0 = -1,naics_Real_estate_and_rental_and_leasing0 = -1,naics_Retail_trade0 = -1,
                          naics_Transportation_and_warehousing0 = -1,naics_Utilities0 = -1,naics_Wholesale_trade0 = -1,nochrd_Administrative_and_senior_clerical_personnel0 = -1,nochrd_Clerical_personnel0 = -1,nochrd_Intermediate_sales_and_service_personnel0 = -1,nochrd_Managers0 = -1,nochrd_Other_manual_workers0 = -1,nochrd_Other_sales_and_service_personnel0 = -1,nochrd_Professionals0 = -1,nochrd_Semi_professionals_and_technicians0 = -1,
                          nochrd_Semi_skilled_manual_workers0 = -1,nochrd_Skilled_crafts_and_trades_workers0 = -1,nochrd_Skilled_sales_and_service_personnel0 = -1,nochrd_Supervisors0 = -1,nocs_Chefs_and_cooks__supervisors__and_other_occupations_in_food_and_beverage_service0 = -1,nocs_Childcare_and_home_support_workers0 = -1,nocs_Clerical_occupations_and_clerical_supervisors0 = -1,nocs_Construction_trades0 = -1,nocs_Contractors_and_supervisors_in_trades_and_transportation0 = -1,nocs_Financial__secretarial_and_administrative_occupations0 = -1,nocs_Labourers_in_processing__manufacturing_and_utilities0 = -1,
                          nocs_Occupations_in_art__culture__recreation_and_sport0 = -1,nocs_Occupations_in_natural_and_applied_sciences0 = -1,nocs_Occupations_in_protective_services0 = -1,nocs_Occupations_in_social_science__government_services_and_religion0 = -1,nocs_Occupations_unique_to_primary_industries0 = -1,nocs_Other_management_occupations0 = -1,nocs_Other_trades_occupations0 = -1,nocs_Professional_occupations_in_business_and_finance0 = -1,nocs_Professional_occupations_in_health__registered_nurses_and_supervisors0 = -1,nocs_Retail_trade_supervisors__salespersons__sales_clerks_and_cashiers0 = -1,nocs_Senior_management_occupations0 = -1,
                          nocs_Service_supervisors__occupations_in_travel_and_accommodation__attendants_in0 = -1,nocs_Supervisors__machine_operators_and_assemblers_in_manufacturing0 = -1,nocs_Teachers_and_professors0 = -1,nocs_Technical__assisting_and_related_occupations_in_health0 = -1,nocs_Trades_helpers__construction__and_transportation_labourers_and_related_occupations0 = -1,nocs_Transport_and_equipment_operators0 = -1,nocs_Wholesale__technical__insurance__real_estate_sales_specialists__and_retail__wholesale0 = -1) {
  if (CASEID0 != -1) dev = filter(dev,CASEID == CASEID0)
  if (hhsize0 != -1) dev = filter(dev,hhsize == hhsize0)
  if (agegrp0 != -1) dev = filter(dev,agegrp == agegrp0)
  if (hdgree0 != -1) dev = filter(dev,hdgree == hdgree0)
  if (hrswrk0 != -1) dev = filter(dev,hrswrk == hrswrk0)
  if (morethan60kyr0 != -1) dev = filter(dev,morethan60kyr == morethan60kyr0)
  if (cma_Brantford_Guelph_Barrie0 != -1) dev = filter(dev,cma_Brantford_Guelph_Barrie == cma_Brantford_Guelph_Barrie0)
  if (cma_Calgary0 != -1) dev = filter(dev,cma_Calgary == cma_Calgary0)
  if (cma_Edmonton0 != -1) dev = filter(dev,cma_Edmonton == cma_Edmonton0)
  if (cma_GreaterSudbury_GrandSudbury_ThunderBay0 != -1) dev = filter(dev,cma_GreaterSudbury_GrandSudbury_ThunderBay == cma_GreaterSudbury_GrandSudbury_ThunderBay0)
  if (cma_Halifax0 != -1) dev = filter(dev,cma_Halifax == cma_Halifax0)
  if (cma_Hamilton0 != -1) dev = filter(dev,cma_Hamilton == cma_Hamilton0)
  if (cma_Kelowna_Abbotsford0 != -1) dev = filter(dev,cma_Kelowna_Abbotsford == cma_Kelowna_Abbotsford0)
  if (cma_Kingston_Peterborough0 != -1) dev = filter(dev,cma_Kingston_Peterborough == cma_Kingston_Peterborough0)
  if (cma_Kitchener0 != -1) dev = filter(dev,cma_Kitchener == cma_Kitchener0)
  if (cma_London0 != -1) dev = filter(dev,cma_London == cma_London0)
  if (cma_Moncton_SaintJohn0 != -1) dev = filter(dev,cma_Moncton_SaintJohn == cma_Moncton_SaintJohn0)
  if (cma_Montreal0 != -1) dev = filter(dev,cma_Montreal == cma_Montreal0)
  if (cma_Oshawa0 != -1) dev = filter(dev,cma_Oshawa == cma_Oshawa0)
  if (cma_Ottawa_Gatineau0 != -1) dev = filter(dev,cma_Ottawa_Gatineau == cma_Ottawa_Gatineau0)
  if (cma_Quebec0 != -1) dev = filter(dev,cma_Quebec == cma_Quebec0)
  if (cma_Regina_Saskatoon0 != -1) dev = filter(dev,cma_Regina_Saskatoon == cma_Regina_Saskatoon0)
  if (cma_Sherbrooke_TroisRivieres0 != -1) dev = filter(dev,cma_Sherbrooke_TroisRivieres == cma_Sherbrooke_TroisRivieres0)
  if (cma_StCatharines_Niagara0 != -1) dev = filter(dev,cma_StCatharines_Niagara == cma_StCatharines_Niagara0)
  if (cma_Toronto0 != -1) dev = filter(dev,cma_Toronto == cma_Toronto0)
  if (cma_Vancouver0 != -1) dev = filter(dev,cma_Vancouver == cma_Vancouver0)
  if (cma_Victoria0 != -1) dev = filter(dev,cma_Victoria == cma_Victoria0)
  if (cma_Windsor0 != -1) dev = filter(dev,cma_Windsor == cma_Windsor0)
  if (cma_Winnipeg0 != -1) dev = filter(dev,cma_Winnipeg == cma_Winnipeg0)
  if (cma_other0 != -1) dev = filter(dev,cma_other == cma_other0)
  if (pr_Alberta0 != -1) dev = filter(dev,pr_Alberta == pr_Alberta0)
  if (pr_BritishColumbia0 != -1) dev = filter(dev,pr_BritishColumbia == pr_BritishColumbia0)
  if (pr_Manitoba0 != -1) dev = filter(dev,pr_Manitoba == pr_Manitoba0)
  if (pr_NewBrunswick0 != -1) dev = filter(dev,pr_NewBrunswick == pr_NewBrunswick0)
  if (pr_Newfoundland_Labrador0 != -1) dev = filter(dev,pr_Newfoundland_Labrador == pr_Newfoundland_Labrador0)
  if (pr_NorthernCanada0 != -1) dev = filter(dev,pr_NorthernCanada == pr_NorthernCanada0)
  if (pr_NovaScotia0 != -1) dev = filter(dev,pr_NovaScotia == pr_NovaScotia0)
  if (pr_Ontario0 != -1) dev = filter(dev,pr_Ontario == pr_Ontario0)
  if (pr_PrinceEdwardIsland0 != -1) dev = filter(dev,pr_PrinceEdwardIsland == pr_PrinceEdwardIsland0)
  if (pr_Quebec0 != -1) dev = filter(dev,pr_Quebec == pr_Quebec0)
  if (pr_Saskatchewan0 != -1) dev = filter(dev,pr_Saskatchewan == pr_Saskatchewan0)
  if (marst_Divorced0 != -1) dev = filter(dev,marst_Divorced == marst_Divorced0)
  if (marst_Legally_married0 != -1) dev = filter(dev,marst_Legally_married == marst_Legally_married0)
  if (marst_Separated_married0 != -1) dev = filter(dev,marst_Separated_married == marst_Separated_married0)
  if (marst_Single0 != -1) dev = filter(dev,marst_Single == marst_Single0)
  if (marst_Widowed0 != -1) dev = filter(dev,marst_Widowed == marst_Widowed0)
  if (sex_female0 != -1) dev = filter(dev,sex_female == sex_female0)
  if (sex_male0 != -1) dev = filter(dev,sex_male == sex_male0)
  if (citizen_Canada_birth0 != -1) dev = filter(dev,citizen_Canada_birth == citizen_Canada_birth0)
  if (citizen_Canada_naturalization0 != -1) dev = filter(dev,citizen_Canada_naturalization == citizen_Canada_naturalization0)
  if (citizen_Other0 != -1) dev = filter(dev,citizen_Other == citizen_Other0)
  if (fol_Both_EnglishFrench0 != -1) dev = filter(dev,fol_Both_EnglishFrench == fol_Both_EnglishFrench0)
  if (fol_English0 != -1) dev = filter(dev,fol_English == fol_English0)
  if (fol_French0 != -1) dev = filter(dev,fol_French == fol_French0)
  if (fol_Neither_EnglishFrench0 != -1) dev = filter(dev,fol_Neither_EnglishFrench == fol_Neither_EnglishFrench0)
  if (kol_Both_EnglishFrench0 != -1) dev = filter(dev,kol_Both_EnglishFrench == kol_Both_EnglishFrench0)
  if (kol_English0 != -1) dev = filter(dev,kol_English == kol_English0)
  if (kol_French0 != -1) dev = filter(dev,kol_French == kol_French0)
  if (kol_Neither_EnglishFrench0 != -1) dev = filter(dev,kol_Neither_EnglishFrench == kol_Neither_EnglishFrench0)
  if (attsch_Attended0 != -1) dev = filter(dev,attsch_Attended == attsch_Attended0)
  if (attsch_Not_Attended0 != -1) dev = filter(dev,attsch_Not_Attended == attsch_Not_Attended0)
  if (cip_Agriculture_NatResources_Conservation0 != -1) dev = filter(dev,cip_Agriculture_NatResources_Conservation == cip_Agriculture_NatResources_Conservation0)
  if (cip_Architecture_Eingeering_andRelatedTechnologies0 != -1) dev = filter(dev,cip_Architecture_Eingeering_andRelatedTechnologies == cip_Architecture_Eingeering_andRelatedTechnologies0)
  if (cip_Business_Management_PublicAdministration0 != -1) dev = filter(dev,cip_Business_Management_PublicAdministration == cip_Business_Management_PublicAdministration0)
  if (cip_Education0 != -1) dev = filter(dev,cip_Education == cip_Education0)
  if (cip_Health_Parks_Recreation_Fitness0 != -1) dev = filter(dev,cip_Health_Parks_Recreation_Fitness == cip_Health_Parks_Recreation_Fitness0)
  if (cip_Humanities0 != -1) dev = filter(dev,cip_Humanities == cip_Humanities0)
  if (cip_Mathematics_Computer_InformationSciences0 != -1) dev = filter(dev,cip_Mathematics_Computer_InformationSciences == cip_Mathematics_Computer_InformationSciences0)
  if (cip_Other0 != -1) dev = filter(dev,cip_Other == cip_Other0)
  if (cip_Personal_Protective_TransportationServices0 != -1) dev = filter(dev,cip_Personal_Protective_TransportationServices == cip_Personal_Protective_TransportationServices0)
  if (cip_Physical_LifeSciences_Technologies0 != -1) dev = filter(dev,cip_Physical_LifeSciences_Technologies == cip_Physical_LifeSciences_Technologies0)
  if (cip_Social_BehaviouralSciences_Law0 != -1) dev = filter(dev,cip_Social_BehaviouralSciences_Law == cip_Social_BehaviouralSciences_Law0)
  if (cip_Visual_PerformingArts_CommunicationsTechnologies0 != -1) dev = filter(dev,cip_Visual_PerformingArts_CommunicationsTechnologies == cip_Visual_PerformingArts_CommunicationsTechnologies0)
  if (locstud_Alberta0 != -1) dev = filter(dev,locstud_Alberta == locstud_Alberta0)
  if (locstud_British_Columbia0 != -1) dev = filter(dev,locstud_British_Columbia == locstud_British_Columbia0)
  if (locstud_Eastern_Asia0 != -1) dev = filter(dev,locstud_Eastern_Asia == locstud_Eastern_Asia0)
  if (locstud_Europe0 != -1) dev = filter(dev,locstud_Europe == locstud_Europe0)
  if (locstud_Manitoba0 != -1) dev = filter(dev,locstud_Manitoba == locstud_Manitoba0)
  if (locstud_New_Brunswick0 != -1) dev = filter(dev,locstud_New_Brunswick == locstud_New_Brunswick0)
  if (locstud_Newfoundland_and_Labrador0 != -1) dev = filter(dev,locstud_Newfoundland_and_Labrador == locstud_Newfoundland_and_Labrador0)
  if (locstud_Nova_Scotia0 != -1) dev = filter(dev,locstud_Nova_Scotia == locstud_Nova_Scotia0)
  if (locstud_Ontario0 != -1) dev = filter(dev,locstud_Ontario == locstud_Ontario0)
  if (locstud_Other_Americas0 != -1) dev = filter(dev,locstud_Other_Americas == locstud_Other_Americas0)
  if (locstud_Other_countries_and_regions0 != -1) dev = filter(dev,locstud_Other_countries_and_regions == locstud_Other_countries_and_regions0)
  if (locstud_Prince_Edward_Island0 != -1) dev = filter(dev,locstud_Prince_Edward_Island == locstud_Prince_Edward_Island0)
  if (locstud_Quebec0 != -1) dev = filter(dev,locstud_Quebec == locstud_Quebec0)
  if (locstud_Saskatchewan0 != -1) dev = filter(dev,locstud_Saskatchewan == locstud_Saskatchewan0)
  if (locstud_Southeast_and_Southern_Asia0 != -1) dev = filter(dev,locstud_Southeast_and_Southern_Asia == locstud_Southeast_and_Southern_Asia0)
  if (locstud_United_States_of_America0 != -1) dev = filter(dev,locstud_United_States_of_America == locstud_United_States_of_America0)
  if (mtnno_Aboriginal_languages0 != -1) dev = filter(dev,mtnno_Aboriginal_languages == mtnno_Aboriginal_languages0)
  if (mtnno_All_other_languages0 != -1) dev = filter(dev,mtnno_All_other_languages == mtnno_All_other_languages0)
  if (mtnno_Arabic0 != -1) dev = filter(dev,mtnno_Arabic == mtnno_Arabic0)
  if (mtnno_Austro_Asiatic_languages0 != -1) dev = filter(dev,mtnno_Austro_Asiatic_languages == mtnno_Austro_Asiatic_languages0)
  if (mtnno_Chinese_languages0 != -1) dev = filter(dev,mtnno_Chinese_languages == mtnno_Chinese_languages0)
  if (mtnno_Dravidian_languages0 != -1) dev = filter(dev,mtnno_Dravidian_languages == mtnno_Dravidian_languages0)
  if (mtnno_Finno_Ugric_Languages0 != -1) dev = filter(dev,mtnno_Finno_Ugric_Languages == mtnno_Finno_Ugric_Languages0)
  if (mtnno_German0 != -1) dev = filter(dev,mtnno_German == mtnno_German0)
  if (mtnno_Greek0 != -1) dev = filter(dev,mtnno_Greek == mtnno_Greek0)
  if (mtnno_Indo_Iranian_Languages0 != -1) dev = filter(dev,mtnno_Indo_Iranian_Languages == mtnno_Indo_Iranian_Languages0)
  if (mtnno_Italian0 != -1) dev = filter(dev,mtnno_Italian == mtnno_Italian0)
  if (mtnno_Niger_Congo_languages_and_other_African0 != -1) dev = filter(dev,mtnno_Niger_Congo_languages_and_other_African == mtnno_Niger_Congo_languages_and_other_African0)
  if (mtnno_No_non_official_language0 != -1) dev = filter(dev,mtnno_No_non_official_language == mtnno_No_non_official_language0)
  if (mtnno_Other_Afro_Asiatic_languages0 != -1) dev = filter(dev,mtnno_Other_Afro_Asiatic_languages == mtnno_Other_Afro_Asiatic_languages0)
  if (mtnno_Other_European_languages0 != -1) dev = filter(dev,mtnno_Other_European_languages == mtnno_Other_European_languages0)
  if (mtnno_Other_Germanic_Languages0 != -1) dev = filter(dev,mtnno_Other_Germanic_Languages == mtnno_Other_Germanic_Languages0)
  if (mtnno_Other_Slavic_Languages0 != -1) dev = filter(dev,mtnno_Other_Slavic_Languages == mtnno_Other_Slavic_Languages0)
  if (mtnno_Other_South_East_Asian_languages0 != -1) dev = filter(dev,mtnno_Other_South_East_Asian_languages == mtnno_Other_South_East_Asian_languages0)
  if (mtnno_Panjabi0 != -1) dev = filter(dev,mtnno_Panjabi == mtnno_Panjabi0)
  if (mtnno_Polish0 != -1) dev = filter(dev,mtnno_Polish == mtnno_Polish0)
  if (mtnno_Portuguese0 != -1) dev = filter(dev,mtnno_Portuguese == mtnno_Portuguese0)
  if (mtnno_Russian0 != -1) dev = filter(dev,mtnno_Russian == mtnno_Russian0)
  if (mtnno_Spanish0 != -1) dev = filter(dev,mtnno_Spanish == mtnno_Spanish0)
  if (mtnno_Tagalog0 != -1) dev = filter(dev,mtnno_Tagalog == mtnno_Tagalog0)
  if (mtnno_Ukrainian0 != -1) dev = filter(dev,mtnno_Ukrainian == mtnno_Ukrainian0)
  if (mfs_Agricultural__biological__nutritional__and_food_sciences0 != -1) dev = filter(dev,mfs_Agricultural__biological__nutritional__and_food_sciences == mfs_Agricultural__biological__nutritional__and_food_sciences0)
  if (mfs_Applied_science_technologies_and_trades0 != -1) dev = filter(dev,mfs_Applied_science_technologies_and_trades == mfs_Applied_science_technologies_and_trades0)
  if (mfs_Commerce__management_and_business_administration0 != -1) dev = filter(dev,mfs_Commerce__management_and_business_administration == mfs_Commerce__management_and_business_administration0)
  if (mfs_Educational__recreational_and_counselling_services0 != -1) dev = filter(dev,mfs_Educational__recreational_and_counselling_services == mfs_Educational__recreational_and_counselling_services0)
  if (mfs_Engineering_and_applied_sciences0 != -1) dev = filter(dev,mfs_Engineering_and_applied_sciences == mfs_Engineering_and_applied_sciences0)
  if (mfs_Fine_and_applied_arts0 != -1) dev = filter(dev,mfs_Fine_and_applied_arts == mfs_Fine_and_applied_arts0)
  if (mfs_Health_professions_and_related_technologies0 != -1) dev = filter(dev,mfs_Health_professions_and_related_technologies == mfs_Health_professions_and_related_technologies0)
  if (mfs_Humanities_and_related_fields0 != -1) dev = filter(dev,mfs_Humanities_and_related_fields == mfs_Humanities_and_related_fields0)
  if (mfs_Mathematics__computer_and_physical_sciences0 != -1) dev = filter(dev,mfs_Mathematics__computer_and_physical_sciences == mfs_Mathematics__computer_and_physical_sciences0)
  if (mfs_Social_sciences_and_related_fields0 != -1) dev = filter(dev,mfs_Social_sciences_and_related_fields == mfs_Social_sciences_and_related_fields0)
  if (powst_No_fixed_address0 != -1) dev = filter(dev,powst_No_fixed_address == powst_No_fixed_address0)
  if (powst_Worked_at_home0 != -1) dev = filter(dev,powst_Worked_at_home == powst_Worked_at_home0)
  if (powst_Worked_in_a_different_census_municipality_within0 != -1) dev = filter(dev,powst_Worked_in_a_different_census_municipality_within == powst_Worked_in_a_different_census_municipality_within0)
  if (powst_Worked_in_a_different_county0 != -1) dev = filter(dev,powst_Worked_in_a_different_county == powst_Worked_in_a_different_county0)
  if (powst_Worked_in_a_different_province0 != -1) dev = filter(dev,powst_Worked_in_a_different_province == powst_Worked_in_a_different_province0)
  if (powst_Worked_in_census_municipality_of_residence0 != -1) dev = filter(dev,powst_Worked_in_census_municipality_of_residence == powst_Worked_in_census_municipality_of_residence0)
  if (powst_Worked_outside_Canada0 != -1) dev = filter(dev,powst_Worked_outside_Canada == powst_Worked_outside_Canada0)
  if (cow_Paid_worker_Originally_self_employed_with_paid_help__incorporated0 != -1) dev = filter(dev,cow_Paid_worker_Originally_self_employed_with_paid_help__incorporated == cow_Paid_worker_Originally_self_employed_with_paid_help__incorporated0)
  if (cow_Paid_worker_Originally_self_employed_without_paid_help__incorporated0 != -1) dev = filter(dev,cow_Paid_worker_Originally_self_employed_without_paid_help__incorporated == cow_Paid_worker_Originally_self_employed_without_paid_help__incorporated0)
  if (cow_Paid_worker_Working_for_wages__salary__tips_or_commission0 != -1) dev = filter(dev,cow_Paid_worker_Working_for_wages__salary__tips_or_commission == cow_Paid_worker_Working_for_wages__salary__tips_or_commission0)
  if (cow_Self_employed_with_paid_help__not_incorporated0 != -1) dev = filter(dev,cow_Self_employed_with_paid_help__not_incorporated == cow_Self_employed_with_paid_help__not_incorporated0)
  if (cow_Self_employed_without_paid_help__not_incorporated0 != -1) dev = filter(dev,cow_Self_employed_without_paid_help__not_incorporated == cow_Self_employed_without_paid_help__not_incorporated0)
  if (cow_Unpaid_family_workers_in_a_family_business0 != -1) dev = filter(dev,cow_Unpaid_family_workers_in_a_family_business == cow_Unpaid_family_workers_in_a_family_business0)
  if (naics_Accommodation_and_food_services0 != -1) dev = filter(dev,naics_Accommodation_and_food_services == naics_Accommodation_and_food_services0)
  if (naics_Administrative_and_support__waste_management_and0 != -1) dev = filter(dev,naics_Administrative_and_support__waste_management_and == naics_Administrative_and_support__waste_management_and0)
  if (naics_Agriculture__forestry__fishing_and_hunting0 != -1) dev = filter(dev,naics_Agriculture__forestry__fishing_and_hunting == naics_Agriculture__forestry__fishing_and_hunting0)
  if (naics_Arts__entertainment_and_recreation0 != -1) dev = filter(dev,naics_Arts__entertainment_and_recreation == naics_Arts__entertainment_and_recreation0)
  if (naics_Construction0 != -1) dev = filter(dev,naics_Construction == naics_Construction0)
  if (naics_Educational_services0 != -1) dev = filter(dev,naics_Educational_services == naics_Educational_services0)
  if (naics_Finance_and_insurance0 != -1) dev = filter(dev,naics_Finance_and_insurance == naics_Finance_and_insurance0)
  if (naics_Health_care_and_social_assistance0 != -1) dev = filter(dev,naics_Health_care_and_social_assistance == naics_Health_care_and_social_assistance0)
  if (naics_Information_and_cultural_industries0 != -1) dev = filter(dev,naics_Information_and_cultural_industries == naics_Information_and_cultural_industries0)
  if (naics_Management_of_companies_and_enterprises0 != -1) dev = filter(dev,naics_Management_of_companies_and_enterprises == naics_Management_of_companies_and_enterprises0)
  if (naics_Manufacturing0 != -1) dev = filter(dev,naics_Manufacturing == naics_Manufacturing0)
  if (naics_Mining_and_oil_and_gas_extraction0 != -1) dev = filter(dev,naics_Mining_and_oil_and_gas_extraction == naics_Mining_and_oil_and_gas_extraction0)
  if (naics_Other_services_except_public_administration0 != -1) dev = filter(dev,naics_Other_services_except_public_administration == naics_Other_services_except_public_administration0)
  if (naics_Professional__scientific_and_technical_services0 != -1) dev = filter(dev,naics_Professional__scientific_and_technical_services == naics_Professional__scientific_and_technical_services0)
  if (naics_Public_administration0 != -1) dev = filter(dev,naics_Public_administration == naics_Public_administration0)
  if (naics_Real_estate_and_rental_and_leasing0 != -1) dev = filter(dev,naics_Real_estate_and_rental_and_leasing == naics_Real_estate_and_rental_and_leasing0)
  if (naics_Retail_trade0 != -1) dev = filter(dev,naics_Retail_trade == naics_Retail_trade0)
  if (naics_Transportation_and_warehousing0 != -1) dev = filter(dev,naics_Transportation_and_warehousing == naics_Transportation_and_warehousing0)
  if (naics_Utilities0 != -1) dev = filter(dev,naics_Utilities == naics_Utilities0)
  if (naics_Wholesale_trade0 != -1) dev = filter(dev,naics_Wholesale_trade == naics_Wholesale_trade0)
  if (nochrd_Administrative_and_senior_clerical_personnel0 != -1) dev = filter(dev,nochrd_Administrative_and_senior_clerical_personnel == nochrd_Administrative_and_senior_clerical_personnel0)
  if (nochrd_Clerical_personnel0 != -1) dev = filter(dev,nochrd_Clerical_personnel == nochrd_Clerical_personnel0)
  if (nochrd_Intermediate_sales_and_service_personnel0 != -1) dev = filter(dev,nochrd_Intermediate_sales_and_service_personnel == nochrd_Intermediate_sales_and_service_personnel0)
  if (nochrd_Managers0 != -1) dev = filter(dev,nochrd_Managers == nochrd_Managers0)
  if (nochrd_Other_manual_workers0 != -1) dev = filter(dev,nochrd_Other_manual_workers == nochrd_Other_manual_workers0)
  if (nochrd_Other_sales_and_service_personnel0 != -1) dev = filter(dev,nochrd_Other_sales_and_service_personnel == nochrd_Other_sales_and_service_personnel0)
  if (nochrd_Professionals0 != -1) dev = filter(dev,nochrd_Professionals == nochrd_Professionals0)
  if (nochrd_Semi_professionals_and_technicians0 != -1) dev = filter(dev,nochrd_Semi_professionals_and_technicians == nochrd_Semi_professionals_and_technicians0)
  if (nochrd_Semi_skilled_manual_workers0 != -1) dev = filter(dev,nochrd_Semi_skilled_manual_workers == nochrd_Semi_skilled_manual_workers0)
  if (nochrd_Skilled_crafts_and_trades_workers0 != -1) dev = filter(dev,nochrd_Skilled_crafts_and_trades_workers == nochrd_Skilled_crafts_and_trades_workers0)
  if (nochrd_Skilled_sales_and_service_personnel0 != -1) dev = filter(dev,nochrd_Skilled_sales_and_service_personnel == nochrd_Skilled_sales_and_service_personnel0)
  if (nochrd_Supervisors0 != -1) dev = filter(dev,nochrd_Supervisors == nochrd_Supervisors0)
  if (nocs_Chefs_and_cooks__supervisors__and_other_occupations_in_food_and_beverage_service0 != -1) dev = filter(dev,nocs_Chefs_and_cooks__supervisors__and_other_occupations_in_food_and_beverage_service == nocs_Chefs_and_cooks__supervisors__and_other_occupations_in_food_and_beverage_service0)
  if (nocs_Childcare_and_home_support_workers0 != -1) dev = filter(dev,nocs_Childcare_and_home_support_workers == nocs_Childcare_and_home_support_workers0)
  if (nocs_Clerical_occupations_and_clerical_supervisors0 != -1) dev = filter(dev,nocs_Clerical_occupations_and_clerical_supervisors == nocs_Clerical_occupations_and_clerical_supervisors0)
  if (nocs_Construction_trades0 != -1) dev = filter(dev,nocs_Construction_trades == nocs_Construction_trades0)
  if (nocs_Contractors_and_supervisors_in_trades_and_transportation0 != -1) dev = filter(dev,nocs_Contractors_and_supervisors_in_trades_and_transportation == nocs_Contractors_and_supervisors_in_trades_and_transportation0)
  if (nocs_Financial__secretarial_and_administrative_occupations0 != -1) dev = filter(dev,nocs_Financial__secretarial_and_administrative_occupations == nocs_Financial__secretarial_and_administrative_occupations0)
  if (nocs_Labourers_in_processing__manufacturing_and_utilities0 != -1) dev = filter(dev,nocs_Labourers_in_processing__manufacturing_and_utilities == nocs_Labourers_in_processing__manufacturing_and_utilities0)
  if (nocs_Occupations_in_art__culture__recreation_and_sport0 != -1) dev = filter(dev,nocs_Occupations_in_art__culture__recreation_and_sport == nocs_Occupations_in_art__culture__recreation_and_sport0)
  if (nocs_Occupations_in_natural_and_applied_sciences0 != -1) dev = filter(dev,nocs_Occupations_in_natural_and_applied_sciences == nocs_Occupations_in_natural_and_applied_sciences0)
  if (nocs_Occupations_in_protective_services0 != -1) dev = filter(dev,nocs_Occupations_in_protective_services == nocs_Occupations_in_protective_services0)
  if (nocs_Occupations_in_social_science__government_services_and_religion0 != -1) dev = filter(dev,nocs_Occupations_in_social_science__government_services_and_religion == nocs_Occupations_in_social_science__government_services_and_religion0)
  if (nocs_Occupations_unique_to_primary_industries0 != -1) dev = filter(dev,nocs_Occupations_unique_to_primary_industries == nocs_Occupations_unique_to_primary_industries0)
  if (nocs_Other_management_occupations0 != -1) dev = filter(dev,nocs_Other_management_occupations == nocs_Other_management_occupations0)
  if (nocs_Other_trades_occupations0 != -1) dev = filter(dev,nocs_Other_trades_occupations == nocs_Other_trades_occupations0)
  if (nocs_Professional_occupations_in_business_and_finance0 != -1) dev = filter(dev,nocs_Professional_occupations_in_business_and_finance == nocs_Professional_occupations_in_business_and_finance0)
  if (nocs_Professional_occupations_in_health__registered_nurses_and_supervisors0 != -1) dev = filter(dev,nocs_Professional_occupations_in_health__registered_nurses_and_supervisors == nocs_Professional_occupations_in_health__registered_nurses_and_supervisors0)
  if (nocs_Retail_trade_supervisors__salespersons__sales_clerks_and_cashiers0 != -1) dev = filter(dev,nocs_Retail_trade_supervisors__salespersons__sales_clerks_and_cashiers == nocs_Retail_trade_supervisors__salespersons__sales_clerks_and_cashiers0)
  if (nocs_Senior_management_occupations0 != -1) dev = filter(dev,nocs_Senior_management_occupations == nocs_Senior_management_occupations0)
  if (nocs_Service_supervisors__occupations_in_travel_and_accommodation__attendants_in0 != -1) dev = filter(dev,nocs_Service_supervisors__occupations_in_travel_and_accommodation__attendants_in == nocs_Service_supervisors__occupations_in_travel_and_accommodation__attendants_in0)
  if (nocs_Supervisors__machine_operators_and_assemblers_in_manufacturing0 != -1) dev = filter(dev,nocs_Supervisors__machine_operators_and_assemblers_in_manufacturing == nocs_Supervisors__machine_operators_and_assemblers_in_manufacturing0)
  if (nocs_Teachers_and_professors0 != -1) dev = filter(dev,nocs_Teachers_and_professors == nocs_Teachers_and_professors0)
  if (nocs_Technical__assisting_and_related_occupations_in_health0 != -1) dev = filter(dev,nocs_Technical__assisting_and_related_occupations_in_health == nocs_Technical__assisting_and_related_occupations_in_health0)
  if (nocs_Trades_helpers__construction__and_transportation_labourers_and_related_occupations0 != -1) dev = filter(dev,nocs_Trades_helpers__construction__and_transportation_labourers_and_related_occupations == nocs_Trades_helpers__construction__and_transportation_labourers_and_related_occupations0)
  if (nocs_Transport_and_equipment_operators0 != -1) dev = filter(dev,nocs_Transport_and_equipment_operators == nocs_Transport_and_equipment_operators0)
  if (nocs_Wholesale__technical__insurance__real_estate_sales_specialists__and_retail__wholesale0 != -1) dev = filter(dev,nocs_Wholesale__technical__insurance__real_estate_sales_specialists__and_retail__wholesale == nocs_Wholesale__technical__insurance__real_estate_sales_specialists__and_retail__wholesale0)  
  print(dim(dev)[1])
  print(ratio(dev$morethan60kyr))
}






# 1 depth tree
