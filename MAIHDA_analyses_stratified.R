library(haven)


# pull popn density var in from exam 4 datafile
census_df <- read_sas("/Users/tinlizzy/Documents/professional/career/BUSPH/GREEENS and ESIcog/Green space project/data/MESAa23_CensTrctSES_20220824/MESAa23_CensTrctSES_20220824.sas7bdat")
head(census_df)
census_df_sm <- census_df %>% 
  filter(EXAM==4) %>% #select for exam 4 values only
  select(idno,popdenmi_nowat) # keep only necess vars
head(census_df_sm)
dim(census_df_sm) #5693
head(gsv_mesa)
dim(gsv_mesa) #6814
gsv_mesa_popden <- inner_join(gsv_mesa,census_df_sm,"idno")
head(gsv_mesa_popden) # 6814   36
dim (gsv_mesa_popden) # 5693   37
