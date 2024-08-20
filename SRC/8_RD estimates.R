#Loading required packages.
library(dplyr)
library(rdrobust)
library(ggplot2)
library(stargazer)
options(scipen=999)

#Reading final dataset.
df_controls <- readRDS("DATA/df_controls.rds")

#Main RDD statewise.

#Creating list variables to store regression results.
srdd <- list()
srdd2 <- list()

#Running statewise RDD with and without controls using riots post 2018 as outcome.
for (i in c("BIHAR","KERALA", "ANDHRA PRADESH", "TELANGANA",
            "UTTARPRADESH", "JHARKHAND", "CHHATISGARH",
            "MADHYA PRADESH", "MAHARASHTRA","ODISHA")) {
  df <- df_controls[df_controls$STATE == i, ]
  z_controls <- cbind(df$pc18_sc_share, df$pc18_st_share,
                      df$pc18_work_share, df$pc18_rural_share,
                      df$pc18_lit_share, df$pc18_forest_share)
  
  srdd[[i]] <- rdrobust(y = df$rt_2019 + df$rt_2020 + df$rt_2021 + df$rt_2022 + df$rt_2023, x = df$d2c_18,  all = TRUE, covs = z_controls)
  srdd2[[i]] <- rdrobust(y = df$rt_2019 + df$rt_2020 + df$rt_2021 + df$rt_2022 + df$rt_2023, x = df$d2c_18,  all = TRUE)
}

#Reporting main results state-wise.
summary(srdd[["ANDHRA PRADESH"]])
summary(srdd2[["ANDHRA PRADESH"]])
summary(srdd[["BIHAR"]])
summary(srdd2[["BIHAR"]])
summary(srdd[["CHHATISGARH"]])
summary(srdd2[["CHHATISGARH"]])
summary(srdd[["JHARKHAND"]])
summary(srdd2[["JHARKHAND"]])
summary(srdd[["KERALA"]])
summary(srdd2[["KERALA"]])
summary(srdd[["MADHYA PRADESH"]])
summary(srdd2[["MADHYA PRADESH"]])
summary(srdd[["MAHARASHTRA"]])
summary(srdd2[["MAHARASHTRA"]])
summary(srdd[["ODISHA"]])
summary(srdd2[["ODISHA"]])
summary(srdd[["TELANGANA"]])
summary(srdd2[["TELANGANA"]])
summary(srdd[["UTTARPRADESH"]])
summary(srdd2[["UTTARPRADESH"]])

#Running aggregate RDD with and without controls using riots post 2018 as outcome.

#Copying final data into a temporary df.
df <- df_controls

#Creating matrix for state fixed effects.
states_fe <- model.matrix(~ df$STATE - 1)
states_fe <- states_fe[, -c(11)]

#Binding all controls together.
z_controls <- cbind(df$pc18_sc_share, df$pc18_st_share,
                    df$pc18_work_share, df$pc18_rural_share,
                    df$pc18_lit_share, df$pc18_forest_share, states_fe)

#With controls and standard errors clustered by state.
srdd3 <- rdrobust(y = df$rt_2019 + df$rt_2020 + df$rt_2021 + df$rt_2022 + df$rt_2023,
                 x = df$d2c_18,  all = TRUE, cluster = df$STATE, covs = z_controls)
summary(srdd3)


#Without controls but standard errors clustered by state.
srdd4 <- rdrobust(y = df$rt_2019 + df$rt_2020 + df$rt_2021 + df$rt_2022 + df$rt_2023,
                 x = df$d2c_18,  all = TRUE, cluster = df$STATE)
summary(srdd4)

#Placebo test.
#Using pre 2018 riots as outcome. Only checking for CHHATISGARH and ODISHA.

#CHHATISGARH
df <- df_controls[df_controls$STATE == "CHHATISGARH", ]

z_controls <- cbind(df$pc18_sc_share, df$pc18_st_share,
                    df$pc18_work_share, df$pc18_rural_share,
                    df$pc18_lit_share, df$pc18_forest_share)

srdd5 <- rdrobust(y = df$rt_2016 + df$rt_2017,
                 x = df$d2c_18,  all = TRUE, covs = z_controls)
summary(srdd5)

#ODISHA
df <- df_controls[df_controls$STATE == "ODISHA", ]

z_controls <- cbind(df$pc18_sc_share, df$pc18_st_share,
                    df$pc18_work_share, df$pc18_rural_share,
                    df$pc18_lit_share, df$pc18_forest_share)

srdd6 <- rdrobust(y = df$rt_2016 + df$rt_2017,
                 x = df$d2c_18,  all = TRUE, covs = z_controls)
summary(srdd6)


#Temmporary


#Running statewise RDD with and without controls using all violence post 2018 as outcome.
srdd7 <- list()
for (i in c("BIHAR","KERALA", "ANDHRA PRADESH", "TELANGANA",
            "UTTARPRADESH", "JHARKHAND", "CHHATISGARH",
            "MADHYA PRADESH", "MAHARASHTRA","ODISHA")) {
  df <- df_controls[df_controls$STATE == i, ]
  z_controls <- cbind(df$pc18_sc_share, df$pc18_st_share,
                      df$pc18_work_share, df$pc18_rural_share,
                      df$pc18_lit_share, df$pc18_forest_share)
  
  srdd7[[i]] <- rdrobust(y = df$rt_2019 + df$rt_2020 + df$rt_2021 + df$rt_2022 + df$rt_2023 +
		df$vc_2019 + df$vc_2020 + df$vc_2021 + df$vc_2022 + df$vc_2023 +
		df$er_2019 + df$er_2020 + df$er_2021 + df$er_2022 + df$er_2023 +
		df$bt_2019 + df$bt_2020 + df$bt_2021 + df$bt_2022 + df$bt_2023 +
		df$pr_2019 + df$pr_2020 + df$pr_2021 + df$pr_2022 + df$pr_2023 +
		df$st_2019 + df$st_2020 + df$st_2021 + df$st_2022 + df$st_2023, x = df$d2c_18,  all = TRUE, covs = z_controls)
}

summary(srdd7[["ANDHRA PRADESH"]])
summary(srdd7[["BIHAR"]])
summary(srdd7[["CHHATISGARH"]])
summary(srdd7[["JHARKHAND"]])
summary(srdd7[["KERALA"]])
summary(srdd7[["MADHYA PRADESH"]])
summary(srdd7[["MAHARASHTRA"]])
summary(srdd7[["ODISHA"]])
summary(srdd7[["TELANGANA"]])
summary(srdd7[["UTTARPRADESH"]])
