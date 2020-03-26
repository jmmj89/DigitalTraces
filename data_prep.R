# Sociodemographics
# Remove duplicated rows from socio
df.socdem <- df.socdem[-which(duplicated(df.socdem)),]
# Collapsing categories (family, education, children, income)
df.socdem$family <- forcats::fct_collapse(df.socdem$family,
                                      "1: Married" = c("1: Marié", "1: Married"),
                                      "2: Civil Partnership" = c("2: Civil Partnership", "2: Pacsé"),
                                      "3: Single, living with partner" = c("3: En couple", "3: Single, living with partner"),
                                      "4: Single, not living with partner" = c("4: Célibataire", "4: Single, not living with partner"),
                                      "5: Divorced/widowed, living with partner" = c("5: Divorcé/Veuf, en couple", "5: Divorced/widowed, living with partner"),
                                      "6: Divorced/widowed, not living with partner" = c("6: Divorcé/Veuf, célibataire", "6: Divorced/widowed, not living with partner"))
df.socdem$family <- gsub('\\d+: ', '', df.socdem$family)

# Income categories only untill 2500 pounds
df.socdem$income[which(grepl("1: ", df.socdem$income))] <- "1: under 500 EUR/pounds"
df.socdem$income[which(grepl("2: ", df.socdem$income))] <- "2: 500 to 1,000 EUR/pounds"
df.socdem$income[which(grepl("3: ", df.socdem$income))] <- "3: 1,000 to 1,500 EUR/pounds"
df.socdem$income[which(grepl("4: ", df.socdem$income))] <- "4: 1,500 to 2,000 EUR/pounds"
df.socdem$income[which(grepl("5: ", df.socdem$income))] <- "5: 2,000 to 2,500 EUR/pounds"
df.socdem$income[which(grepl("6: ", df.socdem$income) | grepl("7: ", df.socdem$income) | grepl("8: 3", df.socdem$income) |
                     grepl("9: ", df.socdem$income) | grepl("10: ", df.socdem$income) |
                     grepl("11: ", df.socdem$income))] <- "6: Over 2,500 EUR/pounds"
df.socdem$income[which(grepl("98: ", df.socdem$income))] <- "98: no income"

# Number of children
df.socdem$children <- forcats::fct_collapse(df.socdem$children,
                                        "3: 3 Children and more" = c("3: 3 Children", "4: 4 Children",
                                                                     "5: 5 Children and more"))
df.socdem$children <- gsub('\\d+: ', '', df.socdem$children)

# Edit age categories
df.socdem$age_class <- gsub('\\d+: ', '', df.socdem$age_class)

exp_dat <- merge(df.survey[,c("panelist_id", "country", "reg_vote", "has.twitter", "has.facebook", "has.instagram",
                            "has.linkedin", "has.oth.smedia", "voted", "change", "undecided", "polinterest", "polinterest.num",
                            "leftmidright", "leftmidright.num", "trust.EP", "trust.nat.pol")],
                  df.socdem[,c("panelist_id", "gender", "age_num", "age_class", "children", "income", "family",
                               "education", "ISCED")], by = "panelist_id", all.x = TRUE)





