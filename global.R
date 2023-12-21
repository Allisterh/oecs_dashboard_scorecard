


# analysis dfs 
oecs.long <- readRDS("./data/oecs.long.RDS") 
projectionValues <- readRDS("./data/projectionValues.RDS") %>% 
  filter(gr == 0.05)

# images and visualization dfs
formattableListFin <- readRDS("./data/formattableListFin.RDS") 
countryGaugeList <- readRDS("./data/countryGaugeList.RDS")
averageGaugeList <- readRDS("./data/averageGaugeList.RDS")
sdg.list2 <- readRDS("./data/sdg_list_tidy.RDS")

# replace with new indicator
progress_dat <- readRDS("data/2020_progress.RDS")
five_year <- readRDS("./data/five_year_value.RDS")

# ------------------------------------------
clist.avail <- countrycode(c("Montserrat", "St. Vincent and the Grenadines", "St. Kitts and Nevis", 
                             "Antigua and Barbuda","St. Lucia", "Dominica", "Grenada"), "country.name", "country.name")
clist.iso3c <- countrycode(clist.avail, "country.name", "iso3c")
current.year <- as.numeric(format(Sys.Date(), format = "%Y"))

# Set variable list for input selection and organization
var.list2 <- c(list("Economic indicators" =
                      list("GDP in market prices (current EC$) (millions)", 
                           "GDP per capita (current EC$)",
                           "Value added per formal employee (EC$)",
                           "Foreign direct investment, net inflows (% of GDP)",
                           "Exports of goods (current US$) (millions)",
                           "Exports of services (current US$) (millions)",
                           "Intra-regional exports of goods (current US$) (millions)",
                           "International tourism, receipts (current US$) (millions)",
                           "International tourism, receipts per arrival (current US$)",
                           "Agriculture, Livestock and Forestry, value added (current US$) (millions)",
                           "Fishing, value added (current US$) (millions)",
                           "Public Sector Debt to GDP (%)",
                           "Individuals using the Internet (% of population)")),
               list("Environment indicators" =
                      list("Marine protected areas (% of territorial waters)",
                           "Fish species, threatened",
                           "Renewable electricity output (% of total electricity output)",
                           "Forest area (% of land area)",
                           "Total greenhouse gas emissions (kt of CO2 equivalent)")),
               list("Social indicators" =
                      list("Formal employment-population ratio, 15+ (%)",
                           "Intentional homicides (per 100,000 people)",
                           "Rates of police-recorded offenses (robbery) (per 100,000 population)",
                           "Current health expenditure (% of GDP)",
                           "Out-of-pocket expenditure (% of current health expenditure)",
                           "Maternal mortality ratio (modeled estimate, per 100,000 live births)",
                           "Mortality rate, neonatal (per 1,000 live births)",
                           "Prevalence of obesity (BMI > 30) (% of adults)",
                           "Prevalence of obesity in children/adolescents aged 10-19 years (% total)",
                           "Preprimary enrollment (% gross)",
                           "Trained teachers in primary education (% of total teachers)",
                           "Trained teachers in secondary education (% of total teachers)",
                           "Government expenditure on education, total (% of GDP)")),
               list("Insufficient" =
                      list("Agriculture, forestry, and fishing, value added per worker (constant 2015 US$)",
                           "Unemployment, total (% of total labor force) (national estimate)",
                           "Unemployment, female (% of female labor force) (national estimate)",
                           "Employment to population ratio, 15+, total (%) (national estimate)",
                           "Employment to population ratio, 15+, female (%) (national estimate)",
                           "Poverty headcount ratio at $6.85 a day (2017 PPP) (% of population)",
                           "Mortality from CVD, cancer, diabetes or CRD between exact ages 30 and 70 (%)",
                           "Suicide mortality rate (age-adjusted per 100 000 population)",
                           "Tertiary enrollment (% gross)")))

oecs.long <- oecs.long %>% 
  filter(country %in% c(clist.avail, "OECS")) %>%
  filter(variable %in% c(var.list2 %>% unlist() %>% 
  as.character(), "Population projection"))

# Create lists with additional information ------------------

# Indicators which we want to decrease
lower.wanted <- c("Intentional homicides (per 100,000 people)", 
                  "Public Sector Debt to GDP (%)",
                  "Mortality from CVD, cancer, diabetes or CRD between exact ages 30 and 70 (%)",
                  "Poverty headcount ratio at $6.85 a day (2017 PPP) (% of population)", 
                  "Public Sector Debt to GDP (%)",
                  "Unemployment, total (% of total labor force) (national estimate)",
                  "Mortality rate, infant (per 1,000 live births)",
                  "Prevalence of obesity (BMI > 30) (% of adults)",
                  "Fish species, threatened",
                  "Poverty headcount ratio at $6.85 a day (2017 PPP) (% of population)",
                  "Unemployment, female (% of female labor force) (national estimate)",
                  "Total greenhouse gas emissions (kt of CO2 equivalent)",
                  "Out-of-pocket expenditure (% of current health expenditure)",
                  "Maternal mortality ratio (modeled estimate, per 100,000 live births)",
                  "Mortality rate, neonatal (per 1,000 live births)",
                  "Rates of police-recorded offenses (robbery) (per 100,000 population)",
                  "Noncommunicable diseases mortality rate (age-adjusted per 100 000 population)",
                  "Prevalence of obesity in children/adolescents aged 10-19 years (% total)",
                  "Suicide mortality rate (age-adjusted per 100 000 population)")

# Add in new prior data from github -------------------------------------------

oecs.long.envi <- oecs.long

# !!!!!!! REPLACE THIS LINK WITH ECCB RUN SCRIPT !!!!!!!!!
prior_dat <- read_csv("https://raw.githubusercontent.com/kate-chalmers/oecs-scorecard-data-collection/main/data/new_prior_dat.csv")

new_dat_tidy <- prior_dat %>%
  rename(variable = category) 

anti_joiners <- new_dat_tidy %>%
  mutate(iso3c = countrycode(country, "country.name","iso3c")) %>%
  select(variable, iso3c) %>%
  distinct()

oecs.long.new <- oecs.long.envi %>%
  merge(., new_dat_tidy %>% select(country, variable) %>% 
          distinct()) %>%
  group_by(country, variable) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  select(-year, -value) %>%
  merge(., new_dat_tidy) %>%
  group_by(country, variable) %>%
  mutate(vlast = ifelse(year == max(year), 1, 0)) %>%
  rbind(., oecs.long) %>%
  arrange(variable, iso3c, year)

oecs.long <- oecs.long.new %>%
  filter(!country == "OECS") %>%
  group_by(variable) %>%
  group_by(variable, iso3c, country, region, income) %>%
  complete(year = min(year):2020) %>%
  group_by(variable, iso3c) %>%
  mutate(value = zoo::na.approx(value,na.rm=F),
         value = zoo::na.locf(value)) %>% 
  group_by(variable, year) %>%
  mutate(value = mean(value,na.rm=T),
         country = "OECS",
         iso3c = "OECS",
         income = "Aggregate") %>%
  slice(1) %>%
  ungroup() %>%
  group_by(variable) %>%
  mutate(vlast = ifelse(year == max(year), 1, 0)) %>%
  ungroup() %>%
  rbind(., oecs.long.new %>% filter(!country == "OECS")) %>%
  arrange(variable, iso3c, year) %>%
  mutate(value = as.numeric(value))

new_start_point <- oecs.long %>%
  filter(iso3c == "OECS", !variable == "Population projection") %>%
  group_by(variable) %>%
  filter(year == max(year)) %>%
  select(variable, year, value) %>%
  ungroup() %>%
  mutate(value = ifelse(year < 2020, paste0(round(value,2), "<p style='font-size:8px;'> (",year,") </p>"), round(value, 2))) %>%
  select(Indicator = variable, "Starting point (2020)" = value) 

formattableListFin <- formattableListFin %>%
  select(-"Starting point (2020)") %>%
  left_join(., new_start_point, by = "Indicator") %>%
  relocate("Starting point (2020)", .after="Performance")

countryGaugeList <- oecs.long %>%
  filter(vlast == 1, !variable == "Population projection", 
         !iso3c == "OECS") %>%
  group_by(variable) %>%
  complete(country = clist.avail) %>%
  ungroup() %>%
  select(variable, iso3c, country, year, value) %>%
  mutate(year = ifelse(is.na(year), "No data available", year),
         label = paste0(country, "\n (", year, ")"),
         lower_val_wanted = ifelse(variable %in% lower.wanted, 1, 0)) %>%
  left_join(., countryGaugeList %>% 
  select(variable, iso3c, goal), by = c("variable", "iso3c")) %>%
  group_by(variable, iso3c) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  rename(current = value)  %>%
  mutate(
    goal = parse_number(goal),
    goal = ifelse(variable == "Fish species, threatened", round(goal, 0), goal),
    percent = ifelse(current < goal & lower_val_wanted == 0, current/goal,
                     ifelse(current < goal & lower_val_wanted == 1, 1,
                            ifelse(current > goal & lower_val_wanted == 0, 1, goal/current))),
    percent = percent * 100,
    percent = round(percent, digits=1),
    goal = prettyNum(round(goal, digits=2), big.mark = ","),
    current = prettyNum(round(current, digits=2), big.mark=",")) %>%
  mutate(iso3c = countrycode(country, "country.name", "iso3c", custom_match = c(Anguilla = "AIA"))) %>%
  select(-lower_val_wanted, -country) %>%
  select(variable, iso3c, goal, current, percent, label) 

averageGaugeList <- oecs.long %>%
  filter(vlast == 1, !variable == "Population projection", iso3c == "OECS") %>%
  select(variable, year, value) %>%
  mutate(lower.wanted = ifelse(variable %in% lower.wanted, 1, 0)) %>%
  left_join(., averageGaugeList %>% 
  select(variable, goal), by = c("variable")) %>%
  rename(last_year = year, current = value)  %>%
  mutate(goal = parse_number(goal),
         goal = ifelse(variable == "Fish species, threatened", round(goal, 0), goal)) %>%
  ungroup() %>%
  mutate(gauge = ifelse(lower.wanted == 1 & current < goal, 1,
                          ifelse(lower.wanted == 1 & current > goal, goal/current,
                                 ifelse(lower.wanted== 0 & current < goal, current/goal, 1))),
           gauge = round(gauge * 100, digits=1),
           goal = round(goal, digits=2),
           current = round(current, digits=2),
           goal = prettyNum(goal, big.mark=","),
           current = prettyNum(current, big.mark=",")) %>%
  select(variable, goal, last_year, current, gauge)

projection_dat_anti <- new_dat_tidy %>%
  mutate(growthrate = "GDP targeting rate", gr = 0.05) %>%
  group_by(country, variable) %>%
  complete(year = 2010:max(year)) %>%
  ungroup()

connect_dat_new <- projection_dat_anti %>%
  group_by(country, variable) %>%
  filter(year == max(year)) %>%
  ungroup()

projection_dat_new <- new_dat_tidy %>%
  mutate(growthrate = "Current performance", gr = 0.05) %>%
  rbind(., connect_dat_new)

projectionValues <- projectionValues %>%
  anti_join(., projection_dat_anti, by = c("country", "year", "variable")) %>%
  rbind(., projection_dat_new) %>%
  arrange(variable, country, year) 

projection_oecs_anti <- oecs.long %>%
  filter(country == "OECS", variable %in% unique(new_dat_tidy$variable)) %>%
  select(country, variable, year, value) %>%
  mutate(growthrate = "GDP targeting rate", gr = 0.05) %>%
  group_by(variable) %>%
  complete(year = 2010:max(year)) %>%
  ungroup()

connect_oecs_new <- projection_oecs_anti %>%
  group_by(variable) %>%
  filter(year == max(year)) %>%
  ungroup()

projection_oecs_new <- oecs.long %>%
  filter(country == "OECS", variable %in% unique(new_dat_tidy$variable)) %>%
  select(country, variable, year, value) %>%
  mutate(growthrate = "Current performance", gr = 0.05) %>%
  rbind(., connect_oecs_new)

projectionValues <- projectionValues %>%
  anti_join(., projection_oecs_anti, by = c("country", "year",  "variable")) %>%
  rbind(., projection_oecs_new) %>%
  arrange(variable, country, year) 

oecs.long$lower_val_wanted <- ifelse(oecs.long$variable %in% lower.wanted, 1, 0)

goals <- averageGaugeList %>%
  mutate(goal = parse_number(goal), iso3c = "OECS") %>%
  select(variable,iso3c, goal)

goals <- countryGaugeList %>%
  mutate(goal = parse_number(goal)) %>%
  select(variable,iso3c, goal) %>%
  rbind(goals, .)

previous_values <- oecs.long %>%
  filter(variable %in% c(var.list2[[1]], var.list2[[2]], var.list2[[3]], var.list2[[4]])) %>%
  filter(iso3c == "OECS") %>%
  group_by(variable) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  select(variable, lower_val_wanted, value) %>%
  merge(., goals, by = "variable") %>%
  mutate(percent = ifelse(value < goal & lower_val_wanted == 0, value/goal,
                          ifelse(value < goal& lower_val_wanted == 1, 1,
                                 ifelse(value > goal& lower_val_wanted == 0, 1, goal/value))),
         percent = round(percent * 100, 1)) %>%
  select(variable, "prev_pct" = percent)

five_year_value <- oecs.long %>%
  filter(variable %in% c(var.list2[[1]], var.list2[[2]], var.list2[[3]], var.list2[[4]])) %>%
  filter(iso3c %in% c(clist.iso3c, "OECS")) %>%
  group_by(variable,iso3c) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  select(variable, iso3c, lower_val_wanted, value) %>%
  merge(., goals, by = c("variable", "iso3c")) %>%
  mutate(percent = ifelse(value < goal & lower_val_wanted == 0, value/goal,
                          ifelse(value < goal& lower_val_wanted == 1, 1,
                                 ifelse(value > goal& lower_val_wanted == 0, 1, goal/value))),
         percent = round(percent * 100, 1)) %>%
  select(variable, iso3c, "prev_pct" = percent)


# Indicators that should not go above 100%
threshold.vars <- c("Trained teachers in secondary education (% of total teachers)",
                    "Trained teachers in primary education (% of total teachers)",
                    "Renewable electricity output (% of total electricity output)")

# Assign sources --------------
sources <- list("undoc" = c("Intentional homicides (per 100,000 people)", 
                            "Rates of police-recorded offenses (robbery) (per 100,000 population)"),
                "un.data" =c("Preprimary enrollment (% gross)"),
                "stat.office" = c("Unemployment, female (% of female labor force) (national estimate)",
                                  "Unemployment, total (% of total labor force) (national estimate)",
                                  "Employment to population ratio, 15+, total (%) (national estimate)",
                                  "Employment to population ratio, 15+, female (%) (national estimate)"),
                "paho" = c("Life expectancy at birth, total (years)", "Hospital beds (per 1,000 people)",
                           "Out-of-pocket expenditure (% of current health expenditure)",
                           "Maternal mortality ratio (modeled estimate, per 100,000 live births)",
                           "Noncommunicable diseases mortality rate (age-adjusted per 100 000 population)",
                           "Mortality rate, neonatal (per 1,000 live births)",
                           "Intentional homicides (per 100,000 people)",
                           "Suicide mortality rate (age-adjusted per 100 000 population)"),
                "who" = c("Prevalence of obesity in children/adolescents aged 10-19 years (% total)",
                          "Prevalence of obesity (BMI > 30) (% of adults)",
                          "Suicide mortality rate (age-adjusted per 100 000 population)"),
                "index.mundi" = c("Mortality rate, infant (per 1,000 live births)"),
                "comtrade" = c("Exports of goods (current US$) (millions)",
                               "Exports of services (current US$) (millions)",
                               "Intra-regional exports of goods (current US$) (millions)"),
                "eccb" = c("GDP in market prices (current EC$) (millions)", "Public Sector Debt to GDP (%)",
                           "International tourism, receipts per arrival (current US$)",
                           "International tourism, receipts (current US$) (millions)",
                           "Value added per formal employee (EC$)",
                           "Agriculture, forestry, and fishing, value added (current US$) (millions)",
                           "Exports of goods and services (current US$) (millions)",
                           "Exports of services (current US$) (millions)",
                           "Exports of goods (current US$) (millions)",
                           "Fishing, value added (current US$) (millions)", 
                           "GDP per capita (current EC$)",
                           "Formal employment-population ratio, 15+ (%)",
                           "Foreign direct investment, net inflows (BoP, current US$) (millions)",
                           "Agriculture, Livestock and Forestry, value added (current US$) (millions)"),
                "social.sec" = c("Formal employment-population ratio, 15+ (%)", 
                                 "Value added per formal employee (EC$)"),
                "IRENA" = c("Renewable electricity output (% of total electricity output)"),
                "wdi" = c("Agriculture, forestry, and fishing, value added per worker (constant 2015 US$)",
                          "Agriculture, forestry, and fishing, value added (current US$) (millions)",
                          "Individuals using the Internet (% of population)",
                          "Fish species, threatened",
                          "International tourism, receipts per arrival (current US$)",
                          "Employment to population ratio, 15+, total (%) (national estimate)",
                          "Government expenditure on education, total (% of GDP)",
                          "Total greenhouse gas emissions (kt of CO2 equivalent)",
                          "Current health expenditure (% of GDP)",
                          "Exports of goods and services (% of GDP)",
                          "Foreign direct investment, net inflows (% of GDP)",
                          "Forest area (% of land area)", 
                          "GDP per capita, PPP (constant 2017 international $)",
                          "Gross domestic product, constant prices (billions national currency)",
                          "Intentional homicides (per 100,000 people)", 
                          "International tourism, receipts (% of total exports)",
                          "Life expectancy at birth, total (years)",
                          "Marine protected areas (% of territorial waters)", 
                          "Mortality from CVD, cancer, diabetes or CRD between exact ages 30 and 70 (%)",
                          "Mortality rate, infant (per 1,000 live births)",
                          "Poverty headcount ratio at $6.85 a day (2017 PPP) (% of population)",
                          "Prevalence of overweight (% of adults)",
                          "Preprimary enrollment (% gross)", 
                          "Tertiary enrollment (% gross)",
                          "Unemployment, female (% of female labor force) (national estimate)",
                          "Employment to population ratio, 15+, female (%) (national estimate)",
                          "Trained teachers in primary education (% of total teachers)",
                          "Trained teachers in secondary education (% of total teachers)",
                          "Unemployment, total (% of total labor force) (national estimate)",
                          "Exports of goods and services (current US$) (millions)",
                          "International tourism, receipts (current US$) (millions)",
                          "Out-of-pocket expenditure (% of current health expenditure)",
                          "Maternal mortality ratio (modeled estimate, per 100,000 live births)",
                          "Mortality rate, neonatal (per 1,000 live births)"))

sources <- rbind(data.frame("variable" = sources[['wdi']], Source = "WDI"),
                 data.frame("variable" = sources[["undoc"]], Source = "UNODC"),
                 data.frame("variable" = sources[["un.data"]], Source = "UNESCO"),
                 data.frame("variable" = sources[["stat.office"]], Source = "Statistical offices"),
                 data.frame("variable" = sources[["index.mundi"]], Source = "Index Mundi"),
                 data.frame("variable" = sources[["paho"]], Source = "PAHO"),
                 data.frame("variable" = sources[["comtrade"]], Source = "UN Comtrade"),
                 data.frame("variable" = sources[["eccb"]], Source = "ECCB"),
                 data.frame("variable" = sources[["who"]], Source = "WHO"),
                 data.frame("variable" = sources[["IRENA"]], Source = "IRENA"),
                 data.frame("variable" = sources[["social.sec"]], Source = "Social security boards"))

sources <- aggregate(data=sources,Source~.,FUN=paste,collapse=", ")

oecs.long <- oecs.long %>% merge(., sources, by = "variable", all=T)

# Indicator icons ------------

renew.indic <- "Renewable electricity output (% of total electricity output)"
green.indic <- "Total greenhouse gas emissions (kt of CO2 equivalent)"

average_tags <- averageGaugeList %>% select(variable, goal) 

indicator.tags <- oecs.long %>%
  select(variable, Source) %>%
  distinct() %>%
  filter(variable %in% c(var.list2[[1]], var.list2[[2]], var.list2[[3]], var.list2[[4]])) %>%
  merge(., average_tags, by="variable") 

units <- list(
  "currency" = c("GDP in market prices (current EC$) (millions)",
                 "GDP per capita (current EC$)",
                 "Value added per formal employee (EC$)",
                 "Exports of goods (current US$) (millions)",
                 "Exports of services (current US$) (millions)",
                 "Intra-regional exports of goods (current US$) (millions)",
                 "International tourism, receipts (current US$) (millions)",                      
                 "International tourism, receipts per arrival (current US$)",                     
                 "Agriculture, Livestock and Forestry, value added (current US$) (millions)",     
                 "Fishing, value added (current US$) (millions)",
                 "Agriculture, forestry, and fishing, value added per worker (constant 2015 US$)"),
  "specific" = c("Fish species, threatened",
                 "Total greenhouse gas emissions (kt of CO2 equivalent)",
                 "Rates of police-recorded offenses (robbery) (per 100,000 population)",
                 "Maternal mortality ratio (modeled estimate, per 100,000 live births)",
                 "Mortality rate, neonatal (per 1,000 live births)",
                 "Intentional homicides (per 100,000 people)",
                 "Suicide mortality rate (age-adjusted per 100 000 population)"
  )
)

oecs.long <- oecs.long %>% arrange(variable, iso3c, year)

# targetList <- targetList[order(match(targetList$variable,c(var.list2[[1]], var.list2[[2]], var.list2[[3]], var.list2[[4]]))),]

countryGaugeList <- countryGaugeList %>%
  mutate(val = parse_number(current)) %>%
  mutate(current = ifelse(is.na(val), " ", current),
         goal = ifelse(is.na(val), " ", goal),
         percent = ifelse(is.na(val), 0, percent)) 

print("Global loaded")

