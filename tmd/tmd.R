
# install.packages("BiocManager")
# BiocManager::install("rhdf5")

# setup -------------------------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))
library(formattable)
# library(btools)
library(rhdf5)


# constants ---------------------------------------------------------------
wsldir <- "/\\wsl.localhost"
pydir <- "Ubuntu/home/donboyd5/Documents/python_projects"
tmddir <- "tax-microdata-benchmarking/tax_microdata_benchmarking/storage/output"
stor_path <- fs::path(wsldir, pydir, tmddir)


# get cli data ------------------------------------------------------------

# tmd_renamed-21-#-#-#.csv

clidir <- "Ubuntu/home/donboyd5/Documents/python_projects/tmd_analysis/use_cli"
fname <- "tmd_renamed-21-#-#-#.csv"
fp <- path(wsldir, clidir, fname)

cliout <- vroom(fp)

cliout |> 
  summarise(weights=sum(s006),
            agi=sum(s006 * c00100),
            ti=sum(s006 * c04800), .by=data_source) |> 
  mutate(agi=formattable::comma(agi / 1000, digits = 3),
         ti=formattable::comma(ti / 1000, digits = 3))
  # mutate(tot=ifelse(name=="weight", 
  #                   tot,
  #                   formattable::comma(tot / 1000, digits = 3)))
  


# get targets data ----------------------------------------------------------------
tpath <- here::here("data", "potential_targets.csv")

ptargs <- readRDS(here::here("data", "potential_targets.rds"))
glimpse(ptargs)



# raw 2015 puf ------------------------------------------------------------

pufdir <- r"(E:\data\puf_files\puf2015)"
ppath <- fs::path(pufdir, "puf_2015.csv")

puf <- vroom(ppath)
puf2015 <- puf |> 
  btools::lcnames() |> 
  mutate(year=2015, s006=s006 / 100)
glimpse(puf2015)



# 2021 puf from Nikhil ----------------------------------------------------

puf2021 <- vroom(fs::path(stor_path, "puf_2021.csv")) |> 
  lcnames() |> 
  mutate(year=2021, s006=s006 / 100)
glimpse(puf2021) # 207,696
# skim(puf2021)


# weights analysis --------------------------------------------------------

wtcomp <- 
  left_join(puf2015 |>
              select(recid, s006_puf2015=s006),
            puf2021 |>
              select(recid, s006_puf2021=s006),
            by = join_by(recid)) |> 
  mutate(r2115=s006_puf2021 / s006_puf2015)

summary(wtcomp)
ht(wtcomp)

ns(tmd2021)
wtsall <- wtcomp |>
  left_join(tmd2021 |> 
              select(recid=RECID, 
                     s006_tmd2021=s006,
                     s006_original),
            by = join_by(recid))

summary(wtsall)
# skim(wtsall)
ht(wtsall)


# get raw tmd files --------------------------------------------------------
# \\wsl.localhost\Ubuntu\home\donboyd5\Documents\python_projects\tax-microdata-benchmarking\tax_microdata_benchmarking\storage\output
# tmd.csv.gz # 233412
# tmd_weights.csv.gz
# growfactors.csv
# puf_ecps_2021.csv.gz # 233412 -- appears run through tc
# puf_ecps_2023.csv.gz
# taxdata_puf_2023.csv.gz

tmd2021 <- vroom(fs::path(stor_path, "tmd_2021.csv"))
glimpse(tmd2021)
summary(tmd2021 |> select(RECID, s006, s006_original, c00100))

ns(puf2015)

tmd2021 |> 
  arrange(RECID) |> 
  filter(RECID >= 999000) |> 
  select(RECID, s006, s006_original, c01000) |> 
  left_join(puf2015 |> 
              select(RECID=recid, s006_puf2015=s006, e00100, e01000),
            by = join_by(RECID))
  

wtsraw <- vroom(fs::path(stor_path, "tmd_weights.csv.gz"))
wtsums <- wtsraw |> 
  mutate(recnum=row_number()) |> 
  pivot_longer(-recnum, names_to = "year", values_to = "weight") |> 
  mutate(year=as.integer(str_sub(year, 3, 6)),
         weight=weight / 100.) |> 
  summarise(weight=sum(weight), .by=year)

wtsraw2 <- wtsraw |> 
  mutate(recnum=row_number()) |> 
  pivot_longer(-recnum, names_to = "year", values_to = "weight") |> 
  mutate(year=as.integer(str_sub(year, 3, 6)),
         weight=weight / 100.) 


wtsums |> 
  ggplot(aes(year, weight)) +
  geom_point() +
  geom_line()

wtsums |> 
  arrange(year) |> 
  mutate(pch=weight / lag(weight) - 1) |> 
  ggplot(aes(year, pch)) +
  geom_point(colour="blue") +
  geom_line(colour="blue") +
  scale_x_continuous(breaks = c(seq(2020, 2030, 2), seq(2030, 2100, 5))) +
  scale_y_continuous(breaks=seq(-.1, .1, .001), labels = scales::percent_format(accuracy=.1)) +
  geom_hline(yintercept = 0) +
  ggtitle("Percent change in sum of tmd weights by year")

ggplot(df, aes(x = as.factor(year), y = value)) +
  geom_boxplot() +
  labs(x = "Year", y = "Value", title = "Distribution of Value Across Years") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


tmdraw <- vroom(fs::path(stor_path, "tmd.csv.gz")) # raw tmd, 2021
ns(tmdraw)
glimpse(tmdraw)
sum(tmdraw$e00200 * tmdraw$s006) # 9.915791e+12
sum(tmdraw$s006) # 219,593,696
wtsums$weight[wtsums$year==2021] # 219,593,696.44


p <- wtsraw2 |> 
  ggplot(aes(x = as.factor(year), y = weight)) +
  geom_boxplot() +
  labs(x = "Year", y = "Value", title = "Distribution of Value Across Years") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

p + scale_y_continuous(limits=c(-1000, 5e3))


pufcps2021 <- vroom(fs::path(stor_path, "puf_ecps_2021.csv.gz")) # tmd through tc, 2021
ns(pufcps2021)
sum(pufcps2021$e00200 * pufcps2021$s006) # 9.915889e+12
sum(pufcps2021$s006) # 219,593,697
sum(pufcps2021$c01000 * pufcps2021$s006)

pufcps2023 <- vroom(fs::path(stor_path, "puf_ecps_2023.csv.gz")) # tmd through tc, 2023
ns(pufcps2023)
sum(pufcps2023$e00200 * pufcps2023$s006) # 1.124855e+13
sum(pufcps2023$s006) # 217,889,255 huh??
wtsums$weight[wtsums$year==2023] # 222,393,767.63

# 10,310,664,684,015.027 e00200 of something (in NW tests)


# get tmd after tc  -------------------------------------------------------------------------

# fp <- "/\\wsl.localhost\\Ubuntu\\home\\donboyd5\\Documents\\python_projects\\tmd_analysis\\data\\results\\tmdout_2021.csv" # good
# fp <- "//wsl.localhost/Ubuntu/home/donboyd5/Documents/python_projects/tmd_analysis/data/results/tmdout_2021.csv" # good

projdir <- "Ubuntu/home/donboyd5/Documents/python_projects/tmd_analysis"
filedir <- "data/results"
fname <- "tmdout_2021.csv"
fp <- path(wsldir, projdir, filedir, fname)

tmd1 <- read_csv(fp)
glimpse(tmd1)
ns(tmd1)

count(tmd1,data_source)
# data_source      n
# <dbl>  <int>
# 1           0  25716
# 2           1 207696  good, matches puf
sum(tmd1$s006)
sum(tmd1$e00200 * tmd1$s006) # 9.915791e+12

# constants ---------------------------------------------------------------

# use ycut19 for filers, all (?) IRS tables, and taxpayers tab11
ycut19 <- c(-Inf, 1, 
            seq(5e3, 30e3, 5e3),
            40e3, 50e3, 75e3, 100e3,
            200e3, 500e3,
            1e6, 1.5e6, 2e6, 5e6, 10e6, Inf)


# prep targets -----------------------------------------
targnames <- count(ptargs, vname)

dvars <- c("cgnet", "cgloss", "cggross", "pensions", "pensions_taxable", "unempcomp", "wages")
nvars <- paste0("nret_", dvars)
(vars <- c(dvars, nvars))

ptargs2 <- ptargs |> 
  filter(table=="tab14", year==2021, datatype=="filers") |> 
  filter(vname %in% vars) |> 
  select(incsort, incrange, vname, ptarget) |> 
  mutate(ptarget=ifelse(str_detect(vname, "nret_"), ptarget, ptarget * 1000.)) |> 
  pivot_wider(names_from = vname, values_from = ptarget, values_fill = 0) |> 
  mutate(cgnet=cggross - cgloss,
         nret_cgnet=nret_cggross + nret_cgloss) |> 
  pivot_longer(-c(incsort, incrange))
ptargs2
count(ptargs2, name)

ptargs2 |> 
  filter(incsort==1, str_detect(name, "pension"))

# prep tmd file ----
tmd2 <- tmd1 |> 
  rename(recnum=1) |> 
  select(recnum, c00100, c01000, e00200, e01400, e01500, e01700, e02300, s006)
glimpse(tmd2)

# irstottax=sum(s006 * irstottax),

# e02300	unempcomp
# e02300_nnz	nret_unempcomp
# e01500	pensions
# e01500_nnz	nret_pensions
# e01700 pensions_taxable




collapse <- tmd2 |>
  mutate(ycut=cut(c00100, ycut19, right=FALSE),
         incsort=as.integer(ycut) + 1L) |> # incsort will match with targets file
  summarise(nret = sum(s006), 
            agi = sum(s006 * c00100),
            wages = sum(s006 * e00200),
            pensions = sum(s006 * e01500),
            nret_pensions = sum(s006 * (e01500 != 0)),
            pensions_taxable = sum(s006 * e01700),
            nret_pensions_taxable = sum(s006 * (e01700 != 0)),
            nret_wages = sum(s006 * (e00200 != 0)),
            unempcomp = sum(s006 * e02300),
            nret_unempcomp = sum(s006 * (e02300 != 0)),
            cgnet = sum(s006 * c01000),
            nret_cgnet = sum(s006 * (cgnet != 0)),
            .by=c(incsort, ycut)) |>
  pivot_longer(cols=-c(incsort, ycut))

# concatentate totals by name
tots <- collapse |> 
  summarise(value=sum(value), .by=c(name)) |> 
  mutate(incsort=1)

combo <- bind_rows(collapse, tots) |> 
  arrange(name, incsort)

count(combo, name)

combo |> 
  filter(name=="pensions_taxable")



# combine targets and tmd -------------------------------------------------
mrg <- ptargs2 |> 
  filter(str_detect_any(name, c("cgnet", "pensions", "pensions_taxable", "unempcomp",  "wages"))) |> 
  rename(target=value) |> 
  left_join(combo |> 
              rename(tmdcalc=value),
            by = join_by(incsort, name)) |> 
  relocate(name) |> 
  relocate(ycut, .after = incrange) |> 
  mutate(diff=tmdcalc - target,
         pdiff = diff / target) |> 
  arrange(name, incsort)

sums <- mrg |> 
  filter(incsort==1) |> 
  mutate(across(c(target, tmdcalc), \(x) formattable::comma(x, digits = 0)))
  

mrg |> 
  filter(name=="cgnet") |> 
  gt() |> 
  fmt_number(columns = c(target, tmdcalc, diff),
             decimals=2,
             scale=1e-9) |> 
  fmt_percent(columns = pdiff,
              decimals = 1)

mrg |> 
  filter(name=="pensions") |> 
  gt() |> 
  fmt_number(columns = c(target, tmdcalc, diff),
             decimals=2,
             scale=1e-9) |> 
  fmt_percent(columns = pdiff,
              decimals = 1)

mrg |> 
  filter(name=="pensions_taxable") |> 
  gt() |> 
  fmt_number(columns = c(target, tmdcalc, diff),
             decimals=2,
             scale=1e-9) |> 
  fmt_percent(columns = pdiff,
              decimals = 1)


mrg |> 
  filter(name=="wages") |> 
  gt() |> 
  fmt_number(columns = c(target, tmdcalc, diff),
             decimals=2,
             scale=1e-9) |> 
  fmt_percent(columns = pdiff,
              decimals = 1)

mrg |> 
  filter(name=="unempcomp") |> 
  gt() |> 
  fmt_number(columns = c(target, tmdcalc, diff),
             decimals=2,
             scale=1e-9) |> 
  fmt_percent(columns = pdiff,
              decimals = 1)


# calc totals -------------------------------------------------------------

tmdx <- tmd1 |> 
  filter(data_source==1) |> 
  rename(recnum=1) |> 
  select(recnum, data_source, c00100, e00200, e00300, e00600, e00650,
         e00900, c01000, 
         e01400, e01500, e01700,
         e02300, e02400, c02500, c04800, c05800, # c08800 does not exist
         e18400,
         e26270,
         s006) |> 
  mutate(weight=1) |> 
  pivot_longer(-c(recnum, s006)) |> 
  summarise(tot=sum(value * s006), .by=name) |>
  mutate(tot=formattable::comma(tot / 1000, digits = 3))
tmdx
formattable::comma(174185064, digits = 3)

# c01000	cgnet
# c01000_nnz	nret_cgnet
# c01000neg	cgloss
# c01000neg_nnz	nret_cgloss
# c01000pos	cggross
# c01000pos_nnz	nret_cggross

# df |> 
#   summarise(nnz=sum((c01000 !=0) * s006),
#             cgnet=sum(c01000 * s006),
#             .by=data_source) |>
#   mutate(across(c(nnz, cgnet), \(x) formattable::comma(x, digits = 0)))
# 
# f <- function(var){
#   df |> 
#     select(data_source, s006, value=all_of(var)) |> 
#     summarise(nnz=sum((value !=0) * s006),
#               value=sum(value * s006),
#               .by=data_source) |>
#     mutate(vname=var,
#            across(c(nnz, value), \(x) formattable::comma(x, digits = 0)))
# }
#   
# f("c01000")
# f("e02300")
# f("e02400")
# f("e01500")
# f("e26270")


# mutate(cgnet = formattable::comma(cgnet, digits = 0))
  



# test h5 files -----------------------------------------------------------

fpath <- fs::path(stor_path, "pe_puf_2015.h5")
fpath <- fs::path(stor_path, "pe_puf_2021.h5")


h5file <- H5Fopen(fpath)
h5ls(h5file) # contents
# Read a dataset
data <- h5read(h5file)
data <- h5read(h5file, "age")

contents <- h5ls(h5file) 
count(contents, dim)
count(contents, otype)
count(contents, dclass)

data_list <- list()

# Iterate over each dataset
for (i in 1:nrow(contents)) {
  dataset_name <- contents[i, "name"]
  group_name <- contents[i, "group"]
  full_name <- paste0(group_name, "/", dataset_name)
  
  # Read the dataset
  data_list[[dataset_name]] <- h5read(h5file, full_name)
}

# Combine the list into a DataFrame
data_df <- as.data.frame(data_list)

my_h5_files <- Sys.glob(fpath)

# Close the HDF5 file
H5Fclose(h5file)
