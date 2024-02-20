

# setup -------------------------------------------------------------------

source(here::here("r", "libraries.r"))
library(jsonlite)
library(tidyjson)


# locations ----------------------------------------------------------------
tddir <- r"(E:\data\taxdata-psl)"

# URL of tax-calculator variables json file
url <- "https://raw.githubusercontent.com/PSLmodels/Tax-Calculator/master/taxcalc/records_variables.json"


# get tax-calculator variables metatdata, save as rds ---------------------

jsdata <- fromJSON(url)
names(jsdata)
jsdata$read # 106
jsdata$calc # 103

allvars <- bind_rows(tibble(vtype="read", lst=jsdata$read),
                tibble(vtype="calc", lst=jsdata$calc)) |> 
  mutate(vname=names(lst)) |> 
  unnest_wider(col=lst) |>
  unnest_longer(col=form) |> 
  rename(formyears=form_id)|> 
  relocate(vname)

glimpse(allvars)
count(allvars, vtype)
count(allvars, type)
count(allvars, availability)

saveRDS(allvars, here::here("data", "tcvars.rds"))

# REAL START BEGINS HERE ----

# get previously saved tax-calculator variables metadata ----
tcvars <- readRDS(here::here("data", "tcvars.rds"))
ht(tcvars)

# get frozentax-calculator file for 2023 ----
tdfn <- "tc23.csv"
tdpath <- path(tddir, tdfn)
tddf <- vroom(tdpath) # Rows: 252868 Columns: 209
glimpse(tddf)

# get in-progress policy engine flat file for 2023 ----

## get the file modification date of the csv file using winrar because that will not change the date the way other utilities do ----
wrp <- r"(c:\Program Files\WinRAR\WinRAR.exe)"
pez <- path(tddir, "tax_microdata.csv.gz")
# un-winrar the file
command <- sprintf('"%s" x -ibck -o+ "%s" "*.*" "%s"', wrp, pez, tddir)
system(command, wait = TRUE)

# now that we have the file unzipped, get its date
pefn <- "tax_microdata.csv"
pepath <- path(tddir, pefn)
pedata <- file_info(pepath)
glimpse(pedata)
(pedate <- pedata$modification_time)
# we can delete the csv file if we want

## get the pe data directly from the gz file ----
pedf <- vroom(pez) # read directly from the gz file Rows: 155312 Columns: 62                                                                                                                                                                                        
glimpse(pedf)


# explore variables -------------------------------------------------------

# in td but not pe:
setdiff(names(tddf), names(pedf)) # 155 vars
names(tddf)
ns(tddf)

calcvars <- names(tddf)[11:48]
keep <- setdiff(names(tddf), calcvars)
setdiff(keep, names(pedf)) # 122 vars

# what's this about?
setdiff(names(pedf), names(tddf)) |> sort() # "e00800p" "e00800s" "e01500p" "e01500s" "e02300p" "e02300s" "e02400p" "e02400s"
# Nikhil will remove these variables


# construct stacked file against which we'll run baseline or reforms --------

goodvars <- intersect(names(tddf), names(pedf))

stack <- bind_rows(
  tddf |> mutate(src="tdall"),
  tddf |> select(any_of(goodvars)) |> mutate(src="td"),
  pedf |> select(any_of(goodvars)) |> mutate(src="pe")) |> 
  mutate(across(where(is.numeric), ~replace_na(., 0)))

skim(stack)

# quick check
stack |> 
  summarise(n=n(), nret=sum(s006), wages=sum(e00200 * s006) / 1e9, .by=src) |> 
  mutate(avgwage=wages * 1e9 / nret) |> 
  pivot_longer(cols= - src) |> 
  pivot_wider(names_from = src) |> 
  mutate(pdiff=pe / td - 1)


# save stacked file ----
stack |> 
  write_csv(here::here(tddir, "scratch", "stack.csv"))

# run baseline tax on stacked file ----
# if can't run due to permission error, delete dataframes created from output file and closeAllConnections()
# rm(sdf, sdf2)
# closeAllConnections()
cmd1 <- "C:/Users/donbo/anaconda3/Scripts/tc.exe"
args <- c(shQuote("E:/data/taxdata-psl/scratch/stack.csv"), "2023",
          "--dump",
          "--outdir", "E:/data/taxdata-psl/scratch/")
system2(cmd1, args)


# get and examine the stacked data ----------------------------------------
sfn <- "stack-23-#-#-#.csv"
spath <- path(tddir, "scratch", sfn)
sdf <- vroom(spath) |>  # Rows: sum Columns: 209
  mutate(src=stack$src)
glimpse(sdf)
count(sdf, src)

sdf |> 
  summarise(n=n(), nret=sum(s006), taxbc=sum(taxbc * s006) / 1e9, .by=src) |> 
  mutate(avgtaxbc=taxbc * 1e9 / nret) |> 
  pivot_longer(cols= - src) |> 
  pivot_wider(names_from = src) |> 
  mutate(pdiff=pe / td - 1)


# weighted aggregates, selected variables ---------------------------------
ns(pedf)

utcvars <- tcvars |> 
  select(vname, desc) |> 
  distinct() |> 
  add_case(vname=c("n", "nret"), desc=c("# records", "# returns (millions)"))

sdf |> 
  summarise(n=n(), 
            nret=sum(s006), 
            across(
              c(c00100, e00200, e00300, e00400, e00600, e00650, e00650, e00800, e00900, e01100, e01400, 
                e01500, e01700, e02100, e02300, e02400,
                e03150, e03210, e03270, e03300, e17500, e18400, e32800),
              ~sum(.x * s006) /1e9), .by=src) |> 
  pivot_longer(-src, names_to = "vname") |> 
  pivot_wider(names_from = src) |> 
  mutate(pemtd=pe - td, pdiff=pemtd / td) |> 
  left_join(utcvars, by = join_by(vname))
  





# tables by agi range -----------------------------------------------------

ycuts <- c(-Inf, -1e-99, 0, 25e3, 50e3, 100e3, 200e3, 500e3, 1e6, Inf)
sdf2 <- sdf |> 
  mutate(agirange=cut(c00100, ycuts))
count(sdf2, agirange)
  
tabdata <- sdf2 |> 
  summarise(n=n(), nret=sum(s006), taxbc=sum(taxbc * s006), .by=c(src, agirange)) |> 
  pivot_longer(cols= -c(src, agirange)) |> 
  arrange(src, name, agirange) |> 
  pivot_wider(names_from = c(name, src)) |> 
  select(agirange, starts_with(c("n_", "nret_", "taxbc_")), everything()) |> 
  arrange(agirange) |> 
  janitor::adorn_totals() |> 
  mutate(avgtaxbc_pe=taxbc_pe / nret_pe, avgtaxbc_td=taxbc_td / nret_td)

tabdata |> 
  gt() |> 
  tab_header(
    title = html("Comparison by AGI range of Policy Engine (pe), taxdata same variables (td), and taxdata all variables (tdall)"),
    subtitle = html("Tax year 2023")
  ) |>
  tab_spanner(columns = starts_with("n_"),
              label="# of records") |> 
  tab_spanner(columns = starts_with("nret_"),
              label=html("# of returns<br>(millions)")) |> 
  tab_spanner(columns = starts_with("taxbc_"),
              label=html("tax before credits<br>($ billions)")) |>   
  tab_spanner(columns = starts_with("avgtaxbc_"),
              label=html("average tax before credits<br>($ dollars)")) |> 
  fmt_number(columns=starts_with("n_"),
             decimals=0) |> 
  fmt_number(columns=starts_with("nret_"),
             scale=1e-6,
             decimals=1) |> 
  fmt_number(columns=starts_with("taxbc_"),
             scale=1e-9,
             decimals=1) |> 
  fmt_number(columns=starts_with("avgtaxbc_"),
             scale=1,
             decimals=0) 
  
  



# tax-calculator ----
# tc "d:/tcdir/puf_vs_csv/puf.csv" 2014 --dump --outdir "d:/tcdir/puf_vs_csv"

# tc tax_microdata.csv 2023 --dump

# the conda forge installation of tc.exe is at:
#    C:\Users\donbo\anaconda3\Scripts\tc.exe

cmd1 <- "C:/Users/donbo/anaconda3/Scripts/tc.exe"
args <- c(shQuote("E:/data/taxdata-psl/tax_microdata.csv"), "2023",
         "--dump",
         "--outdir", "E:/data/taxdata-psl/scratch/")
system2(cmd1, args)


# args <- c(shQuote("D:/tcdir/synth10syn20.csv"), "2013",
#           "--reform", "D:/Dropbox/RPrograms PC/OSPC/syndata4/tax_plans/brk4_1k_2013.json",
#           "--dump",
#           "--outdir", "D:/tcdir/")
# system2(cmd1, args)




# running tax-calculator from r ----
# Tax-Calculator user guide:
# https://pslmodels.github.io/Tax-Calculator/uguide.html

# Here is the tc CLI usage:
# tc INPUT TAXYEAR [--help]
# [--baseline BASELINE] [--reform REFORM] [--assump  ASSUMP]
# [--exact] [--tables] [--graphs]
# [--dump] [--dvars DVARS] [--sqldb] [--outdir OUTDIR]
# [--test] [--version]

# --dump causes all the input variables (including the ones understood by Tax-Calculator but not included in test.csv,
# which are all zero) and all the output variables calculated by Tax-Calculator to be included in the output file

# Create a dumpvars.txt text file like this:
# c00100 c62100 c09600 c05800 taxbc
# cat("c00100 c62100 c09600 c05800 taxbc", file="D:/tax_data/tc_testfiles/dumpvars.txt",sep=" ")


# first baseline, then reform
# tc "d:/tcdir/puf_vs_csv/puf.csv" 2014 --dump --outdir "d:/tcdir/puf_vs_csv"
# tc "d:/tcdir/puf_vs_csv/cps.csv" 2014 --dump --outdir "d:/tcdir/puf_vs_csv"

# tc test.csv 2020 runs tc on the file test.csv; it produces test-20-#-#-#.csv
#   first # symbol indicates we did not specify a baseline file
#   second # symbol indicates we did not specify a policy reform file
#   third # symbol indicates we did not specify an economic assumption file.

# variables included in the minimal output file include:
#   RECID (of filing unit in the input file), YEAR (specified when executing tc), WEIGHT (which is same as s006),
#   INCTAX (which is same as iitax), LSTAX (which is same as lumpsum_tax) and PAYTAX (which is same as payroll_tax)

# Tax-Calculator extrapolation: ----
# Per the user guide,
#   Tax-Calculator knows to extrapolate (or age) filing unit data in the cps.csv file to the specified tax year
# I assume this is true if the file is named puf.csv, also, but documentation does not say

# To do extrapolation manually, we will need:
#   our equivalent of the enhanced puf: puf_data/cps-matched-puf.csv (restricted so not online)
#   taxdata/puf_stage1/growfactors.csv and Tax-Calculator/taxcalc/growfactors.csv,
#     apply as in Tax-Calculator/taxcalc/records.py _extrapolate
#   our equivalent of taxdata/puf_stage2/puf_weights.csv
#   taxdata/puf_stage3/puf_ratios.csv and Tax-Calculator/taxcalc/puf_ratios.csv
#     apply as Tax-Calculator/in taxcalc/records.py _adjust
#  plus, useful to look at stage3_targets.csv but not needed


# To find directory for tc, in a linux command window type:
#   where tc.exe
# or in Windows powershell:
#   Get-Command tc.exe

# Build a Windows system command using the R command system2:
#   - needs to define the location of the tc.exe command
#   - to find this location, in a command window type: where tc.exe
#   - all directory and file locations must be specified fully, not relative to this project
#   - any directory or file names in the args component of system2 that have spaces must be shQuoted as shown below
#   - fine to quote them all, even if they don't have spaces, just to be safe
#   - DO NOT shQuote the cmd argument of system2

# Use system2 command: here is an example:
# cmd1 <- "C:/ProgramData/Anaconda3/Scripts/tc"
# args <- c(shQuote("D:/tcdir/synth10syn20.csv"), "2013",
#           "--reform", "D:/Dropbox/RPrograms PC/OSPC/syndata4/tax_plans/brk4_1k_2013.json",
#           "--dump",
#           "--outdir", "D:/tcdir/")
# system2(cmd1, args)

# Do not include --reform and its location if this is a baseline run

test_dir <- "D:/tax_data/tc_testfiles/"
test_path <- paste0(test_dir, "test.csv")
cmd1 <- "C:/ProgramData/Anaconda3/Scripts/tc"

# very simple run, baseline
args <- c(shQuote(test_path), "2020",
          "--outdir", shQuote(test_dir))

# baseline, dump all results
args <- c(shQuote(test_path), "2020",
          "--dump",
          "--outdir", shQuote(test_dir))

# A simple run - copy the line below, from C:, exactly as is, into a command window and run:
#   C:/ProgramData/Anaconda3/Scripts/tc "D:/tax_data/tc_testfiles/test.csv" 2020 --outdir "D:/tax_data/tc_testfiles/"
# when we construct the command in R, we must shQuote the directory names, so an R variable that contains this information
# will not look quite like the commented out line above.

# create a list of variables to put in the output file, if we do not want them all
dvars <- c("c00100", "c62100", "c09600", "c05800", "taxbc")
dvars_path <- "D:/tax_data/tc_testfiles/dumpvars.txt"
cat(dvars, file=dvars_path, sep=" ") # write the dvars file

args <- c(shQuote(test_path), "2020",
          "--dump",
          "--dvars", shQuote(dvars_path),
          "--outdir", shQuote(test_dir))

cmd1; args

system2(cmd1, args)
