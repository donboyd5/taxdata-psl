


# setup -------------------------------------------------------------------

source(here::here("r", "libraries.r"))
source(here::here("r", "constants.r"))
source(here::here("r", "functions.r"))


# get var info ------------------------------------------------------------

tcvars <- readRDS(here::here("data", "tcvars.rds"))
# ht(tcvars)

utcvars <- tcvars |> 
  select(vname, vtype, desc) |> 
  distinct()

count(utcvars, vtype)

invars <- utcvars |> 
  filter(vtype=="read") |> 
  pull(vname)


# raw 2015 puf ------------------------------------------------------------

pufdir <- r"(E:\data\puf_files\puf2015)"
ppath <- fs::path(pufdir, "puf_2015.csv")

puf <- vroom(ppath)
puf2 <- puf |> 
  btools::lcnames() |> 
  mutate(year=2015, s006=s006 / 100)
skim(puf2)



# get data ----------------------------------------------------------------

tctd <- open_dataset(dpqtctd)
pufpe <- open_dataset(dpqpufpe)


# check -------------------------------------------------------------------

tctd |> 
  select(year, s006, e26270) |> 
  summarise(n=n(), wtdn=sum(s006), e26270=sum(e26270 * s006), .by=year) |> 
  collect()

pufpe |> 
  select(year, s006, e26270) |> 
  summarise(n=n(), wtdn=sum(s006), e26270=sum(e26270 * s006), .by=year) |> 
  collect()

ykeep <- 2015

stack <- bind_rows(
  pufpe |> 
    filter(year==ykeep) |> 
    select(year, any_of(invars)) |> 
    mutate(file="pufpe") |> 
    collect(),
  tctd |> 
    filter(year==ykeep) |> 
    select(year, any_of(invars)) |> 
    mutate(file="tctd") |> 
    collect(),
  puf2 |> 
    select(year, any_of(invars)) |> 
    mutate(file="puf2015"))

glimpse(stack)

wsums <- stack |> 
  select(file, year, s006, starts_with("e", ignore.case=FALSE), starts_with("p", ignore.case=FALSE)) |> 
  pivot_longer(-c(file, year, s006)) |> 
  summarise(wtdsum=sum(value * s006), .by=c(file, year, name)) |> 
  pivot_wider(names_from = file, values_from = wtdsum)
wsums

wsums2 <- wsums |> 
  left_join(utcvars |> 
              select(name=vname, desc),
            by = join_by(name)) |> 
  mutate(adiff=abs(pufpe - puf2015)) |> 
  arrange(desc(adiff))
wsums2

wsums2 |> 
  filter(pufpe==0) |> 
  arrange() |> 
  select(year, name, tctd, puf2015, pufpe, adiff, desc) |> 
  gt() |> 
  tab_header(title = html("'e' and 'p' variables that have zero values in the 2015 PUF-PolicyEngine file"),
             subtitle="Compared to raw 2015 PUF and 2015 taxdata file (grown from base year by Martin), $ billions") |> 
  fmt_number(columns = -c(year, name, desc),
             scale=1e-9,
             decimals=1) 


