
tmp <- stack0 |> filter(type %in% c("taxdata", "pr148"))
ns(tmp)

tmp2 <- tmp |> 
  select(RECID, data_source, s006, s006_original, c00100, type)
ht(tmp2)

tmp2 |> 
  summarise(agi=sum(c00100 * s006), .by=c(data_source, type)) |> 
  pivot_wider(names_from = type, values_from = agi)


fpath <- fs::path(scratch_path, "tmd.csv.gz") # tmd after growth before taxcalc
tmd1 <- vroom(fpath) |> lcnames()
tmd1 |> 
  select(recid, s006, data_source, e00200) |>
  summarise(nret=sum(s006), 
            wages=sum(s006 * e00200),
            uwages=sum(e00200),
            .by=data_source)
# ds1 nret 162.267532 wages 9.03e12 uwages 125,989,660,332 $125b


fpath <- fs::path(scratch_path, "puf2011.csv") # raw puf 2011
puf1 <- vroom(fpath) |> lcnames()
puf1 |> 
  select(recid, s006, e00100, e00200) |> 
  mutate(s006=s006 / 100) |> 
  summarise(nret=sum(s006), 
            agi=sum(s006 * e00100),
            wages=sum(s006 * e00200),
            uwages=sum(e00200))
# in 2011: nret 145.163288  wages 6.05e12 uwages 35,974,107,506

fpath <- fs::path(scratch_path, "puf.csv") # taxdata taxcalc puf
puf2 <- vroom(fpath) |> lcnames()
puf2 |> 
  select(recid, s006, data_source, e00200) |> 
  mutate(s006=s006 / 100) |> 
  summarise(nret=sum(s006), 
            wages=sum(s006 * e00200),
            uwages=sum(e00200), .by=data_source)
# in 2015: ds1 nret 149.499059 wages 6.23e12 uwages 39,328,573,752

fpath <- fs::path(scratch_path, "puf-21-#-#-#.csv")
puf3 <- vroom(fpath) |> lcnames()
ns(puf3)
puf3 |> 
  select(recid, s006, data_source, c00100, e00200) |> 
  summarise(nret=sum(s006), 
            agi=sum(s006 * c00100),
            wages=sum(s006 * e00200),
            uwages=sum(e00200), .by=data_source)
# in 2021: ds1 nret 161.130969 wages 3.67e13 uwages 54,374,248,899
# over 10 years, unweighted wages grew 38.3%, weighted wages grew 489%


fpath <- fs::path(scratch_path, "td21.csv")
puf4 <- vroom(fpath) |> lcnames()
ns(puf4)
puf4 |> 
  select(recid, s006, data_source, c00100, e00200) |> 
  summarise(nret=sum(s006), 
            agi=sum(s006 * c00100),
            wages=sum(s006 * e00200),
            uwages=sum(e00200), .by=data_source)
# ds1 nret 161.130969. wages 9.05e12 uwages 54,374,277,656

puf34 <- bind_rows(puf3 |> mutate(type="dbtcout"),
                   puf4 |> mutate(type="mhtcout"))

puf34 |> 
  filter(data_source==1) |> 
  summarise(nret=sum(s006), 
            agi=sum(s006 * c00100),
            wages=sum(s006 * e00200),
            uwages=sum(e00200), .by=type)
# db wages out much greater than mh wages out, even though nret and uwages sums about same

puf34 |> 
  select(recid, s006, type) |> 
  pivot_wider(names_from = type, values_from = s006) |> 
  filter(dbtcout != mhtcout) # db and mh weights out of tc match

puf34 |> 
  select(recid, e00200, type) |> 
  pivot_wider(names_from = type, values_from = e00200) |> 
  filter(dbtcout != mhtcout) # db and mh wages out of tc NOT EVEN CLOSE


# ......

puf23 <- bind_rows(puf2 |> mutate(s006=s006 / 100,
                                  type="dbtcin"),
                   puf3 |> mutate(type="dbtcout"))

puf23 |> 
  filter(data_source==1) |> 
  summarise(nret=sum(s006), 
            wages=sum(s006 * e00200),
            uwages=sum(e00200), .by=type)

puf23 |> 
  select(recid, s006, type) |> 
  pivot_wider(names_from = type, values_from = s006) |> 
  mutate(ratio=dbtcout / dbtcin)

# wages all grow by 38%, loooks ok
puf23 |> 
  select(recid, e00200, type) |> 
  pivot_wider(names_from = type, values_from = e00200) |> 
  mutate(ratio=dbtcout / dbtcin)

# pstack ----

# variants of the 2011 puf
porder <- c("raw2011", "dbpufcsv2011", "dbtcout2021", "mhtcout2021")
pufstack <- bind_rows(
  puf1 |>  
    select(recid, s006, e00200) |> 
    mutate(s006=s006 / 100,
           type="raw2011"),
  puf2 |>
    filter(data_source==1) |> 
    select(recid, s006, e00200) |> 
    mutate(s006=s006 / 100,
           type="dbpufcsv2011"),
  puf3 |>
    filter(data_source==1) |> 
    select(recid, s006, e00200) |> 
    mutate(type="dbtcout2021"),
  puf4 |>
    filter(data_source==1) |> 
    select(recid, s006, e00200) |> 
    mutate(type="mhtcout2021"))

pufstack |> 
  summarise(nret=sum(s006), wages=sum(e00200 * s006), uwages=sum(e00200), .by=type)

pufstack |> 
  select(recid, s006, type) |> 
  pivot_wider(names_from = type, values_from = s006) # s006 looks ok

pufstack |> 
  select(recid, e00200, type) |> 
  pivot_wider(names_from = type, values_from = e00200) # 


check <- pufstack |> 
  select(recid, e00200, type) |> 
  pivot_wider(names_from = type, values_from = e00200) 
summary(check)
ht(check)
head(check, 20)

check |> 
  summarise(across(-recid, \(x) sum(x, na.rm=TRUE)))

# hypothesis -- in puf2011.csv --> puf.csv, td is mixing up who, in 2011, wages belong to, but not who s006 belongs to

