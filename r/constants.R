

# locations: freestanding constants --------------------------------------------------

tddir <- r"(E:\data\taxdata-psl)"
dyfiles <- fs::path(tddir, "year_files")
dpq <- fs::path(tddir, "parquet")

wrp <- r"(c:\Program Files\WinRAR\WinRAR.exe)"
pez <- path(tddir, "tax_microdata.csv.gz")


# plot items: freestanding ----------------------------------------------------
legend_none <- theme(legend.position = "None")
legend_notitle <- theme(legend.title = element_blank())
caption_left <- theme(plot.caption = element_text(hjust = 0))

x90 <- theme(axis.text.x = element_text(angle = -90, vjust = 0, hjust=0.5))

# constants list TBD ---------------------------------------------------------------


