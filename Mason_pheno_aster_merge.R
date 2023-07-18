
#phenology data
names(df)

#aster data
names(dat)

#make 'Sample.ID' variable in dataframe 'dat' to match phenology data

# in format of 'Family.Unique.Block.ID'

#First replace "_" in Family.Unique with ".", and then add '.' to beginning of 'Block.ID", then merge

# replace '_' with '.'

dat$Family.Unique <- gsub('_', '.', dat$Family.Unique)

#add '.' to start of 'Block.ID'

dat$Block.ID <- paste(".", dat$Block.ID, sep="")

#now merge "Family.Unique" and "Block.ID"
dat$Sample.ID <- paste(dat$Family.Unique, dat$Block.ID)

#now remove space between components of Sample.ID

dat$Sample.ID <- gsub('\\s+', '', dat$Sample.ID)

#list of 'phenology' variable to add to aster data

#   These varibles of interest are from Zebs table, remaining variable (e.g., number of flowers,
#   number of fruits, seed mass) are already in aster data

phen<- df[c("Sample.ID","Germ.to.TrueLeaf", "No.Days.to.Germ", "No.Days.to.TrueLeaf", "No.Days.Germ.to.Flr",
            "no.Planting.to.DTFF", "Planting.to.DTFF.2017" ,"Germ.to.DTFF.2017", "Planting.to.DTB.2017" ,     
             "Germ.to.DTB.2017", "Planting.to.DatetoFrt.2017","Germ.to.DatetoFrt.2017", "Planting.to.DTFF.2018" ,    
            "Germ.to.DTFF.2018","Planting.to.DTB.2018" ,"Germ.to.DTB.2018", "Planting.to.DatetoFrt.2018",
            "Germ.to.Date.to.Frt.2018")]
          

final<- merge(dat, phen, by="Sample.ID", all.x = TRUE)

#remove "." from Block.ID


write.table(final, file="C:/Users/Mason Kulbaba/Dropbox/git/geum-aster/final_data.csv", sep=",", 
            quote = FALSE, row.names = FALSE)
