
require(foreign)
# calls in the raw SPSS file and saves it in Rds for further processing
 ds0_1 <- read.spss(file = pathFile1, use.value.labels=TRUE)
 ds0_1 <- data.frame(ds0_1)
 saveRDS(object = ds0_1, file=pathFile1RDS, compress="xz")

# calls in the raw SPSS file and saves it in Rds for further processing
 ds0_2 <- read.spss(file=pathFile2, use.value.labels=TRUE)
 ds0_2 <- data.frame(ds0_2)
 saveRDS(object = ds0_2, file=pathFile2RDS, compress="xz")


# calls in the raw SPSS file and saves it in Rds for further processing
 ds0_3 <- read.spss(file=pathFile3, use.value.labels=TRUE)
 ds0_3 <- data.frame(ds0_3)
 saveRDS(object = ds0_3, file=pathFile3RDS, compress="xz")


# calls in the raw SPSS file and saves it in Rds for further processing
 ds0_4 <- read.spss(file = pathFile4, use.value.labels=TRUE)
 ds0_4 <- data.frame(ds0_4)
 saveRDS(object = ds0_4, file=pathFile4RDS, compress="xz")

# calls in the raw SPSS file and saves it in Rds for further processing
 ds0_5 <- read.spss(file=pathFile5, use.value.labels=TRUE)
 ds0_5 <- data.frame(ds0_5)
 saveRDS(object = ds0_5, file=pathFile5RDS, compress="xz")




