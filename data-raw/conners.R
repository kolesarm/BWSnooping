download.file("http://biostat.mc.vanderbilt.edu/wiki/pub/Main/DataSets/rhc.sav",
              "rhc.RData")
load("rhc.RData")

## Transform data as in Hirano and Imbens

## NA in cat2 means no secondary disease category, so include
## that as as level
levels(rhc$cat2) <- c(levels(rhc$cat2), "None")
rhc$cat2[is.na(rhc$cat2)] <- "None"
levels(rhc$ca) <- c("No", "Localized", "Metastatic")

## 2184 treated and 3551 controls., 5735 observations
d <- data.frame(rhc=(rhc$swang1=="RHC"),
                survival=(rhc$dth30=="Yes"),
                age=as.vector(rhc$age),
                sex=rhc$sex=="Female",
                ## 0 asians so drop that level
                race=droplevels(rhc$race),
                edu=as.vector(rhc$edu),
                income=rhc$income,
                insurance=rhc$ninsclas,
                cat1=droplevels(rhc$cat1),
                cat2=droplevels(rhc$cat2),
                ad_resp=(rhc$resp=="Yes"),
                ad_card=(rhc$card=="Yes"),
                ad_neuro=(rhc$neuro=="Yes"),
                ad_gastr=(rhc$gastr=="Yes"),
                ad_renal=(rhc$renal=="Yes"),
                ad_meta=(rhc$meta=="Yes"),
                ad_hema=(rhc$hema=="Yes"),
                ad_seps=(rhc$seps=="Yes"),
                ad_trauma=(rhc$trauma=="Yes"),
                ad_ortho=(rhc$ortho=="Yes"),
                das2d3pc=as.vector(rhc$das2d3pc),
                dnr1=(rhc$dnr1=="Yes"),
                cancer=rhc$ca,
                surv2md1=as.vector(rhc$surv2md1),
                aps1=as.vector(rhc$aps1),
                scoma1=as.vector(rhc$scoma1),
                wtkilo1=as.vector(rhc$wtkilo1),
                wt0=(rhc$wtkilo1==0),
                temp1=as.vector(rhc$temp1),
                meanbp1=as.vector(rhc$meanbp1),
                resp1=as.vector(rhc$resp1),
                hrt1=as.vector(rhc$hrt1),
                pafi1=as.vector(rhc$pafi1),
                paco21=as.vector(rhc$paco21),
                ph1=as.vector(rhc$ph1),
                wbc1=as.vector(rhc$wblc1),
                hema1=as.vector(rhc$hema1),
                sod1=as.vector(rhc$sod1),
                pot1=as.vector(rhc$pot1),
                crea1=as.vector(rhc$crea1),
                bili1=as.vector(rhc$bili1),
                alb1=as.vector(rhc$alb1),
                hx_cardio=(rhc$cardiohx==1),
                hx_chf=(rhc$chfhx==1),
                hx_dement=(rhc$dementhx==1),
                hx_psych=(rhc$psychhx==1),
                hx_cpd=(rhc$chrpulhx==1),
                hx_renal=(rhc$renalhx==1),
                hx_liver=(rhc$liverhx==1),
                hx_gibleed=(rhc$gibledhx==1),
                hx_malig=(rhc$malighx==1),
                hx_immuno=(rhc$immunhx==1),
                hx_ami=(rhc$amihx==1),
                hx_transfer=(rhc$transhx==1)
                )

rhc <- d

devtools::use_data(rhc, overwrite=TRUE, internal=FALSE)
