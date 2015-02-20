# rename and select vars for 2006


ds06 <- plyr::rename(ds06, replace = c(  	
  "HHIDPN"="id_2006",
  "BIRTHYF"="BIRTHYFDis_2006",
  "BIRTHYD"="BIRTHYDis_2006",
  "BIRTHMO"="birthM_2006",
  "BIRTHYR"="birthY_2006",
  "DEGREE"="degree_2006",
  "FIRSTIW"="Firstiyr_2006",
  "GENDER"="female_2006",
  "HISPANIC"="Hispanic_2006",
  "IMMGYEAR"="Immgyear_2006",
  "OVHHIDC"="OldHRSPN_2006",
  "OVPNC"="OldHRSPN_2006",
  "OVRESULT"="OverlapCas_2006",
  "RACE"="race_2006",
  "SCHLYRS"="eduyears_2006",
  
  
  
  "USBORN"="usborn_2006",
  "WTCOHORT"="wbirthcohort_2006",
  "KCSR01"="CSR04_2006",
  "KN_INHH"="RHHold_2006",
  "KANYFINR"="FinRespHH_2006",
  
  
  
  "KANYFAMR"="FamResp_2006",
  "KFAM_RHP"="FamRespID_2006",
  
  
  
  "KIWLANG"="language_2006",
  "KIWMODE"="Intmode_2006",
  "KIWMONTH"="intmonth_2006",
  "KIWYEAR"="intyear_2006",
  "KMARST"="maritalstat_2006",
  "KNURSHM"="nurshm_2006",
  
  "KPROXY"="proxy_2006",
  
  
  
  
  "KPN_SP"="sppn_2006",
  
  
  
  
  
  "KPNHM"="spnursinghm_2006",
  "KMARSTD"="dermaritalstat_2006",
  "KMARSTF"="marstatflag_2006",
  "KMARSTA"="derpartner_2006",
  "KMARSTP"="marstatwpart_2006",
  "KPARTNR"="partnered_2006",
  
  
  "KHASNEWP"="hasnewp_2006",
  
  "KCSRF"="covrespond_2006",
  
  
  
  
  
  "KPROXYD"="proxyder_2006",
  "KPROXYR"="proxyrel_2006",
  "KPRVIW"="previnterview_2006",
  "KPRVIWMO"="preintmnth_2006",
  "KPRVIWYR"="preintyr_2006",
  "KA500"="intMonth_2006",
  "KA501"="intYr_2006",
  "KA002"="agreeInt_2006",
  "KA009"="proxy_2006",
  "KA010"="sameproxy_2006",
  "KA103"="proxyrel_2006",
  "KA011"="proxyCog_2006",
  "KA012"="language_2006",
  "KA019"="age_2006",
  "KA028"="nursH_2006",
  "KA065"="nursHmth_2006",
  "KA066"="nursHyr_2006",
  
  
  
  
  
  
  
  
  "KB002"="usborn_2006",
  "KB006"="arriveyr_2006",
  "KB014"="educ_2006",
  "KB017M"="degree_2006",
  "KB020"="ses_2006",
  "KB026"="FathEd_2006",
  "KB027"="momEd_2006",
  "KB028"="hispanic_2006",
  
  "KB033"="childev_2006",
  "KB034"="childliv_2006",
  "KB035"="military_2006",
  "KB038"="militarydis_2006",
  "KB045"="yrslivearea_2006",
  "KB050"="religion_2006",
  "KB082"="relServ_2006",
  "KB053"="relImport_2006",
  "KB054"="englishH_2006",
  "KB055"="marrynew_2006",
  "KB056"="marryyr_2006",
  "KB057"="marryyr_2006",
  "KB058"="divwidPW_2006",
  "KB059"="divwidmth_2006",
  "KB060"="divwidyr_2006",
  "KB061"="unmarried_2006",
  "KB065"="nummarry_2006",
  "KB066_1"="marry1yr_2006",
  "KB067_1"="marry1mth_2006",
  "KB068_1"="marry1end_2006",
  "KB070_1"="marry1yrs_2006",
  "KB066_2"="marry2yr_2006",
  "KB067_2"="marry2mth_2006",
  "KB068_2"="marry2end_2006",
  "KB070_2"="marry2yrs_2006",
  "KB066_3"="marry3yr_2006",
  "KB067_3"="marry3mth_2006",
  "KB068_3"="marry3end_2006",
  "KB070_3"="marry3yrs_2006",
  "KB066_4"="marry4yr_2006",
  "KB067_4"="marry4mth_2006",
  "KB068_4"="marry4end_2006",
  "KB070_4"="marry4yrs_2006",
  "KB063"="maritalstat_2006",
  "KB076"="demhelp_2006",
  
  
  
  
  
  
  
  "KC001"="rhealth_2006",
  "KC185"="diffreport_2006",
  "KC002"="comphlth_2006",
  "KC005"="highBP_2006",
  "KC006"="bpmed_2006",
  "KC008"="bpmanaged_2006",
  "KC009"="bpworse_2006",
  "KC211"="bpchecked_2006",
  "KC212"="bpcheckedyr_2006",
  "KC010"="diabetes_2006",
  "KC214"="diabetesyr_2006",
  "KC011"="diabetespills_2006",
  "KC012"="insulin_2006",
  "KC015"="diabcontrol_2006",
  "KC016"="diabworse_2006",
  "KC017"="kidney_2006",
  "KC215"="sugartest_2006",
  "KC216"="sugartestyr_2006",
  "KC018"="cancer_2006",
  "KC023"="cancerworse_2006",
  "KC024"="newcancer_2006",
  "KC028"="canceryr_2006",
  "KC029"="cancermth_2006",
  "KC030"="lungdis_2006",
  "KC031"="lungworse_2006",
  "KC032"="lungmed_2006",
  "KC033"="lungoxy_2006",
  "KC034"="lungresther_2006",
  "KC035"="lungactive_2006",
  "KC036"="heartcond_2006",
  "KC037"="heartmed_2006",
  "KC039"="heartworse_2006",
  "KC040"="heartattack_2006",
  "KC042"="hrtattackmed_2006",
  "KC043"="heartattackyr_2006",
  "KC044"="heartattackmth_2006",
  "KC045"="angina_2006",
  "KC046"="anginamed_2006",
  "KC047"="anginalimit_2006",
  "KC048"="heartfail_2006",
  "KC049"="hospheartfail_2006",
  "KC050"="heartfailmed_2006",
  "KC051"="hearttreat_2006",
  "KC052"="heartsurg_2006",
  "KC053"="stroke_2006",
  "KC055"="strokeprob_2006",
  "KC060"="strokemed_2006",
  "KC061"="stroketherp_2006",
  "KC062"="strokeLW_2006",
  "KC064"="strokeyr_2006",
  "KC063"="strokemth_2006",
  "KC065"="psychprob_2006",
  "KC066"="psychworse_2006",
  "KC067"="psychtreat_2006",
  "KC068"="psychmeds_2006",
  "KC069"="memorydis_2006",
  "KC070"="arthritis_2006",
  "KC071"="athritworse_2006",
  "KC074"="arthmed_2006",
  "KC075"="arthactivity_2006",
  "KC076"="jointrepl_2006",
  "KC218"="jointtype_2006",
  "KC219"="osteoarth_2006",
  "KC220"="rheumatoid_2006",
  "KC221"="gout_2006",
  "KC222"="arthinjury_2006",
  "KC079"="fall2yrs_2006",
  "KC080"="timefall_2006",
  "KC081"="fallinjury_2006",
  "KC082"="hipbroke_2006",
  "KC087"="incontience_2006",
  "KC095"="eyesrate_2006",
  "KC098"="cataractsurg_2006",
  "KC101"="glaucoma_2006",
  "KC102"="hearaid_2006",
  "KC103"="hearingrate_2006",
  "KC083"="fallasleep_2006",
  "KC084"="wakenight_2006",
  "KC085"="wakeearl_2006",
  "KC086"="rested_2006",
  "KC104"="pain_2006",
  "KC105"="painrate_2006",
  "KC106"="painactivity_2006",
  "KC107"="othermed_2006",
  "KC223"="activityvig_2006",
  "KC224"="activitymod_2006",
  "KC225"="activitymild_2006",
  "KC116"="smokeEv_2006",
  "KC117"="smokecurrent_2006",
  "KC118"="numcig_2006",
  "KC125"="yrsquit_2006",
  "KC126"="yrquit_2006",
  "KC127"="agequit_2006",
  "KC128"="alcohol_2006",
  "KC129"="alcdays_2006",
  "KC130"="alcdrinks_2006",
  "KC131"="binge_2006",
  "KC134"="alcever_2006",
  "KC135"="CAGE1_2006",
  "KC136"="CAGE2_2006",
  "KC137"="CAGE3_2006",
  "KC138"="CAGE4_2006",
  "KC139"="weight_2006",
  "KC140"="changelbs_2006",
  "KC141"="heightft_2006",
  "KC142"="heightin_2006",
  "KC143"="feetswell_2006",
  "KC144"="breathshort_2006",
  "KC145"="dizzy_2006",
  "KC146"="backpain_2006",
  "KC147"="headache_2006",
  "KC148"="fatigue_2006",
  "KC149"="cough_2006",
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  "KD101"="rmemory_2006",
  "KD102"="pastmem_2006",
  "KD104"="wordlist_2006",
  "KD182M1"="wordIR1_2006",
  "KD182M2"="wordIR2_2006",
  "KD182M3"="wordIR3_2006",
  "KD182M4"="wordIR4_2006",
  "KD182M5"="wordIR5_2006",
  "KD182M6"="wordIR6_2006",
  "KD182M7"="wordIR7_2006",
  "KD182M8"="wordIR8_2006",
  "KD182M9"="wordIR9_2006",
  "KD182M10"="wordIR10_2006",
  
  
  "KD174"="wrdsImgood_2006",
  "KD175"="wrdsIwrong_2006",
  "KD176"="wordIforg_2006",
  "KD177"="nowordsIm_2006",
  "KD108M1"="wordprob1_2006",
  "KD108M2"="wordprob2_2006",
  "KD108M3"="wordprob4_2006",
  "KD108M4"="wordcheck_2006",
  "KD110"="cesd1_2006",
  "KD111"="cesd2_2006",
  "KD112"="cesd3_2006",
  "KD113"="cesd4_2006",
  "KD114"="cesd5_2006",
  "KD115"="cesd6_2006",
  "KD116"="cesd7_2006",
  "KD117"="cesd8_2006",
  "KD118"="cesd9_2006",
  "KD120"="count_2006",
  
  
  
  
  
  
  
  
  
  "KD142"="serial7s1_2006",
  "KD143"="serial7s2_2006",
  "KD144"="serial7s3_2006",
  "KD145"="serial7s4_2006",
  "KD146"="serial7s5_2006",
  "KD183M1"="wordDR1_2006",
  "KD183M2"="wordDR2_2006",
  "KD183M3"="wordDR3_2006",
  "KD183M4"="wordDR4_2006",
  "KD183M5"="wordDR5_2006",
  "KD183M6"="wordDR6_2006",
  "KD183M7"="wordDR7_2006",
  "KD183M8"="wordDR8_2006",
  "KD183M9"="wordDR9_2006",
  "KD183M10"="wordDR10_2006",
  
  
  
  
  
  
  "KD184"="wrdsDgood_2006",
  "KD185"="wrdsDwrong_2006",
  "KD186"="wordDforg_2006",
  "KD187"="nowordsDel_2006",
  "KD150"="intro_2006",
  "KD151"="qMonth_2006",
  "KD152"="qDay_2006",
  "KD153"="qYear_2006",
  "KD154"="qWeekday_2006",
  "KD155"="naming1_2006",
  "KD156"="naming2_2006",
  "KD157"="president_2006",
  "KD158"="vicepres_2006",
  "KD170"="TICScount_2006",
  "KD170A"="TICScount65_2006",
  "KD159"="vocabgiven_2006",
  "KD161"="vocab1_2006",
  "KD163"="vocab2_2006",
  "KD165"="vocab3_2006",
  "KD167"="vocab4_2006",
  "KD169"="vocab5_2006",
  "KD178"="numbers1_2006",
  "KD179"="numbers2_2006",
  "KD180"="numbers3_2006",
  "KD172"="needassist_2006",
  "KD171"="helpedcog_2006",
  "KD501"="proxycog1_2006",
  "KD502"="proxycog2_2006",
  "KD505"="proxycog3_2006",
  "KD506"="iqcode1 _2006",
  "KD507"="iqcode1I _2006",
  "KD508"="iqcode1w_2006",
  "KD509"="iqcode2_2006",
  "KD510"="iqcode2i_2006",
  "KD511"="iqcode2w_2006",
  "KD512"="iqcode3_2006",
  "KD513"="iqcode3i_2006",
  "KD514"="iqcode3w_2006",
  "KD515"="iqcode4_2006",
  "KD516"="iqcode4i_2006",
  "KD517"="iqcode4w_2006",
  "KD518"="iqcode5_2006",
  "KD519"="iqcode5i_2006",
  "KD520"="iqcode5w_2006",
  "KD521"="iqcode6_2006",
  "KD522"="iqcode6i_2006",
  "KD523"="iqcode6w_2006",
  "KD524"="iqcode7_2006",
  "KD525"="iqcode7i_2006",
  "KD526"="iqcode7w_2006",
  "KD527"="iqcode8_2006",
  "KD528"="iqcode8i_2006",
  "KD529"="iqcode8w_2006",
  "KD530"="iqcode9_2006",
  "KD531"="iqcode9i_2006",
  "KD532"="iqcode9w_2006",
  "KD533"="iqcode10_2006",
  "KD534"="iqcode10i_2006",
  "KD535"="iqcode10w_2006",
  "KD536"="iqcode11_2006",
  "KD537"="iqcode11i_2006",
  "KD538"="iqcode11w_2006",
  "KD539"="iqcode12_2006",
  "KD540"="iqcode12i_2006",
  "KD541"="iqcode12w_2006",
  "KD542"="iqcode13_2006",
  "KD543"="iqcode13i_2006",
  "KD544"="iqcode13w_2006",
  "KD545"="iqcode14_2006",
  "KD546"="iqcode14i_2006",
  "KD547"="iqcode14w_2006",
  "KD548"="iqcode15_2006",
  "KD549"="iqcode15i_2006",
  "KD550"="iqcode15w_2006",
  "KD551"="iqcode16_2006",
  "KD552"="iqcode16i_2006",
  "KD553"="iqcode16w_2006",
  "KD554"="getslost_2006",
  "KD555"="wanderoff_2006",
  "KD556"="leftalone_2006",
  "KD557"="hallucinate_2006"
  
  
))

keepvars06 <- c(
  "id_2006",
  "BIRTHYFDis_2006",
  "BIRTHYDis_2006",
  "birthM_2006",
  "birthY_2006",
  "degree_2006",
  "Firstiyr_2006",
  "female_2006",
  "Hispanic_2006",
  "Immgyear_2006",
  "OldHRSPN_2006",
  "OldHRSPN_2006",
  "OverlapCas_2006",
  "race_2006",
  "eduyears_2006",
  
  
  
  "usborn_2006",
  "wbirthcohort_2006",
  "CSR04_2006",
  "RHHold_2006",
  "FinRespHH_2006",
  
  
  
  "FamResp_2006",
  "FamRespID_2006",
  
  
  
  "language_2006",
  "Intmode_2006",
  "intmonth_2006",
  "intyear_2006",
  "maritalstat_2006",
  "nurshm_2006",
  
  "proxy_2006",
  
  
  
  
  "sppn_2006",
  
  
  
  
  
  "spnursinghm_2006",
  "dermaritalstat_2006",
  "marstatflag_2006",
  "derpartner_2006",
  "marstatwpart_2006",
  "partnered_2006",
  
  
  "hasnewp_2006",
  
  "covrespond_2006",
  
  
  
  
  
  "proxyder_2006",
  "proxyrel_2006",
  "previnterview_2006",
  "preintmnth_2006",
  "preintyr_2006",
  "intMonth_2006",
  "intYr_2006",
  "agreeInt_2006",
  "proxy_2006",
  "sameproxy_2006",
  "proxyrel_2006",
  "proxyCog_2006",
  "language_2006",
  "age_2006",
  "nursH_2006",
  "nursHmth_2006",
  "nursHyr_2006",
  
  
  
  
  
  
  
  
  "usborn_2006",
  "arriveyr_2006",
  "educ_2006",
  "degree_2006",
  "ses_2006",
  "FathEd_2006",
  "momEd_2006",
  "hispanic_2006",
  
  
  
  
  
  
  "religion_2006",
  "relServ_2006",
  "relImport_2006",
  "englishH_2006",
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  "rhealth_2006",
  "diffreport_2006",
  "comphlth_2006",
  "highBP_2006",
  "bpmed_2006",
  "bpmanaged_2006",
  "bpworse_2006",
  
  
  "diabetes_2006",
  "diabetesyr_2006",
  "diabetespills_2006",
  "insulin_2006",
  "diabcontrol_2006",
  "diabworse_2006",
  "kidney_2006",
  
  
  "cancer_2006",
  "cancerworse_2006",
  "newcancer_2006",
  "canceryr_2006",
  "cancermth_2006",
  "lungdis_2006",
  "lungworse_2006",
  "lungmed_2006",
  "lungoxy_2006",
  "lungresther_2006",
  "lungactive_2006",
  "heartcond_2006",
  "heartmed_2006",
  "heartworse_2006",
  "heartattack_2006",
  "hrtattackmed_2006",
  "heartattackyr_2006",
  "heartattackmth_2006",
  "angina_2006",
  "anginamed_2006",
  "anginalimit_2006",
  "heartfail_2006",
  "hospheartfail_2006",
  "heartfailmed_2006",
  "hearttreat_2006",
  "heartsurg_2006",
  "stroke_2006",
  "strokeprob_2006",
  "strokemed_2006",
  "stroketherp_2006",
  "strokeLW_2006",
  "strokeyr_2006",
  "strokemth_2006",
  "psychprob_2006",
  "psychworse_2006",
  "psychtreat_2006",
  "psychmeds_2006",
  "memorydis_2006",
  "arthritis_2006",
  "athritworse_2006",
  "arthmed_2006",
  "arthactivity_2006",
  "jointrepl_2006",
  "jointtype_2006",
  "osteoarth_2006",
  "rheumatoid_2006",
  "gout_2006",
  "arthinjury_2006",
  "fall2yrs_2006",
  "timefall_2006",
  "fallinjury_2006",
  "hipbroke_2006",
  "incontience_2006",
  "eyesrate_2006",
  "cataractsurg_2006",
  "glaucoma_2006",
  "hearaid_2006",
  "hearingrate_2006",
  "fallasleep_2006",
  "wakenight_2006",
  "wakeearl_2006",
  "rested_2006",
  "pain_2006",
  "painrate_2006",
  "painactivity_2006",
  "othermed_2006",
  "activityvig_2006",
  "activitymod_2006",
  "activitymild_2006",
  "smokeEv_2006",
  "smokecurrent_2006",
  "numcig_2006",
  "yrsquit_2006",
  "yrquit_2006",
  "agequit_2006",
  "alcohol_2006",
  "alcdays_2006",
  "alcdrinks_2006",
  "binge_2006",
  "alcever_2006",
  
  
  
  
  "weight_2006",
  
  "heightft_2006",
  "heightin_2006",
  
  "breathshort_2006",
  "dizzy_2006",
  "backpain_2006",
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  "rmemory_2006",
  "pastmem_2006",
  "wordlist_2006",
  "wordIR1_2006",
  "wordIR2_2006",
  "wordIR3_2006",
  "wordIR4_2006",
  "wordIR5_2006",
  "wordIR6_2006",
  "wordIR7_2006",
  "wordIR8_2006",
  "wordIR9_2006",
  "wordIR10_2006",
  
  
  "wrdsImgood_2006",
  
  "wordIforg_2006",
  "nowordsIm_2006",
  "wordprob1_2006",
  "wordprob2_2006",
  "wordprob4_2006",
  "wordcheck_2006",
  "cesd1_2006",
  "cesd2_2006",
  "cesd3_2006",
  "cesd4_2006",
  "cesd5_2006",
  "cesd6_2006",
  "cesd7_2006",
  "cesd8_2006",
  "cesd9_2006",
  "count_2006",
  
  
  
  
  
  
  
  
  
  "serial7s1_2006",
  "serial7s2_2006",
  "serial7s3_2006",
  "serial7s4_2006",
  "serial7s5_2006",
  "wordDR1_2006",
  "wordDR2_2006",
  "wordDR3_2006",
  "wordDR4_2006",
  "wordDR5_2006",
  "wordDR6_2006",
  "wordDR7_2006",
  "wordDR8_2006",
  "wordDR9_2006",
  "wordDR10_2006",
  
  
  
  
  
  
  "wrdsDgood_2006",
  "wrdsDwrong_2006",
  "wordDforg_2006",
  "nowordsDel_2006",
  "intro_2006",
  "qMonth_2006",
  "qDay_2006",
  "qYear_2006",
  "qWeekday_2006",
  "naming1_2006",
  "naming2_2006",
  "president_2006",
  "vicepres_2006",
  "TICScount_2006",
  "TICScount65_2006",
  "vocabgiven_2006",
  "vocab1_2006",
  "vocab2_2006",
  "vocab3_2006",
  "vocab4_2006",
  "vocab5_2006",
  "numbers1_2006",
  "numbers2_2006",
  "numbers3_2006",
  "needassist_2006",
  "helpedcog_2006",
  "proxycog1_2006",
  "proxycog2_2006",
  "proxycog3_2006",
  "iqcode1 _2006",
  "iqcode1I _2006",
  "iqcode1w_2006",
  "iqcode2_2006",
  "iqcode2i_2006",
  "iqcode2w_2006",
  "iqcode3_2006",
  "iqcode3i_2006",
  "iqcode3w_2006",
  "iqcode4_2006",
  "iqcode4i_2006",
  "iqcode4w_2006",
  "iqcode5_2006",
  "iqcode5i_2006",
  "iqcode5w_2006",
  "iqcode6_2006",
  "iqcode6i_2006",
  "iqcode6w_2006",
  "iqcode7_2006",
  "iqcode7i_2006",
  "iqcode7w_2006",
  "iqcode8_2006",
  "iqcode8i_2006",
  "iqcode8w_2006",
  "iqcode9_2006",
  "iqcode9i_2006",
  "iqcode9w_2006",
  "iqcode10_2006",
  "iqcode10i_2006",
  "iqcode10w_2006",
  "iqcode11_2006",
  "iqcode11i_2006",
  "iqcode11w_2006",
  "iqcode12_2006",
  "iqcode12i_2006",
  "iqcode12w_2006",
  "iqcode13_2006",
  "iqcode13i_2006",
  "iqcode13w_2006",
  "iqcode14_2006",
  "iqcode14i_2006",
  "iqcode14w_2006",
  "iqcode15_2006",
  "iqcode15i_2006",
  "iqcode15w_2006",
  "iqcode16_2006",
  "iqcode16i_2006",
  "iqcode16w_2006",
  "getslost_2006",
  "wanderoff_2006",
  "leftalone_2006",
  "hallucinate_2006"
  
)    