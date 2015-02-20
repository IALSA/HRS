# rename and select vars for 2010


ds10 <- plyr::rename(ds10, replace = c(  	
  "hhidpn"="id_2010",
  "birthyf"="BIRTHYFDis_2010",
  "birthyd"="BIRTHYDis_2010",
  "birthmo"="birthM_2010",
  "birthyr"="birthY_2010",
  "degree"="degree_2010",
  "firstiw"="Firstiyr_2010",
  "gender"="female_2010",
  "hispanic"="Hispanic_2010",
  "immgyear"="Immgyear_2010",
  "ovhhidc"="OldHRSPN_2010",
  "ovpnc"="OldHRSPN_2010",
  "ovresult"="OverlapCas_2010",
  "race"="race_2010",
  "schlyrs"="eduyears_2010",
  "secu"="sampleerr_2010",
  "stratum"="stratumid_2010",
  "study"="study_2010",
  "usborn"="usborn_2010",
  "wtcohort"="wbirthcohort_2010",
  "mcsr01"="CSR04_2010",
  "mn_inhh"="RHHold_2010",
  "manyfinr"="FinRespHH_2010",
  "mfin_rhp"="FinRespID_2010",
  "mfinr01"="FinResp04_2010",
  "mnofinr"="NoFinData_2010",
  "manyfamr"="FamResp_2010",
  "mfam_rhp"="FamRespID_2010",
  "mfamr01"="FamResp04_2010",
  "mnofamr"="NoFamData_2010",
  "mcores"="Coresstatus_2010",
  "miwlang"="language_2010",
  "miwmode"="Intmode_2010",
  "miwmonth"="intmonth_2010",
  "miwyear"="intyear_2010",
  "mmarst"="maritalstat_2010",
  "mnurshm"="nurshm_2010",
  "mppn"="spousepn_2010",
  "mproxy"="proxy_2010",
  "msubhhiw"="subhhint_2010",
  "mwgthh"="whyhhweight_2010",
  "mwgtr"="whyresweight_2010",
  
  "mpn_sp"="sppn_2010",
  "ma500"="intMonth_2010",
  "ma501"="intYr_2010",
  "ma002"="agreeInt_2010",
  "ma009"="proxy_2010",
  "ma010"="sameproxy_2010",
  "ma103"="proxyrel_2010",
  "ma011"="proxyCog_2010",
  "ma012"="language_2010",
  "ma019"="age_2010",
  "ma028"="nursH_2010",
  "ma065"="nursHmth_2010",
  "ma066"="nursHyr_2010",
  "ma068m"="regionm_2010",
  "mb002"="usborn_2010",
  "mb006"="arriveyr_2010",
  "mb014"="educ_2010",
  "mb017m"="degree_2010",
  "mb020"="ses_2010",
  "mb026"="FathEd_2010",
  "mb027"="momEd_2010",
  "mb028"="hispanic_2010",
  
  "mb033"="childev_2010",
  "mb034"="childliv_2010",
  "mb035"="military_2010",
  "mb038"="militarydis_2010",
  "mb045"="yrslivearea_2010",
  "mb050"="religion_2010",
  "mb082"="relServ_2010",
  "mb053"="relImport_2010",
  "mb054"="englishH_2010",
  "mb055"="marrynew_2010",
  "mb056"="marryyr_2010",
  "mb057"="marryyr_2010",
  "mb058"="divwidPW_2010",
  "mb059"="divwidmth_2010",
  "mb060"="divwidyr_2010",
  "mb061"="unmarried_2010",
  "mb065"="nummarry_2010",
  "mb066_1"="marry1yr_2010",
  "mb067_1"="marry1mth_2010",
  "mb068_1"="marry1end_2010",
  "mb070_1"="marry1yrs_2010",
  "mb066_2"="marry2yr_2010",
  "mb067_2"="marry2mth_2010",
  "mb068_2"="marry2end_2010",
  "mb070_2"="marry2yrs_2010",
  "mb066_3"="marry3yr_2010",
  "mb067_3"="marry3mth_2010",
  "mb068_3"="marry3end_2010",
  "mb070_3"="marry3yrs_2010",
  "mb066_4"="marry4yr_2010",
  "mb067_4"="marry4mth_2010",
  "mb068_4"="marry4end_2010",
  "mb070_4"="marry4yrs_2010",
  "mb063"="maritalstat_2010",
  "mb076"="demhelp_2010",
  "mc001"="rhealth_2010",
  "mc185"="diffreport_2010",
  "mc002"="comphlth_2010",
  "mc005"="highBP_2010",
  "mc006"="bpmed_2010",
  "mc008"="bpmanaged_2010",
  "mc009"="bpworse_2010",
  "mc211"="bpchecked_2010",
  "mc212"="bpcheckedyr_2010",
  "mc010"="diabetes_2010",
  "mc214"="diabetesyr_2010",
  "mc011"="diabetespills_2010",
  "mc012"="insulin_2010",
  "mc015"="diabcontrol_2010",
  "mc016"="diabworse_2010",
  "mc017"="kidney_2010",
  "mc215"="sugartest_2010",
  "mc216"="sugartestyr_2010",
  "mc018"="cancer_2010",
  "mc023"="cancerworse_2010",
  "mc024"="newcancer_2010",
  "mc028"="canceryr_2010",
  "mc029"="cancermth_2010",
  "mc030"="lungdis_2010",
  "mc031"="lungworse_2010",
  "mc032"="lungmed_2010",
  "mc033"="lungoxy_2010",
  "mc034"="lungresther_2010",
  "mc035"="lungactive_2010",
  "mc036"="heartcond_2010",
  "mc037"="heartmed_2010",
  "mc039"="heartworse_2010",
  "mc040"="heartattack_2010",
  "mc042"="hrtattackmed_2010",
  "mc043"="heartattackyr_2010",
  "mc044"="heartattackmth_2010",
  "mc045"="angina_2010",
  "mc046"="anginamed_2010",
  "mc047"="anginalimit_2010",
  "mc048"="heartfail_2010",
  "mc049"="hospheartfail_2010",
  "mc050"="heartfailmed_2010",
  "mc051"="hearttreat_2010",
  "mc052"="heartsurg_2010",
  "mc053"="stroke_2010",
  "mc055"="strokeprob_2010",
  "mc060"="strokemed_2010",
  "mc061"="stroketherp_2010",
  "mc062"="strokeLW_2010",
  "mc064"="strokeyr_2010",
  "mc063"="strokemth_2010",
  "mc065"="psychprob_2010",
  "mc066"="psychworse_2010",
  "mc067"="psychtreat_2010",
  "mc068"="psychmeds_2010",
  
  "mc070"="arthritis_2010",
  "mc071"="athritworse_2010",
  "mc074"="arthmed_2010",
  "mc075"="arthactivity_2010",
  "mc076"="jointrepl_2010",
  "mc218"="jointtype_2010",
  "mc219"="osteoarth_2010",
  "mc220"="rheumatoid_2010",
  "mc221"="gout_2010",
  "mc222"="arthinjury_2010",
  "mc079"="fall2yrs_2010",
  "mc080"="timefall_2010",
  "mc081"="fallinjury_2010",
  "mc082"="hipbroke_2010",
  "mc087"="incontience_2010",
  "mc095"="eyesrate_2010",
  "mc098"="cataractsurg_2010",
  "mc101"="glaucoma_2010",
  "mc102"="hearaid_2010",
  "mc103"="hearingrate_2010",
  "mc083"="fallasleep_2010",
  "mc084"="wakenight_2010",
  "mc085"="wakeearl_2010",
  "mc086"="rested_2010",
  "mc104"="pain_2010",
  "mc105"="painrate_2010",
  "mc106"="painactivity_2010",
  "mc107"="othermed_2010",
  "mc223"="activityvig_2010",
  "mc224"="activitymod_2010",
  "mc225"="activitymild_2010",
  "mc116"="smokeEv_2010",
  "mc117"="smokecurrent_2010",
  "mc118"="numcig_2010",
  "mc125"="yrsquit_2010",
  "mc126"="yrquit_2010",
  "mc127"="agequit_2010",
  "mc128"="alcohol_2010",
  "mc129"="alcdays_2010",
  "mc130"="alcdrinks_2010",
  "mc131"="binge_2010",
  "mc134"="alcever_2010",
  "mc135"="CAGE1_2010",
  "mc136"="CAGE2_2010",
  "mc137"="CAGE3_2010",
  "mc138"="CAGE4_2010",
  "mc139"="weight_2010",
  "mc140"="changelbs_2010",
  "mc141"="heightft_2010",
  "mc142"="heightin_2010",
  "mc143"="feetswell_2010",
  "mc144"="breathshort_2010",
  "mc145"="dizzy_2010",
  "mc146"="backpain_2010",
  "mc147"="headache_2010",
  "mc148"="fatigue_2010",
  "mc149"="cough_2010",
  "mc229"="C229_2010",
  "mc150"="C150_2010",
  "mc151"="C151_2010",
  "mc152"="C152_2010",
  "mc153"="C153_2010",
  "mc154"="C154_2010",
  "mc155"="C155_2010",
  "mc156"="C156_2010",
  "mc157"="C157_2010",
  "mc158"="C158_2010",
  "mc159"="C159_2010",
  "mc160"="C160_2010",
  "mc161"="C161_2010",
  "mc162"="C162_2010",
  "mc163"="C163_2010",
  "mc164"="C164_2010",
  "mc165"="C165_2010",
  "mc166"="C166_2010",
  "mc167"="C167_2010",
  "mc168"="C168_2010",
  "mc169"="C169_2010",
  "mc170"="C170_2010",
  "mc171"="C171_2010",
  "mc172"="C172_2010",
  "mc173"="C173_2010",
  "mc174"="C174_2010",
  "mc175"="C175_2010",
  "mc176"="C176_2010",
  "mc177"="C177_2010",
  "mc178"="C178_2010",
  "mc179"="C179_2010",
  "mc180"="C180_2010",
  "mc181"="C181_2010",
  "mc182"="C182_2010",
  "mc183"="C183_2010",
  "md101"="rmemory_2010",
  "md102"="pastmem_2010",
  "md104"="wordlist_2010",
  "md182m1"="wordIR1_2010",
  "md182m2"="wordIR2_2010",
  "md182m3"="wordIR3_2010",
  "md182m4"="wordIR4_2010",
  "md182m5"="wordIR5_2010",
  "md182m6"="wordIR6_2010",
  "md182m7"="wordIR7_2010",
  "md182m8"="wordIR8_2010",
  "md182m9"="wordIR9_2010",
  "md182m10"="wordIR10_2010",
  
  
  "md174"="wrdsImgood_2010",
  "md175"="wrdsIwrong_2010",
  "md176"="wordIforg_2010",
  "md177"="nowordsIm_2010",
  "md108m1"="wordprob1_2010",
  "md108m2"="wordprob2_2010",
  "md108m3"="wordprob4_2010",
  "md108m4"="wordcheck_2010",
  "md110"="cesd1_2010",
  "md111"="cesd2_2010",
  "md112"="cesd3_2010",
  "md113"="cesd4_2010",
  "md114"="cesd5_2010",
  "md115"="cesd6_2010",
  "md116"="cesd7_2010",
  "md117"="cesd8_2010",
  "md118"="cesd9_2010",
  "md120"="count_2010",
  
  
  
  
  
  
  
  
  
  "md142"="serial7s1_2010",
  "md143"="serial7s2_2010",
  "md144"="serial7s3_2010",
  "md145"="serial7s4_2010",
  "md146"="serial7s5_2010",
  "md183m1"="wordDR1_2010",
  "md183m2"="wordDR2_2010",
  "md183m3"="wordDR3_2010",
  "md183m4"="wordDR4_2010",
  "md183m5"="wordDR5_2010",
  "md183m6"="wordDR6_2010",
  "md183m7"="wordDR7_2010",
  "md183m8"="wordDR8_2010",
  "md183m9"="wordDR9_2010",
  "md183m10"="wordDR10_2010",
  
  
  
  
  
  
  "md184"="wrdsDgood_2010",
  "md185"="wrdsDwrong_2010",
  "md186"="wordDforg_2010",
  "md187"="nowordsDel_2010",
  "md150"="intro_2010",
  "md151"="qMonth_2010",
  "md152"="qDay_2010",
  "md153"="qYear_2010",
  "md154"="qWeekday_2010",
  "md155"="naming1_2010",
  "md156"="naming2_2010",
  "md157"="president_2010",
  "md158"="vicepres_2010",
  "md170"="TICScount_2010",
  
  "md159"="vocabgiven_2010",
  "md161"="vocab1_2010",
  "md163"="vocab2_2010",
  "md165"="vocab3_2010",
  "md167"="vocab4_2010",
  "md169"="vocab5_2010",
  "md178"="numbers1_2010",
  "md179"="numbers2_2010",
  "md180"="numbers3_2010",
  "md172"="needassist_2010",
  "md171"="helpedcog_2010",
  "md501"="proxycog1_2010",
  "md502"="proxycog2_2010",
  "md505"="proxycog3_2010",
  "md506"="iqcode1 _2010",
  "md507"="iqcode1I _2010",
  "md508"="iqcode1w_2010",
  "md509"="iqcode2_2010",
  "md510"="iqcode2i_2010",
  "md511"="iqcode2w_2010",
  "md512"="iqcode3_2010",
  "md513"="iqcode3i_2010",
  "md514"="iqcode3w_2010",
  "md515"="iqcode4_2010",
  "md516"="iqcode4i_2010",
  "md517"="iqcode4w_2010",
  "md518"="iqcode5_2010",
  "md519"="iqcode5i_2010",
  "md520"="iqcode5w_2010",
  "md521"="iqcode6_2010",
  "md522"="iqcode6i_2010",
  "md523"="iqcode6w_2010",
  "md524"="iqcode7_2010",
  "md525"="iqcode7i_2010",
  "md526"="iqcode7w_2010",
  "md527"="iqcode8_2010",
  "md528"="iqcode8i_2010",
  "md529"="iqcode8w_2010",
  "md530"="iqcode9_2010",
  "md531"="iqcode9i_2010",
  "md532"="iqcode9w_2010",
  "md533"="iqcode10_2010",
  "md534"="iqcode10i_2010",
  "md535"="iqcode10w_2010",
  "md536"="iqcode11_2010",
  "md537"="iqcode11i_2010",
  "md538"="iqcode11w_2010",
  "md539"="iqcode12_2010",
  "md540"="iqcode12i_2010",
  "md541"="iqcode12w_2010",
  "md542"="iqcode13_2010",
  "md543"="iqcode13i_2010",
  "md544"="iqcode13w_2010",
  "md545"="iqcode14_2010",
  "md546"="iqcode14i_2010",
  "md547"="iqcode14w_2010",
  "md548"="iqcode15_2010",
  "md549"="iqcode15i_2010",
  "md550"="iqcode15w_2010",
  "md551"="iqcode16_2010",
  "md552"="iqcode16i_2010",
  "md553"="iqcode16w_2010",
  "md554"="getslost_2010",
  "md555"="wanderoff_2010",
  "md556"="leftalone_2010",
  "md557"="hallucinate_2010",
  "mi800"="I800_2010",
  "mi802"="I802_2010",
  "mi854"="bpYN_2010",
  "mi855m1"="nobp1_2010",
  "mi855m2"="nobp2_2010",
  "mi855m3"="nobp3_2010",
  "mi855m4"="nobp4_2010",
  "mi857"="bptime_2010",
  "mi859"="bpsys1_2010",
  "mi860"="bpdia1_2010",
  "mi861"="bppulse1_2010",
  "mi862"="bptime2_2010",
  "mi864"="bpsys2_2010",
  "mi865"="bpdia2_2010",
  "mi866"="bppulse2_2010",
  "mi867"="bptime3_2010",
  "mi869"="bptimesys3_2010",
  "mi870"="bpdia3_2010",
  "mi871"="bppulse3_2010",
  
  
  "mi872"="bpArm_2010",
  "mi873"="bpComp_2010",
  "mi874"="bpposition_2010",
  "mi875"="bpsmoke_2010",
  "mi804"="breath_2010",
  "mi805m1"="I805M1_2010",
  "mi805m2"="I805M2_2010",
  "mi805m3"="I805M3_2010",
  "mi805m4"="I805M4_2010",
  "mi807"="puff1_2010",
  "mi808"="puff2_2010",
  "mi809"="puff3_2010",
  
  "mi810"="puffeffort_2010",
  "mi811"="puffpostition_2010",
  "mi812"="grip_2010",
  "mi813m1"="I813M1_2010",
  "mi813m2"="I813M2_2010",
  "mi813m3"="I813M3_2010",
  "mi813m4"="I813M4_2010",
  
  "mi815"="domhand_2010",
  "mi816"="gripLH1_2010",
  "mi851"="gripRH1_2010",
  "mi852"="gripLH2_2010",
  "mi853"="gripRH2_2010",
  
  
  "mi817"="gripeffort_2010",
  "mi818"="grippos_2010",
  "mi819"="I819_2010",
  "mi876"="balanceST_2010",
  "mi877m1"="I877M1_2010",
  "mi877m2"="I877M2_2010",
  "mi877m3"="I877M3_2010",
  "mi877m4"="I877M4_2010",
  "mi877m5"="I877M5_2010",
  "mi879"="balSTfulltime_2010",
  "mi880"="balSTtime_2010",
  "mi881"="balSTcomp_2010",
  "mi883"="balSBS_2010",
  "mi884m1"="I884M1_2010",
  "mi884m2"="I884M2_2010",
  "mi884m3"="I884M3_2010",
  "mi884m4"="I884M4_2010",
  "mi884m5"="I884M5_2010",
  
  "mi886"="balSBSfulltime_2010",
  "mi887"="balSBStime_2010",
  "mi888"="balSBScomp_2010",
  "mi889"="I889_2010",
  "mi891"="balSBScompli_2010",
  "mi893"="tandcomp_2010",
  "mi894m1"="I894M1_2010",
  "mi894m2"="I894M2_2010",
  "mi894m3"="I894M3_2010",
  "mi894m4"="I894M4_2010",
  "mi894m5"="I894M5_2010",
  "mi896"="tandfulltime_2010",
  "mi897"="tandtime_2010",
  "mi898"="tandcompens_2010",
  "mi899"="I899_2010",
  "mi902"="tandcompli_2010",
  "mi820"="walk_2010",
  "mi821m1"="I821M1_2010",
  "mi821m2"="I821M2_2010",
  "mi821m3"="I821M3_2010",
  "mi821m4"="I821M4_2010",
  "mi821m5"="I821M5_2010",
  "mi823"="walktime1_2010",
  "mi824"="walktime2_2010",
  
  "mi825"="walksurf_2010",
  "mi828"="walkaid_2010",
  "mi830"="walkeffort_2010",
  "mi831"="I831_2010",
  "mi832m1"="I832M1_2010",
  "mi832m2"="I832M2_2010",
  "mi832m3"="I832M3_2010",
  "mi832m4"="I832M4_2010",
  "mi832m5"="I832M5_2010",
  "mi834"="height_2010",
  
  "mi835"="I835_2010",
  "mi837"="I837_2010",
  "mi903"="I903_2010",
  "mi838"="I838_2010",
  "mi839m1"="I839M1_2010",
  "mi839m2"="I839M2_2010",
  "mi839m3"="I839M3_2010",
  "mi839m4"="I839M4_2010",
  "mi839m5"="I839M5_2010",
  "mi841"="weight_2010",
  
  "mi842"="I842_2010",
  "mi844"="I844_2010",
  "mi947"="I947_2010",
  "mi904"="I904_2010",
  "mi905m1"="I905M1_2010",
  "mi905m2"="I905M2_2010",
  "mi905m3"="I905M3_2010",
  "mi905m4"="I905M4_2010",
  "mi907"="waist_2010",
  
  "mi908m1"="I908_2010",
  "mi910"="I910_2010",
  "mi911"="I911_2010",
  "mi912"="I912_2010",
  "mi913"="I913_2010",
  "mi941m1"="I941M1_2010",
  "mi941m2"="I941M2_2010",
  "mi941m3"="I941M3_2010",
  "mi941m4"="I941M4_2010",
  "mi941m5"="I941M5_2010",
  "mi914"="I914_2010",
  "mi915"="I915_2010",
  "mi916m1"="I916M1_2010",
  "mi916m2"="I916M2_2010",
  "mi916m3"="I916M3_2010",
  "mi916m4"="I916M4_2010",
  "mi918"="I918_2010",
  "mi919m1"="I919M1_2010",
  "mi919m2"="I919M2_2010",
  "mi919m3"="I919M3_2010",
  "mi921"="I921_2010",
  "mi922"="I922_2010",
  "mi943m1"="I943M1_2010",
  "mi943m2"="I943M2_2010",
  "mi943m3"="I943M3_2010",
  "mi943m4"="I943M4_2010",
  "mi943m5"="I943M5_2010",
  
  "mi923"="blood_2010",
  "mi924m1"="I924M1_2010",
  "mi924m2"="I924M2_2010" 
  
))

keepvars10 <- c(
  "id_2010",
  "BIRTHYFDis_2010",
  "BIRTHYDis_2010",
  "birthM_2010",
  "birthY_2010",
  "degree_2010",
  "Firstiyr_2010",
  "female_2010",
  "Hispanic_2010",
  "Immgyear_2010",
  
  
  
  "race_2010",
  "eduyears_2010",
  
  
  "study_2010",
  "usborn_2010",
  "wbirthcohort_2010",
  "CSR04_2010",
  "RHHold_2010",
  "FinRespHH_2010",
  "FinRespID_2010",
  "FinResp04_2010",
  
  "FamResp_2010",
  "FamRespID_2010",
  "FamResp04_2010",
  
  "Coresstatus_2010",
  "language_2010",
  
  
  "intyear_2010",
  "maritalstat_2010",
  "nurshm_2010",
  "spousepn_2010",
  "proxy_2010",
  
  
  
  
  "sppn_2010",
  "intMonth_2010",
  "intYr_2010",
  "agreeInt_2010",
  "proxy_2010",
  "sameproxy_2010",
  "proxyrel_2010",
  "proxyCog_2010",
  "language_2010",
  "age_2010",
  "nursH_2010",
  "nursHmth_2010",
  "nursHyr_2010",
  "regionm_2010",
  "usborn_2010",
  "arriveyr_2010",
  "educ_2010",
  "degree_2010",
  "ses_2010",
  "FathEd_2010",
  "momEd_2010",
  "hispanic_2010",
  
  "childev_2010",
  "childliv_2010",
  
  
  
  "religion_2010",
  "relServ_2010",
  "relImport_2010",
  "englishH_2010",
  
  
  
  "divwidPW_2010",
  "divwidmth_2010",
  "divwidyr_2010",
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  "maritalstat_2010",
  
  "rhealth_2010",
  "diffreport_2010",
  "comphlth_2010",
  "highBP_2010",
  "bpmed_2010",
  "bpmanaged_2010",
  "bpworse_2010",
  "bpchecked_2010",
  "bpcheckedyr_2010",
  "diabetes_2010",
  "diabetesyr_2010",
  "diabetespills_2010",
  "insulin_2010",
  "diabcontrol_2010",
  "diabworse_2010",
  "kidney_2010",
  "sugartest_2010",
  "sugartestyr_2010",
  "cancer_2010",
  "cancerworse_2010",
  "newcancer_2010",
  "canceryr_2010",
  "cancermth_2010",
  "lungdis_2010",
  "lungworse_2010",
  "lungmed_2010",
  "lungoxy_2010",
  "lungresther_2010",
  "lungactive_2010",
  "heartcond_2010",
  "heartmed_2010",
  "heartworse_2010",
  "heartattack_2010",
  "hrtattackmed_2010",
  "heartattackyr_2010",
  "heartattackmth_2010",
  "angina_2010",
  "anginamed_2010",
  "anginalimit_2010",
  "heartfail_2010",
  "hospheartfail_2010",
  "heartfailmed_2010",
  "hearttreat_2010",
  "heartsurg_2010",
  "stroke_2010",
  "strokeprob_2010",
  "strokemed_2010",
  "stroketherp_2010",
  "strokeLW_2010",
  "strokeyr_2010",
  "strokemth_2010",
  "psychprob_2010",
  "psychworse_2010",
  "psychtreat_2010",
  "psychmeds_2010",
  
  "arthritis_2010",
  "athritworse_2010",
  "arthmed_2010",
  "arthactivity_2010",
  "jointrepl_2010",
  "jointtype_2010",
  "osteoarth_2010",
  "rheumatoid_2010",
  "gout_2010",
  "arthinjury_2010",
  "fall2yrs_2010",
  "timefall_2010",
  "fallinjury_2010",
  "hipbroke_2010",
  "incontience_2010",
  "eyesrate_2010",
  "cataractsurg_2010",
  "glaucoma_2010",
  "hearaid_2010",
  "hearingrate_2010",
  "fallasleep_2010",
  "wakenight_2010",
  "wakeearl_2010",
  "rested_2010",
  "pain_2010",
  "painrate_2010",
  "painactivity_2010",
  "othermed_2010",
  "activityvig_2010",
  "activitymod_2010",
  "activitymild_2010",
  "smokeEv_2010",
  "smokecurrent_2010",
  "numcig_2010",
  "yrsquit_2010",
  "yrquit_2010",
  "agequit_2010",
  "alcohol_2010",
  "alcdays_2010",
  "alcdrinks_2010",
  "binge_2010",
  "alcever_2010",
  
  
  
  
  "weight_2010",
  "changelbs_2010",
  "heightft_2010",
  "heightin_2010",
  "feetswell_2010",
  "breathshort_2010",
  "dizzy_2010",
  "backpain_2010",
  "headache_2010",
  "fatigue_2010",
  "cough_2010",
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  "rmemory_2010",
  "pastmem_2010",
  "wordlist_2010",
  "wordIR1_2010",
  "wordIR2_2010",
  "wordIR3_2010",
  "wordIR4_2010",
  "wordIR5_2010",
  "wordIR6_2010",
  "wordIR7_2010",
  "wordIR8_2010",
  "wordIR9_2010",
  "wordIR10_2010",
  
  
  "wrdsImgood_2010",
  "wrdsIwrong_2010",
  "wordIforg_2010",
  "nowordsIm_2010",
  "wordprob1_2010",
  "wordprob2_2010",
  "wordprob4_2010",
  "wordcheck_2010",
  "cesd1_2010",
  "cesd2_2010",
  "cesd3_2010",
  "cesd4_2010",
  "cesd5_2010",
  "cesd6_2010",
  "cesd7_2010",
  "cesd8_2010",
  "cesd9_2010",
  "count_2010",
  
  
  
  
  
  
  
  
  
  "serial7s1_2010",
  "serial7s2_2010",
  "serial7s3_2010",
  "serial7s4_2010",
  "serial7s5_2010",
  "wordDR1_2010",
  "wordDR2_2010",
  "wordDR3_2010",
  "wordDR4_2010",
  "wordDR5_2010",
  "wordDR6_2010",
  "wordDR7_2010",
  "wordDR8_2010",
  "wordDR9_2010",
  "wordDR10_2010",
  
  
  
  
  
  
  "wrdsDgood_2010",
  "wrdsDwrong_2010",
  "wordDforg_2010",
  "nowordsDel_2010",
  "intro_2010",
  "qMonth_2010",
  "qDay_2010",
  "qYear_2010",
  "qWeekday_2010",
  "naming1_2010",
  "naming2_2010",
  "president_2010",
  "vicepres_2010",
  "TICScount_2010",
  
  "vocabgiven_2010",
  "vocab1_2010",
  "vocab2_2010",
  "vocab3_2010",
  "vocab4_2010",
  "vocab5_2010",
  "numbers1_2010",
  "numbers2_2010",
  "numbers3_2010",
  "needassist_2010",
  "helpedcog_2010",
  "proxycog1_2010",
  "proxycog2_2010",
  "proxycog3_2010",
  "iqcode1 _2010",
  "iqcode1I _2010",
  "iqcode1w_2010",
  "iqcode2_2010",
  "iqcode2i_2010",
  "iqcode2w_2010",
  "iqcode3_2010",
  "iqcode3i_2010",
  "iqcode3w_2010",
  "iqcode4_2010",
  "iqcode4i_2010",
  "iqcode4w_2010",
  "iqcode5_2010",
  "iqcode5i_2010",
  "iqcode5w_2010",
  "iqcode6_2010",
  "iqcode6i_2010",
  "iqcode6w_2010",
  "iqcode7_2010",
  "iqcode7i_2010",
  "iqcode7w_2010",
  "iqcode8_2010",
  "iqcode8i_2010",
  "iqcode8w_2010",
  "iqcode9_2010",
  "iqcode9i_2010",
  "iqcode9w_2010",
  "iqcode10_2010",
  "iqcode10i_2010",
  "iqcode10w_2010",
  "iqcode11_2010",
  "iqcode11i_2010",
  "iqcode11w_2010",
  "iqcode12_2010",
  "iqcode12i_2010",
  "iqcode12w_2010",
  "iqcode13_2010",
  "iqcode13i_2010",
  "iqcode13w_2010",
  "iqcode14_2010",
  "iqcode14i_2010",
  "iqcode14w_2010",
  "iqcode15_2010",
  "iqcode15i_2010",
  "iqcode15w_2010",
  "iqcode16_2010",
  "iqcode16i_2010",
  "iqcode16w_2010",
  "getslost_2010",
  "wanderoff_2010",
  "leftalone_2010",
  "hallucinate_2010",
  
  
  "bpYN_2010",
  "nobp1_2010",
  "nobp2_2010",
  "nobp3_2010",
  "nobp4_2010",
  "bptime_2010",
  "bpsys1_2010",
  "bpdia1_2010",
  "bppulse1_2010",
  "bptime2_2010",
  "bpsys2_2010",
  "bpdia2_2010",
  "bppulse2_2010",
  "bptime3_2010",
  "bptimesys3_2010",
  "bpdia3_2010",
  "bppulse3_2010",
  
  
  "bpArm_2010",
  "bpComp_2010",
  "bpposition_2010",
  "bpsmoke_2010",
  "breath_2010",
  "I805M1_2010",
  "I805M2_2010",
  "I805M3_2010",
  "I805M4_2010",
  "puff1_2010",
  "puff2_2010",
  "puff3_2010",
  
  "puffeffort_2010",
  "puffpostition_2010",
  "grip_2010",
  
  
  
  
  
  "domhand_2010",
  "gripLH1_2010",
  "gripRH1_2010",
  "gripLH2_2010",
  "gripRH2_2010",
  
  
  "gripeffort_2010",
  "grippos_2010",
  "I819_2010",
  "balanceST_2010",
  
  
  
  
  
  "balSTfulltime_2010",
  "balSTtime_2010",
  "balSTcomp_2010",
  "balSBS_2010",
  
  
  
  
  
  
  "balSBSfulltime_2010",
  "balSBStime_2010",
  "balSBScomp_2010",
  "I889_2010",
  "balSBScompli_2010",
  "tandcomp_2010",
  
  
  
  
  
  "tandfulltime_2010",
  "tandtime_2010",
  "tandcompens_2010",
  "I899_2010",
  "tandcompli_2010",
  "walk_2010",
  
  
  
  
  
  "walktime1_2010",
  "walktime2_2010",
  
  "walksurf_2010",
  "walkaid_2010",
  "walkeffort_2010",
  
  
  
  
  
  
  "height_2010",
  
  
  
  
  
  
  
  
  
  
  "weight_2010",
  
  
  
  
  
  
  
  
  
  "waist_2010",
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  "blood_2010"
  
)    