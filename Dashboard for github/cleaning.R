##################################################################
#                             Jojo Emerson                       #
#                      GH CEA Registry Dashboard                 #
#                   Main directory: Data cleaning file           #
#                     Code changes: March 8, 2019                #
#                    GitHub Upload: April 18, 2019               #
#                             Version 1.0.3                      #
##################################################################

#call libraries
library(readxl)

#import methods, ratios
suppressWarnings({
  methods_raw<-read_excel("data/METHODS.xlsx")
  ratios_raw<-read_excel("data/RATIOS.xlsx")
})

#pull in GH CEA registry methods and ratios and merge datasets, 
suppressWarnings(merge_data<-merge(read_excel("data/RATIOS.xlsx"), read_excel("data/METHODS.xlsx"), by = "ArticleID"))

#subset data necessary for league tables
leaguetable_data<-merge_data[,c("Title.x", "PrimAuthFirstName.x", "PrimAuthLastName.x", "GBDDis1_tier1", "GBDDis1_tier2","GBDDis1_tier3", "GBDDis1_tier4",
                                "PubYear.x", "InterventionPhrase", "ComparatorPhrase","lessthan_onexGDP",	"cost_savingGDP","oneto_threexGDP",
                                "greaterthan_threexGDP",	"dominated_GDP", "DisplayRatio","DALYinCurrentUSDollarsRou", "Country.x", 
                                "SponsorshipAcademic", "SponsorshipGovernment", "SponsorshipIntergovernmentalOr", "SponsorshipFoundation",
                                "SponsorshipGatesFoundation", "SponsorshipPharmOrDevice", "SponsorshipHealthCare", "SponsorshipProfMembOrg", 
                                "SponsorshipNone", "SponsorshipNotDetermined", "SponsorshipOther", "PubYear.x", "GDP_percapita2016",
                                "SuperRegion_SubSaharanAfrica.x",	"SuperRegion_SoutheastAsiaOceania.x",	"SuperRegion_SouthAsia.x",	"SuperRegion_NorthAfricaMidEast.x",
                                "SuperRegion_HighIncome.x",	"SuperRegion_EuropeCentralAsia.x",	"SuperRegion_LatinAmerica.x",
                                "Intervention","PrimaryDisCode", "TargetPopulationGender", "TargetPopulationAge_05", "TargetPopulationAge_611", "TargetPopulationAge_1218",
                                "TargetPopulationAge_1940", "TargetPopulationAge_4164", "TargetPopulationAge_65", "TargetPopulationAge_adult", "TargetPopulationAge_unknown",
                                "TargetPopulationHealthText", "TargetPopulationOther", "GBDDis2_tier1", "GBDDis2_tier2","GBDDis2_tier3", "GBDDis2_tier4",
                                "GBDDis3_tier1", "GBDDis3_tier2","GBDDis3_tier3", "GBDDis3_tier4","GBDDis4_tier1", "GBDDis4_tier2","GBDDis4_tier3", "GBDDis4_tier4",
                                "GBDDis5_tier1", "GBDDis5_tier2","GBDDis5_tier3", "GBDDis5_tier4",
                                "Country_Afghanistan.x",	"Country_Albania.x",	"Country_Algeria.x",	"Country_AmericanSamoa.x",	"Country_Andorra.x",	"Country_Angola.x",
                                "Country_AntiguaandBarbuda.x",	"Country_Argentina.x",	"Country_Armenia.x",	"Country_Aruba.x",	"Country_Australia.x",	"Country_Austria.x",
                                "Country_Azerbaijan.x",	"Country_Bahamas.x",	"Country_Bahrain.x",	"Country_Bangladesh.x",	"Country_Barbados.x",	"Country_Belarus.x",	"Country_Belgium.x",
                                "Country_Belize.x",	"Country_Benin.x",	"Country_Bermuda.x",	"Country_Bhutan.x",	"Country_Bolivia.x",	"Country_BosniaandHerzegovina.x",	"Country_Botswana.x",
                                "Country_Brazil.x",	"Country_BritishVirginIslands.x",	"Country_Brunei.x",	"Country_Bulgaria.x",	"Country_BurkinaFaso.x",	"Country_Burundi.x",
                                "Country_CaboVerde.x",	"Country_Cambodia.x",	"Country_Cameroon.x",	"Country_Canada.x",	"Country_CaymanIslands.x",	"Country_CentralAfricanRepublic.x",
                                "Country_Chad.x",	"Country_ChannelIslands.x",	"Country_Chile.x",	"Country_China.x",	"Country_Colombia.x",	"Country_Comoros.x",	"Country_CongoDemRep.x",
                                "Country_CongoRep.x",	"Country_CostaRica.x",	"Country_CotedIvoire.x",	"Country_Croatia.x",	"Country_Cuba.x",	"Country_Curaco.x",	"Country_Cyprus.x",
                                "Country_CzechRepublic.x",	"Country_Denmark.x",	"Country_Djibouti.x",	"Country_Dominica.x",	"Country_DominicanRepublic.x",	"Country_Ecuador.x",
                                "Country_EgyptArabRep.x",	"Country_ElSalvador.x",	"Country_EquatorialGuinea.x",	"Country_Eritrea.x",	"Country_Estonia.x",	"Country_Ethiopia.x",
                                "Country_FaroeIslands.x",	"Country_Fiji.x",	"Country_Finland.x",	"Country_France.x",	"Country_FrenchPolynesia.x",	"Country_Gabon.x",	"Country_GambiaThe.x",
                                "Country_Georgia.x",	"Country_Germany.x",	"Country_Ghana.x",	"Country_Gibraltar.x",	"Country_Greece.x",	"Country_Greenland.x",	"Country_Grenada.x",
                                "Country_Guam.x",	"Country_Guatemala.x",	"Country_Guinea.x",	"Country_GuineaBissau.x",	"Country_Guyana.x",	"Country_Haiti.x",	"Country_Honduras.x",
                                "Country_HongKongSARChina.x",	"Country_Hungary.x",	"Country_Iceland.x",	"Country_India.x",	"Country_Indonesia.x",	"Country_IranIslamicRep.x",	"Country_Iraq.x",
                                "Country_Ireland.x",	"Country_IsleofMan.x",	"Country_Israel.x",	"Country_Italy.x",	"Country_Jamaica.x",	"Country_Japan.x",	"Country_Jordan.x",
                                "Country_Kazakhstan.x",	"Country_Kenya.x",	"Country_Kiribati.x",	"Country_KoreaDemPeoplesRep.x",	"Country_KoreaRep.x",	"Country_Kosovo.x",	"Country_Kuwait.x",
                                "Country_KyrgyzRepublic.x",	"Country_LaoPDR.x",	"Country_Latvia.x",	"Country_Lebanon.x",	"Country_Lesotho.x",	"Country_Liberia.x",	"Country_Libya.x",
                                "Country_Liechtenstein.x",	"Country_Lithuania.x",	"Country_Luxembourg.x",	"Country_MacaoSARChina.x",	"Country_MacedoniaFYR.x",	"Country_Madagascar.x",
                                "Country_Malawi.x",	"Country_Malaysia.x",	"Country_Maldives.x",	"Country_Mali.x",	"Country_Malta.x",	"Country_MarshallIslands.x",	"Country_Mauritania.x",
                                "Country_Mauritius.x",	"Country_Mexico.x",	"Country_MicronesiaFedSts.x",	"Country_Moldova.x",	"Country_Monaco.x",	"Country_Mongolia.x",	"Country_Montenegro.x",
                                "Country_Morocco.x",	"Country_Mozambique.x",	"Country_Myanmar.x",	"Country_Namibia.x",	"Country_Nauru.x",	"Country_Nepal.x",	"Country_Netherlands.x",
                                "Country_NewCaledonia.x",	"Country_NewZealand.x",	"Country_Nicaragua.x",	"Country_Niger.x",	"Country_Nigeria.x",	"Country_NorthernMarianaIslands.x",
                                "Country_Norway.x",	"Country_Oman.x",	"Country_Pakistan.x",	"Country_Palau.x",	"Country_Panama.x",	"Country_PapuaNewGuinea.x",	"Country_Paraguay.x",
                                "Country_Peru.x",	"Country_Philippines.x",	"Country_Poland.x",	"Country_Portugal.x",	"Country_PuertoRico.x",	"Country_Qatar.x",	"Country_Romania.x",
                                "Country_RussianFederation.x",	"Country_Rwanda.x",	"Country_Samoa.x",	"Country_SanMarino.x",	"Country_SaoTomeandPrincipe.x",	"Country_SaudiArabia.x",
                                "Country_Senegal.x",	"Country_Serbia.x",	"Country_Seychelles.x",	"Country_SierraLeone.x",	"Country_Singapore.x",	"Country_SintMaartenDutchpart.x",
                                "Country_SlovakRepublic.x",	"Country_Slovenia.x",	"Country_SolomonIslands.x",	"Country_Somalia.x",	"Country_SouthAfrica.x",	"Country_SouthSudan.x",
                                "Country_Spain.x",	"Country_SriLanka.x",	"Country_StKittsandNevis.x",	"Country_StLucia.x",	"Country_StMartinFrenchpart.x",	"Country_StVincentandtheGrenadine.x",
                                "Country_Sudan.x",	"Country_Suriname.x",	"Country_Swaziland.x",	"Country_Sweden.x",	"Country_Switzerland.x",	"Country_SyrianArabRepublic.x",
                                "Country_Tajikistan.x",	"Country_Tanzania.x",	"Country_Thailand.x",	"Country_TimorLeste.x",	"Country_Togo.x",	"Country_Tonga.x",	"Country_TrinidadandTobago.x",
                                "Country_Tunisia.x",	"Country_Turkey.x",	"Country_Turkmenistan.x",	"Country_TurksandCaicosIslands.x",	"Country_Tuvalu.x",	"Country_Uganda.x",
                                "Country_Ukraine.x",	"Country_UnitedArabEmirates.x",	"Country_UnitedKingdom.x",	"Country_UnitedStates.x",	"Country_Uruguay.x",	"Country_Uzbekistan.x",
                                "Country_Vanuatu.x",	"Country_VenezuelaRB.x",	"Country_Vietnam.x",	"Country_VirginIslandsUS.x",	"Country_WestBankandGaza.x",	"Country_YemenRep.x",
                                "Country_Zambia.x",	"Country_Zimbabwe.x",	"Country_TaiwanChina.x",	"Country_CookIslands.x",	"Country_Niue.x",	"Country_WallisandFutuna.x",	"Country_NA.x", "PreventionPrimary",
                                'InterventionCareDelivery', 'InterventionDiagnostic', 'InterventionEnvironmentalaugmen', 'InterventionEnvironmentalremedi', 'InterventionHealthEducationOrBe',
                                'InterventionImmunization', 'InterventionLegislation', 'InterventionMaternalNeonatalCa', 'InterventionMedicalDevice', 'InterventionMedicalProcedure', 
                                'InterventionPharmaceutical', 'InterventionNutrition', 'InterventionOther', 'InterventionScreening', 'InterventionSurgical'
)]

#--# INTERVENTION #--# 
#reclassify intervention types
#sourced out to external file
source("interventions_cleaning.R")

##CREATE VARIABLES COMPATIBLE WITH SELECTIZE INPUT FILTERS##
#-----# VARIABLES COMPATIBLE WITH SELECTIZE INPUT FILTERS #-----#
#String variables to be used for subsetting data for output table
#--#  Super region, sponsor, target population, country #--# 

#Create string variables to be used for subsetting data for output table
Strings<-list(SuperRegion = c("SuperRegion_s_SSA", "SuperRegion_s_SEA", "SuperRegion_s_SA", "SuperRegion_s_NAME","SuperRegion_s_HI", "SuperRegion_s_ECA","SuperRegion_s_LA"),
              Sponsor = c("Sponsor_s_academic", "Sponsor_s_government", "Sponsor_s_intergovernmental", "Sponsor_s_foundation","Sponsor_s_gates", "Sponsor_s_industry", 
                          "Sponsor_s_healthcare", "Sponsor_s_proforg", "Sponsor_s_none"),
              TargetPop = c("TP_s_age05", "TP_s_age611", "TP_s_age1218", "TP_s_age1940", "TP_s_age4164", "TP_s_age65", "TP_s_ageunspec", "TP_s_ageunknown"),
              Country = c("Country_Afghanistan_s",	"Country_Albania_s",	"Country_Algeria_s",	
                          "Country_AmericanSamoa_s",	"Country_Andorra_s",	"Country_Angola_s",	"Country_AntiguaandBarbuda_s",
                          "Country_Argentina_s",	"Country_Armenia_s",	"Country_Aruba_s",	"Country_Australia_s",
                          "Country_Austria_s",	"Country_Azerbaijan_s",	"Country_Bahamas_s",	"Country_Bahrain_s",
                          "Country_Bangladesh_s",	"Country_Barbados_s",	"Country_Belarus_s",	"Country_Belgium_s",
                          "Country_Belize_s",	"Country_Benin_s",	"Country_Bermuda_s",	"Country_Bhutan_s",
                          "Country_Bolivia_s",	"Country_BosniaandHerzegovina_s",	"Country_Botswana_s",	"Country_Brazil_s",
                          "Country_BritishVirginIslands_s",	"Country_Brunei_s",	"Country_Bulgaria_s",	
                          "Country_BurkinaFaso_s",	"Country_Burundi_s",	"Country_CaboVerde_s",	"Country_Cambodia_s",
                          "Country_Cameroon_s",	"Country_Canada_s",	"Country_CaymanIslands_s",	
                          "Country_CentralAfricanRepublic_s",	"Country_Chad_s",	"Country_ChannelIslands_s",	
                          "Country_Chile_s",	"Country_China_s",	"Country_Colombia_s",	"Country_Comoros_s",
                          "Country_CongoDemRep_s",	"Country_CongoRep_s",	"Country_CostaRica_s",	"Country_CotedIvoire_s",
                          "Country_Croatia_s",	"Country_Cuba_s",	"Country_Curaco_s",	"Country_Cyprus_s",
                          "Country_CzechRepublic_s",	"Country_Denmark_s",	"Country_Djibouti_s",	"Country_Dominica_s",
                          "Country_DominicanRepublic_s",	"Country_Ecuador_s",	"Country_EgyptArabRep_s",	"Country_ElSalvador_s",
                          "Country_EquatorialGuinea_s",	"Country_Eritrea_s",	"Country_Estonia_s",	"Country_Ethiopia_s",
                          "Country_FaroeIslands_s",	"Country_Fiji_s",	"Country_Finland_s",	"Country_France_s",
                          "Country_FrenchPolynesia_s",	"Country_Gabon_s",	"Country_GambiaThe_s",	"Country_Georgia_s",
                          "Country_Germany_s",	"Country_Ghana_s",	"Country_Gibraltar_s",	"Country_Greece_s",
                          "Country_Greenland_s",	"Country_Grenada_s",	"Country_Guam_s",	"Country_Guatemala_s",
                          "Country_Guinea_s",	"Country_GuineaBissau_s",	"Country_Guyana_s",	"Country_Haiti_s",
                          "Country_Honduras_s",	"Country_HongKongSARChina_s",	"Country_Hungary_s",	"Country_Iceland_s",
                          "Country_India_s",	"Country_Indonesia_s",	"Country_IranIslamicRep_s",	"Country_Iraq_s",
                          "Country_Ireland_s",	"Country_IsleofMan_s",	"Country_Israel_s",	"Country_Italy_s",
                          "Country_Jamaica_s",	"Country_Japan_s",	"Country_Jordan_s",	"Country_Kazakhstan_s",
                          "Country_Kenya_s",	"Country_Kiribati_s",	"Country_KoreaDemPeoplesRep_s",	"Country_KoreaRep_s",
                          "Country_Kosovo_s",	"Country_Kuwait_s",	"Country_KyrgyzRepublic_s",	"Country_LaoPDR_s",
                          "Country_Latvia_s",	"Country_Lebanon_s",	"Country_Lesotho_s",	"Country_Liberia_s",
                          "Country_Libya_s",	"Country_Liechtenstein_s",	"Country_Lithuania_s",	"Country_Luxembourg_s",
                          "Country_MacaoSARChina_s",	"Country_MacedoniaFYR_s",	"Country_Madagascar_s",	"Country_Malawi_s",
                          "Country_Malaysia_s",	"Country_Maldives_s",	"Country_Mali_s",	"Country_Malta_s",
                          "Country_MarshallIslands_s",	"Country_Mauritania_s",	"Country_Mauritius_s",	"Country_Mexico_s",
                          "Country_MicronesiaFedSts_s",	"Country_Moldova_s",	"Country_Monaco_s",	"Country_Mongolia_s",
                          "Country_Montenegro_s",	"Country_Morocco_s",	"Country_Mozambique_s",	"Country_Myanmar_s",
                          "Country_Namibia_s",	"Country_Nauru_s",	"Country_Nepal_s",	"Country_Netherlands_s",
                          "Country_NewCaledonia_s",	"Country_NewZealand_s",	"Country_Nicaragua_s",	"Country_Niger_s",
                          "Country_Nigeria_s",	"Country_NorthernMarianaIslands_s",	"Country_Norway_s",	"Country_Oman_s",
                          "Country_Pakistan_s",	"Country_Palau_s",	"Country_Panama_s",	"Country_PapuaNewGuinea_s",
                          "Country_Paraguay_s",	"Country_Peru_s",	"Country_Philippines_s",	"Country_Poland_s",
                          "Country_Portugal_s",	"Country_PuertoRico_s",	"Country_Qatar_s",	"Country_Romania_s",
                          "Country_RussianFederation_s",	"Country_Rwanda_s",	"Country_Samoa_s",	"Country_SanMarino_s",
                          "Country_SaoTomeandPrincipe_s",	"Country_SaudiArabia_s",	"Country_Senegal_s",	"Country_Serbia_s",
                          "Country_Seychelles_s",	"Country_SierraLeone_s",	"Country_Singapore_s",	"Country_SintMaartenDutchpart_s",
                          "Country_SlovakRepublic_s",	"Country_Slovenia_s",	"Country_SolomonIslands_s",	"Country_Somalia_s",
                          "Country_SouthAfrica_s",	"Country_SouthSudan_s",	"Country_Spain_s",	"Country_SriLanka_s",
                          "Country_StKittsandNevis_s",	"Country_StLucia_s",	"Country_StMartinFrenchpart_s",	
                          "Country_StVincentandtheGrenadine_s",	"Country_Sudan_s",	"Country_Suriname_s",	"Country_Swaziland_s",
                          "Country_Sweden_s",	"Country_Switzerland_s",	"Country_SyrianArabRepublic_s",	"Country_Tajikistan_s",
                          "Country_Tanzania_s",	"Country_Thailand_s",	"Country_TimorLeste_s",	"Country_Togo_s",
                          "Country_Tonga_s",	"Country_TrinidadandTobago_s",	"Country_Tunisia_s",	"Country_Turkey_s",
                          "Country_Turkmenistan_s",	"Country_TurksandCaicosIslands_s",	"Country_Tuvalu_s",	"Country_Uganda_s",
                          "Country_Ukraine_s",	"Country_UnitedArabEmirates_s",	"Country_UnitedKingdom_s",	
                          "Country_UnitedStates_s",	"Country_Uruguay_s",	"Country_Uzbekistan_s",	"Country_Vanuatu_s",
                          "Country_VenezuelaRB_s",	"Country_Vietnam_s",	"Country_VirginIslandsUS_s",	"Country_WestBankandGaza_s",
                          "Country_YemenRep_s",	"Country_Zambia_s",	"Country_Zimbabwe_s",	"Country_TaiwanChina_s",
                          "Country_CookIslands_s",	"Country_Niue_s",	"Country_WallisandFutuna_s",	"Country_NA_s"),
              InterventionType = c('Intervention_s_CD', 'Intervention_s_D','Intervention_s_EA','Intervention_s_ER','Intervention_s_HE','Intervention_s_I',
                                   'Intervention_s_L','Intervention_s_ME','Intervention_s_MD','Intervention_s_MP','Intervention_s_P','Intervention_s_N',
                                   'Intervention_s_O','Intervention_s_s','Intervention_s_sur')
)
Binaries<-list(SuperRegion = c("SuperRegion_SubSaharanAfrica.x", "SuperRegion_SoutheastAsiaOceania.x", "SuperRegion_SouthAsia.x","SuperRegion_NorthAfricaMidEast.x",
                               "SuperRegion_HighIncome.x", "SuperRegion_EuropeCentralAsia.x", "SuperRegion_LatinAmerica.x"),
               Sponsor = c("SponsorshipAcademic", "SponsorshipGovernment", "SponsorshipIntergovernmentalOr", 
                           "SponsorshipFoundation", "SponsorshipGatesFoundation", "SponsorshipPharmOrDevice", "SponsorshipHealthCare", "SponsorshipProfMembOrg", 
                           "SponsorshipNone"),
               TargetPop = c("TargetPopulationAge_05", "TargetPopulationAge_611", "TargetPopulationAge_1218", "TargetPopulationAge_1940", "TargetPopulationAge_4164", 
                             "TargetPopulationAge_65", "TargetPopulationAge_adult", "TargetPopulationAge_unknown"),
               Country = c("Country_Afghanistan.x",	"Country_Albania.x",	"Country_Algeria.x",	
                           "Country_AmericanSamoa.x",	"Country_Andorra.x",	"Country_Angola.x",	"Country_AntiguaandBarbuda.x",
                           "Country_Argentina.x",	"Country_Armenia.x",	"Country_Aruba.x",	"Country_Australia.x",
                           "Country_Austria.x",	"Country_Azerbaijan.x",	"Country_Bahamas.x",	"Country_Bahrain.x",
                           "Country_Bangladesh.x",	"Country_Barbados.x",	"Country_Belarus.x",	"Country_Belgium.x",
                           "Country_Belize.x",	"Country_Benin.x",	"Country_Bermuda.x",	"Country_Bhutan.x",
                           "Country_Bolivia.x",	"Country_BosniaandHerzegovina.x",	"Country_Botswana.x",	"Country_Brazil.x",
                           "Country_BritishVirginIslands.x",	"Country_Brunei.x",	"Country_Bulgaria.x",	
                           "Country_BurkinaFaso.x",	"Country_Burundi.x",	"Country_CaboVerde.x",	"Country_Cambodia.x",
                           "Country_Cameroon.x",	"Country_Canada.x",	"Country_CaymanIslands.x",	
                           "Country_CentralAfricanRepublic.x",	"Country_Chad.x",	"Country_ChannelIslands.x",	
                           "Country_Chile.x",	"Country_China.x",	"Country_Colombia.x",	"Country_Comoros.x",
                           "Country_CongoDemRep.x",	"Country_CongoRep.x",	"Country_CostaRica.x",	"Country_CotedIvoire.x",
                           "Country_Croatia.x",	"Country_Cuba.x",	"Country_Curaco.x",	"Country_Cyprus.x",
                           "Country_CzechRepublic.x",	"Country_Denmark.x",	"Country_Djibouti.x",	"Country_Dominica.x",
                           "Country_DominicanRepublic.x",	"Country_Ecuador.x",	"Country_EgyptArabRep.x",	"Country_ElSalvador.x",
                           "Country_EquatorialGuinea.x",	"Country_Eritrea.x",	"Country_Estonia.x",	"Country_Ethiopia.x",
                           "Country_FaroeIslands.x",	"Country_Fiji.x",	"Country_Finland.x",	"Country_France.x",
                           "Country_FrenchPolynesia.x",	"Country_Gabon.x",	"Country_GambiaThe.x",	"Country_Georgia.x",
                           "Country_Germany.x",	"Country_Ghana.x",	"Country_Gibraltar.x",	"Country_Greece.x",
                           "Country_Greenland.x",	"Country_Grenada.x",	"Country_Guam.x",	"Country_Guatemala.x",
                           "Country_Guinea.x",	"Country_GuineaBissau.x",	"Country_Guyana.x",	"Country_Haiti.x",
                           "Country_Honduras.x",	"Country_HongKongSARChina.x",	"Country_Hungary.x",	"Country_Iceland.x",
                           "Country_India.x",	"Country_Indonesia.x",	"Country_IranIslamicRep.x",	"Country_Iraq.x",
                           "Country_Ireland.x",	"Country_IsleofMan.x",	"Country_Israel.x",	"Country_Italy.x",
                           "Country_Jamaica.x",	"Country_Japan.x",	"Country_Jordan.x",	"Country_Kazakhstan.x",
                           "Country_Kenya.x",	"Country_Kiribati.x",	"Country_KoreaDemPeoplesRep.x",	"Country_KoreaRep.x",
                           "Country_Kosovo.x",	"Country_Kuwait.x",	"Country_KyrgyzRepublic.x",	"Country_LaoPDR.x",
                           "Country_Latvia.x",	"Country_Lebanon.x",	"Country_Lesotho.x",	"Country_Liberia.x",
                           "Country_Libya.x",	"Country_Liechtenstein.x",	"Country_Lithuania.x",	"Country_Luxembourg.x",
                           "Country_MacaoSARChina.x",	"Country_MacedoniaFYR.x",	"Country_Madagascar.x",	"Country_Malawi.x",
                           "Country_Malaysia.x",	"Country_Maldives.x",	"Country_Mali.x",	"Country_Malta.x",
                           "Country_MarshallIslands.x",	"Country_Mauritania.x",	"Country_Mauritius.x",	"Country_Mexico.x",
                           "Country_MicronesiaFedSts.x",	"Country_Moldova.x",	"Country_Monaco.x",	"Country_Mongolia.x",
                           "Country_Montenegro.x",	"Country_Morocco.x",	"Country_Mozambique.x",	"Country_Myanmar.x",
                           "Country_Namibia.x",	"Country_Nauru.x",	"Country_Nepal.x",	"Country_Netherlands.x",
                           "Country_NewCaledonia.x",	"Country_NewZealand.x",	"Country_Nicaragua.x",	"Country_Niger.x",
                           "Country_Nigeria.x",	"Country_NorthernMarianaIslands.x",	"Country_Norway.x",	"Country_Oman.x",
                           "Country_Pakistan.x",	"Country_Palau.x",	"Country_Panama.x",	"Country_PapuaNewGuinea.x",
                           "Country_Paraguay.x",	"Country_Peru.x",	"Country_Philippines.x",	"Country_Poland.x",
                           "Country_Portugal.x",	"Country_PuertoRico.x",	"Country_Qatar.x",	"Country_Romania.x",
                           "Country_RussianFederation.x",	"Country_Rwanda.x",	"Country_Samoa.x",	"Country_SanMarino.x",
                           "Country_SaoTomeandPrincipe.x",	"Country_SaudiArabia.x",	"Country_Senegal.x",	"Country_Serbia.x",
                           "Country_Seychelles.x",	"Country_SierraLeone.x",	"Country_Singapore.x",	"Country_SintMaartenDutchpart.x",
                           "Country_SlovakRepublic.x",	"Country_Slovenia.x",	"Country_SolomonIslands.x",	"Country_Somalia.x",
                           "Country_SouthAfrica.x",	"Country_SouthSudan.x",	"Country_Spain.x",	"Country_SriLanka.x",
                           "Country_StKittsandNevis.x",	"Country_StLucia.x",	"Country_StMartinFrenchpart.x",	
                           "Country_StVincentandtheGrenadine.x",	"Country_Sudan.x",	"Country_Suriname.x",	"Country_Swaziland.x",
                           "Country_Sweden.x",	"Country_Switzerland.x",	"Country_SyrianArabRepublic.x",	"Country_Tajikistan.x",
                           "Country_Tanzania.x",	"Country_Thailand.x",	"Country_TimorLeste.x",	"Country_Togo.x",
                           "Country_Tonga.x",	"Country_TrinidadandTobago.x",	"Country_Tunisia.x",	"Country_Turkey.x",
                           "Country_Turkmenistan.x",	"Country_TurksandCaicosIslands.x",	"Country_Tuvalu.x",	"Country_Uganda.x",
                           "Country_Ukraine.x",	"Country_UnitedArabEmirates.x",	"Country_UnitedKingdom.x",	
                           "Country_UnitedStates.x",	"Country_Uruguay.x",	"Country_Uzbekistan.x",	"Country_Vanuatu.x",
                           "Country_VenezuelaRB.x",	"Country_Vietnam.x",	"Country_VirginIslandsUS.x",	"Country_WestBankandGaza.x",
                           "Country_YemenRep.x",	"Country_Zambia.x",	"Country_Zimbabwe.x",	"Country_TaiwanChina.x",
                           "Country_CookIslands.x",	"Country_Niue.x",	"Country_WallisandFutuna.x",	"Country_NA.x"),
               IntervetionType =   c('InterventionCareDelivery', 'InterventionDiagnostic', 'InterventionEnvironmentalaugmen', 'InterventionEnvironmentalremedi', 'InterventionHealthEducationOrBe',
                                     'InterventionImmunization', 'InterventionLegislation', 'InterventionMaternalNeonatalCa', 'InterventionMedicalDevice', 'InterventionMedicalProcedure', 
                                     'InterventionPharmaceutical', 'InterventionNutrition', 'InterventionOther', 'InterventionScreening', 'InterventionSurgical')
)
Names<-list(SuperRegion = c("Sub-Saharan Africa", "Southeast Asia, Oceania", "South Asia", "North Africa, Middle East", "High Income","Europe, Central Asia", "Latin America"),
            Sponsor = c("Academic", "Government", "Intergovernmental Org", "Foundation", "Gates Foundation", "Industry", "Healthcare Org", "Prof Membership Org", "None"),
            TargetPop = c("Children: 0-5 years", "Children: 6-11 years", "Adolescents: 12-18 years", "Adults: 19-40 years", "Adults: 41-64 years", "Older adults: >65 years",
                          "Adult: age unspecified", "Unknown"),
            Country = c("Afghanistan",	"Albania",	"Algeria",	"American Samoa",	"Andorra",	 "Angola",	"Antigua and Barbuda",
                        "Argentina",	"Armenia",	 "Aruba",	 "Australia",	"Austria",	"Azerbaijan",	"Bahamas",	"Bahrain",	
                        "Bangladesh",	"Barbados",	"Belarus",	"Belgium",	 "Belize",	 "Benin",	"Bermuda",	 "Bhutan",	"Bolivia",
                        "Bosnia and Herzegovina",	"Botswana",	 "Brazil",	"British Virgin Islands", "Brunei",	"Bulgaria",	 
                        "Burkina Faso",	"Burundi",	"Cabo Verde",	"Cambodia",	"Cameroon",	 "Canada",	 "Cayman Islands",	
                        "Central African Republic",	"Chad",	 "Channel Islands", "Chile",	 "China",	"Colombia",	"Comoros",	
                        "Democratic Republic of Congo",	 "Congo",	"Costa Rica",	"Cote dIvoire",	"Croatia",	"Cuba",	"Curacao", 
                        "Cyprus",	"Czech Republic",	"Denmark",	"Djibouti",	"Dominica",	 "Dominican Republic",	"Ecuador",	 "Egypt",
                        "El Salvador",	 "Equatorial Guinea",	"Eritrea",	"Estonia",	"Ethiopia", "Faroe Islands",	"Fiji",	"Finland",
                        "France",	 "French Polynesia",	 "Gabon",	"The Gambia",	"Georgia",	"Germany",	 "Ghana",	"Gibraltar",
                        "Greece",	 "Greenland",	"Grenada","Guam",		"Guatemala",	 "Guinea",	"Guinea Bissau",	 "Guyana",	 "Haiti",
                        "Honduras",	"Hong Kong",	"Hungary",	"Iceland",	 "India",	"Indonesia",	"Iran",	"Iraq",	"Ireland","Isle of Man",	
                        "Israel",	 "Italy",	"Jamaica",	 "Japan",	 "Jordan",	"Kazakhstan",	 "Kenya",	"Kiribati",
                        "North Korea",	 "South Korea",	"Kosovo","Kuwait",	 "Kyrgyz Republic",	"Laos",	 "Latvia",	"Lebanon",
                        "Lesotho",	"Liberia",	 "Libya","Liechtenstein",	"Lithuania",	"Luxembourg",	"Macao", "Macedonia",	
                        "Madagascar",	 "Malawi",	"Malaysia",	"Maldives",	"Mali",	 "Malta",	 "Marshall Islands",	"Mauritania",	
                        "Mauritius",	 "Mexico",	"Micronesia",	"Moldova",	 "Monaco",	"Mongolia",	"Montenegro",	"Morocco",	
                        "Mozambique",	"Myanmar",	"Namibia",	 "Nauru",	 "Nepal",	"Netherlands", "New Caledonia", "New Zealand",
                        "Nicaragua",	 "Niger",	"Nigeria",	"Northern Mariana Islands",	 "Norway",	"Oman",	"Pakistan",	 "Palau",	
                        "Panama",	 "Papua New Guinea",	"Paraguay",	"Peru",	"Philippines",	 "Poland",	"Portugal",	"Puerto Rico",
                        "Qatar",	"Romania",	"Russian Federation",	 "Rwanda",	 "Samoa",	"San Marino",	 "Sao Tome and Principe",	 
                        "Saudi Arabia",	"Senegal",	 "Serbia",	 "Seychelles",	 "Sierra Leone",	 "Singapore",	 "Sint Maarten",
                        "Slovak Republic",	"Slovenia",	"Solomon Islands",	"Somalia",	"South Africa",
                        "South Sudan",	"Spain",	"Sri Lanka",	"St Kitts and Nevis",	"St Lucia",	"St Martin",	
                        "St Vincent and the Grenadines",	"Sudan",	"Suriname",	"Swaziland",	 "Sweden",	"Switzerland",	"Syria",
                        "Tajikistan",	"Tanzania",	"Thailand",	 "Timor Leste",	 "Togo",	"Tonga",	"Trinidad and Tobago",	"Tunisia",
                        "Turkey",	"Turkmenistan",	 "Turks and Caicos",	 "Tuvalu",	 "Uganda",	"Ukraine",	"United Arab Emirates",
                        "United Kingdom",	"United States",	"Uruguay",	 "Uzbekistan",	"Vanuatu",	"Venezuela",	"Vietnam",
                        "Virgin Islands US","West Bank and Gaza",	"Yemen",	 "Zambia",	"Zimbabwe",	 "Taiwan",	"Cook Islands",	
                        "Niue",	 "Wallis and Futuna",	"NA"),
            InterventionType = c("Care Delivery","Diagnostic","Environmental Augmentation","Environmental Remediation","Health Education or Behavior","Immunization","Legislation",
                                 "Maternal or Neonatal Care","Medical Device","Medical Procedure","Pharmaceutical","Nutrition","Other","Screening","Surgical")
)
Multiples<-list(SuperRegion = c("multipleregions"),
                Sponsor = c("multiplesponsorship"),
                TargetPop = c("TP_agemultiple"),
                Country = c("country_multiple"),
                InterventionType = c("inttype_multiple"))
Displays<-list(SuperRegion = c("SuperRegion_display"),
               Sponsor = c("Sponsor_display"),
               TargetPop = c("TPage_display"),
               Country = c("Country_display"),
               InterventionType = c("IntType_display"))

#Create all new string, multiples, and display variables in league tables data frame
#strings
for(list in Strings){
  for(string in list){
    leaguetable_data[,string] <- NA
  }
}
#multiples
for(list in Multiples){
  for(var in list){
    leaguetable_data[,var] <- 0
  }
}
#displays
for(list in Displays){
  for(var in list){
    leaguetable_data[,var] <- NA
  }
}

#populate new string variables with names
for(list in 1:length(Strings)){
  for(num in 1:length(Strings[[list]])){
    string_colnum<-which(colnames(leaguetable_data) == Strings[[list]][num])
    binary_colnum<-which(colnames(leaguetable_data) == Binaries[[list]][num])
    leaguetable_data[,string_colnum]<-ifelse(leaguetable_data[,binary_colnum] == 1, Names[[list]][num], NA)
  }
}

#create variables to indicate multiples and display variables for region, sponsor- not target pop
for(list in 1:(length(Binaries))){
  
  for(num in 1:length(Binaries[[list]])){
    
    #string_colnum gets the column index of the matching string variable
    string_colnum<-which(colnames(leaguetable_data) == Strings[[list]][num])
    #binary_colnum gets the column index of the matching binary variable
    binary_colnum<-which(colnames(leaguetable_data) == Binaries[[list]][num])
    #multiple_colnum gets the column index of the matching multiples variable
    multiple_colnum<-which(colnames(leaguetable_data) == Multiples[[list]])
    #display_colnum gets the column index of the matching multiples variable
    display_colnum<-which(colnames(leaguetable_data) == Displays[[list]])
    
    #the appropriate multiple variable gets prior value (NA if none) if the binary is 0, or adds one if binary is 1 
    leaguetable_data[,multiple_colnum]<-ifelse(leaguetable_data[,binary_colnum] == 0,
                                               leaguetable_data[,multiple_colnum],
                                               leaguetable_data[,multiple_colnum]+1)
    
    #the appropriate display variable gets prior value (NA if none) if the string is populated, or adds the string if string is 1 
    leaguetable_data[,display_colnum]<-ifelse(is.na(leaguetable_data[,string_colnum]), 
                                              leaguetable_data[,display_colnum],
                                              paste(leaguetable_data[,display_colnum],leaguetable_data[,string_colnum], sep = ", "))
    
  }
  
  #replace with "not reported" if NA after loop
  leaguetable_data[,display_colnum]<-gsub("NA, ","",leaguetable_data[,display_colnum])
  leaguetable_data[,display_colnum]<-ifelse(is.na(leaguetable_data[,display_colnum]), "Not reported", leaguetable_data[,display_colnum])
}


#--# GBD DISEASE #--# 
#create string, missing and display and display (most specific tier available per obs)
#special case because of tiers

Disease_strings<-c("Disease_s_1", "Disease_s_2", "Disease_s_3", "Disease_s_4", "Disease_s_5")
for(string in Disease_strings){
  leaguetable_data[,string] <- NA
}

GBD_prefix<-"GBDDis"
GBD_suffix<-"_tier"

ctr<-1
ctr2<-4
while(ctr<=5){
  while(ctr2>=1){
    
    var<-paste(GBD_prefix, ctr, GBD_suffix, ctr2, sep = "")
    
    GBD_colnum<-which(colnames(leaguetable_data) == var)
    string_colnum<-which(colnames(leaguetable_data) == Disease_strings[ctr])
    
    leaguetable_data[,string_colnum]<-ifelse(is.na(leaguetable_data[,string_colnum]),
                                             ifelse(is.na(leaguetable_data[,GBD_colnum]),
                                                    leaguetable_data[,string_colnum],
                                                    leaguetable_data[,GBD_colnum]),
                                             leaguetable_data[,string_colnum])
    ctr2<-ctr2-1
  }
  ctr<-ctr+1
  ctr2<-4
}
#create display disease variable
leaguetable_data$Disease_display<-paste(leaguetable_data$Disease_s_1, leaguetable_data$Disease_s_2, leaguetable_data$Disease_s_3, leaguetable_data$Disease_s_4,
                                        leaguetable_data$Disease_s_5, sep = ", ")
leaguetable_data$Disease_display<-gsub("NA, ","",leaguetable_data$Disease_display)
leaguetable_data$Disease_display<-gsub(", NA","",leaguetable_data$Disease_display)


#Country from binary to string (for methods data)
Strings_methods<-list(Country = c("Country_Afghanistan_s",	"Country_Albania_s",	"Country_Algeria_s",	
                          "Country_AmericanSamoa_s",	"Country_Andorra_s",	"Country_Angola_s",	"Country_AntiguaandBarbuda_s",
                          "Country_Argentina_s",	"Country_Armenia_s",	"Country_Aruba_s",	"Country_Australia_s",
                          "Country_Austria_s",	"Country_Azerbaijan_s",	"Country_Bahamas_s",	"Country_Bahrain_s",
                          "Country_Bangladesh_s",	"Country_Barbados_s",	"Country_Belarus_s",	"Country_Belgium_s",
                          "Country_Belize_s",	"Country_Benin_s",	"Country_Bermuda_s",	"Country_Bhutan_s",
                          "Country_Bolivia_s",	"Country_BosniaandHerzegovina_s",	"Country_Botswana_s",	"Country_Brazil_s",
                          "Country_BritishVirginIslands_s",	"Country_Brunei_s",	"Country_Bulgaria_s",	
                          "Country_BurkinaFaso_s",	"Country_Burundi_s",	"Country_CaboVerde_s",	"Country_Cambodia_s",
                          "Country_Cameroon_s",	"Country_Canada_s",	"Country_CaymanIslands_s",	
                          "Country_CentralAfricanRepublic_s",	"Country_Chad_s",	"Country_ChannelIslands_s",	
                          "Country_Chile_s",	"Country_China_s",	"Country_Colombia_s",	"Country_Comoros_s",
                          "Country_CongoDemRep_s",	"Country_CongoRep_s",	"Country_CostaRica_s",	"Country_CotedIvoire_s",
                          "Country_Croatia_s",	"Country_Cuba_s",	"Country_Curaco_s",	"Country_Cyprus_s",
                          "Country_CzechRepublic_s",	"Country_Denmark_s",	"Country_Djibouti_s",	"Country_Dominica_s",
                          "Country_DominicanRepublic_s",	"Country_Ecuador_s",	"Country_EgyptArabRep_s",	"Country_ElSalvador_s",
                          "Country_EquatorialGuinea_s",	"Country_Eritrea_s",	"Country_Estonia_s",	"Country_Ethiopia_s",
                          "Country_FaroeIslands_s",	"Country_Fiji_s",	"Country_Finland_s",	"Country_France_s",
                          "Country_FrenchPolynesia_s",	"Country_Gabon_s",	"Country_GambiaThe_s",	"Country_Georgia_s",
                          "Country_Germany_s",	"Country_Ghana_s",	"Country_Gibraltar_s",	"Country_Greece_s",
                          "Country_Greenland_s",	"Country_Grenada_s",	"Country_Guam_s",	"Country_Guatemala_s",
                          "Country_Guinea_s",	"Country_GuineaBissau_s",	"Country_Guyana_s",	"Country_Haiti_s",
                          "Country_Honduras_s",	"Country_HongKongSARChina_s",	"Country_Hungary_s",	"Country_Iceland_s",
                          "Country_India_s",	"Country_Indonesia_s",	"Country_IranIslamicRep_s",	"Country_Iraq_s",
                          "Country_Ireland_s",	"Country_IsleofMan_s",	"Country_Israel_s",	"Country_Italy_s",
                          "Country_Jamaica_s",	"Country_Japan_s",	"Country_Jordan_s",	"Country_Kazakhstan_s",
                          "Country_Kenya_s",	"Country_Kiribati_s",	"Country_KoreaDemPeoplesRep_s",	"Country_KoreaRep_s",
                          "Country_Kosovo_s",	"Country_Kuwait_s",	"Country_KyrgyzRepublic_s",	"Country_LaoPDR_s",
                          "Country_Latvia_s",	"Country_Lebanon_s",	"Country_Lesotho_s",	"Country_Liberia_s",
                          "Country_Libya_s",	"Country_Liechtenstein_s",	"Country_Lithuania_s",	"Country_Luxembourg_s",
                          "Country_MacaoSARChina_s",	"Country_MacedoniaFYR_s",	"Country_Madagascar_s",	"Country_Malawi_s",
                          "Country_Malaysia_s",	"Country_Maldives_s",	"Country_Mali_s",	"Country_Malta_s",
                          "Country_MarshallIslands_s",	"Country_Mauritania_s",	"Country_Mauritius_s",	"Country_Mexico_s",
                          "Country_MicronesiaFedSts_s",	"Country_Moldova_s",	"Country_Monaco_s",	"Country_Mongolia_s",
                          "Country_Montenegro_s",	"Country_Morocco_s",	"Country_Mozambique_s",	"Country_Myanmar_s",
                          "Country_Namibia_s",	"Country_Nauru_s",	"Country_Nepal_s",	"Country_Netherlands_s",
                          "Country_NewCaledonia_s",	"Country_NewZealand_s",	"Country_Nicaragua_s",	"Country_Niger_s",
                          "Country_Nigeria_s",	"Country_NorthernMarianaIslands_s",	"Country_Norway_s",	"Country_Oman_s",
                          "Country_Pakistan_s",	"Country_Palau_s",	"Country_Panama_s",	"Country_PapuaNewGuinea_s",
                          "Country_Paraguay_s",	"Country_Peru_s",	"Country_Philippines_s",	"Country_Poland_s",
                          "Country_Portugal_s",	"Country_PuertoRico_s",	"Country_Qatar_s",	"Country_Romania_s",
                          "Country_RussianFederation_s",	"Country_Rwanda_s",	"Country_Samoa_s",	"Country_SanMarino_s",
                          "Country_SaoTomeandPrincipe_s",	"Country_SaudiArabia_s",	"Country_Senegal_s",	"Country_Serbia_s",
                          "Country_Seychelles_s",	"Country_SierraLeone_s",	"Country_Singapore_s",	"Country_SintMaartenDutchpart_s",
                          "Country_SlovakRepublic_s",	"Country_Slovenia_s",	"Country_SolomonIslands_s",	"Country_Somalia_s",
                          "Country_SouthAfrica_s",	"Country_SouthSudan_s",	"Country_Spain_s",	"Country_SriLanka_s",
                          "Country_StKittsandNevis_s",	"Country_StLucia_s",	"Country_StMartinFrenchpart_s",	
                          "Country_StVincentandtheGrenadine_s",	"Country_Sudan_s",	"Country_Suriname_s",	"Country_Swaziland_s",
                          "Country_Sweden_s",	"Country_Switzerland_s",	"Country_SyrianArabRepublic_s",	"Country_Tajikistan_s",
                          "Country_Tanzania_s",	"Country_Thailand_s",	"Country_TimorLeste_s",	"Country_Togo_s",
                          "Country_Tonga_s",	"Country_TrinidadandTobago_s",	"Country_Tunisia_s",	"Country_Turkey_s",
                          "Country_Turkmenistan_s",	"Country_TurksandCaicosIslands_s",	"Country_Tuvalu_s",	"Country_Uganda_s",
                          "Country_Ukraine_s",	"Country_UnitedArabEmirates_s",	"Country_UnitedKingdom_s",	
                          "Country_UnitedStates_s",	"Country_Uruguay_s",	"Country_Uzbekistan_s",	"Country_Vanuatu_s",
                          "Country_VenezuelaRB_s",	"Country_Vietnam_s",	"Country_VirginIslandsUS_s",	"Country_WestBankandGaza_s",
                          "Country_YemenRep_s",	"Country_Zambia_s",	"Country_Zimbabwe_s",	"Country_TaiwanChina_s",
                          "Country_CookIslands_s",	"Country_Niue_s",	"Country_WallisandFutuna_s",	"Country_NA_s"),
              InterventionType = c('Intervention_s_CD','Intervention_s_D','Intervention_s_EA','Intervention_s_ER','Intervention_s_HE','Intervention_s_I','Intervention_s_L',
                                   'Intervention_s_ME','Intervention_s_MD','Intervention_s_MP','Intervention_s_P','Intervention_s_N','Intervention_s_O','Intervention_s_s','Intervention_s_sur')
              )

Binaries_methods<-list(Country = c("Country_Afghanistan",	"Country_Albania",	"Country_Algeria",	
                           "Country_AmericanSamoa",	"Country_Andorra",	"Country_Angola",	"Country_AntiguaandBarbuda",
                           "Country_Argentina",	"Country_Armenia",	"Country_Aruba",	"Country_Australia",
                           "Country_Austria",	"Country_Azerbaijan",	"Country_Bahamas",	"Country_Bahrain",
                           "Country_Bangladesh",	"Country_Barbados",	"Country_Belarus",	"Country_Belgium",
                           "Country_Belize",	"Country_Benin",	"Country_Bermuda",	"Country_Bhutan",
                           "Country_Bolivia",	"Country_BosniaandHerzegovina",	"Country_Botswana",	"Country_Brazil",
                           "Country_BritishVirginIslands",	"Country_Brunei",	"Country_Bulgaria",	
                           "Country_BurkinaFaso",	"Country_Burundi",	"Country_CaboVerde",	"Country_Cambodia",
                           "Country_Cameroon",	"Country_Canada",	"Country_CaymanIslands",	
                           "Country_CentralAfricanRepublic",	"Country_Chad",	"Country_ChannelIslands",	
                           "Country_Chile",	"Country_China",	"Country_Colombia",	"Country_Comoros",
                           "Country_CongoDemRep",	"Country_CongoRep",	"Country_CostaRica",	"Country_CotedIvoire",
                           "Country_Croatia",	"Country_Cuba",	"Country_Curaco",	"Country_Cyprus",
                           "Country_CzechRepublic",	"Country_Denmark",	"Country_Djibouti",	"Country_Dominica",
                           "Country_DominicanRepublic",	"Country_Ecuador",	"Country_EgyptArabRep",	"Country_ElSalvador",
                           "Country_EquatorialGuinea",	"Country_Eritrea",	"Country_Estonia",	"Country_Ethiopia",
                           "Country_FaroeIslands",	"Country_Fiji",	"Country_Finland",	"Country_France",
                           "Country_FrenchPolynesia",	"Country_Gabon",	"Country_GambiaThe",	"Country_Georgia",
                           "Country_Germany",	"Country_Ghana",	"Country_Gibraltar",	"Country_Greece",
                           "Country_Greenland",	"Country_Grenada",	"Country_Guam",	"Country_Guatemala",
                           "Country_Guinea",	"Country_GuineaBissau",	"Country_Guyana",	"Country_Haiti",
                           "Country_Honduras",	"Country_HongKongSARChina",	"Country_Hungary",	"Country_Iceland",
                           "Country_India",	"Country_Indonesia",	"Country_IranIslamicRep",	"Country_Iraq",
                           "Country_Ireland",	"Country_IsleofMan",	"Country_Israel",	"Country_Italy",
                           "Country_Jamaica",	"Country_Japan",	"Country_Jordan",	"Country_Kazakhstan",
                           "Country_Kenya",	"Country_Kiribati",	"Country_KoreaDemPeoplesRep",	"Country_KoreaRep",
                           "Country_Kosovo",	"Country_Kuwait",	"Country_KyrgyzRepublic",	"Country_LaoPDR",
                           "Country_Latvia",	"Country_Lebanon",	"Country_Lesotho",	"Country_Liberia",
                           "Country_Libya",	"Country_Liechtenstein",	"Country_Lithuania",	"Country_Luxembourg",
                           "Country_MacaoSARChina",	"Country_MacedoniaFYR",	"Country_Madagascar",	"Country_Malawi",
                           "Country_Malaysia",	"Country_Maldives",	"Country_Mali",	"Country_Malta",
                           "Country_MarshallIslands",	"Country_Mauritania",	"Country_Mauritius",	"Country_Mexico",
                           "Country_MicronesiaFedSts",	"Country_Moldova",	"Country_Monaco",	"Country_Mongolia",
                           "Country_Montenegro",	"Country_Morocco",	"Country_Mozambique",	"Country_Myanmar",
                           "Country_Namibia",	"Country_Nauru",	"Country_Nepal",	"Country_Netherlands",
                           "Country_NewCaledonia",	"Country_NewZealand",	"Country_Nicaragua",	"Country_Niger",
                           "Country_Nigeria",	"Country_NorthernMarianaIslands",	"Country_Norway",	"Country_Oman",
                           "Country_Pakistan",	"Country_Palau",	"Country_Panama",	"Country_PapuaNewGuinea",
                           "Country_Paraguay",	"Country_Peru",	"Country_Philippines",	"Country_Poland",
                           "Country_Portugal",	"Country_PuertoRico",	"Country_Qatar",	"Country_Romania",
                           "Country_RussianFederation",	"Country_Rwanda",	"Country_Samoa",	"Country_SanMarino",
                           "Country_SaoTomeandPrincipe",	"Country_SaudiArabia",	"Country_Senegal",	"Country_Serbia",
                           "Country_Seychelles",	"Country_SierraLeone",	"Country_Singapore",	"Country_SintMaartenDutchpart",
                           "Country_SlovakRepublic",	"Country_Slovenia",	"Country_SolomonIslands",	"Country_Somalia",
                           "Country_SouthAfrica",	"Country_SouthSudan",	"Country_Spain",	"Country_SriLanka",
                           "Country_StKittsandNevis",	"Country_StLucia",	"Country_StMartinFrenchpart",	
                           "Country_StVincentandtheGrenadine",	"Country_Sudan",	"Country_Suriname",	"Country_Swaziland",
                           "Country_Sweden",	"Country_Switzerland",	"Country_SyrianArabRepublic",	"Country_Tajikistan",
                           "Country_Tanzania",	"Country_Thailand",	"Country_TimorLeste",	"Country_Togo",
                           "Country_Tonga",	"Country_TrinidadandTobago",	"Country_Tunisia",	"Country_Turkey",
                           "Country_Turkmenistan",	"Country_TurksandCaicosIslands",	"Country_Tuvalu",	"Country_Uganda",
                           "Country_Ukraine",	"Country_UnitedArabEmirates",	"Country_UnitedKingdom",	
                           "Country_UnitedStates",	"Country_Uruguay",	"Country_Uzbekistan",	"Country_Vanuatu",
                           "Country_VenezuelaRB",	"Country_Vietnam",	"Country_VirginIslandsUS",	"Country_WestBankandGaza",
                           "Country_YemenRep",	"Country_Zambia",	"Country_Zimbabwe",	"Country_TaiwanChina",
                           "Country_CookIslands",	"Country_Niue",	"Country_WallisandFutuna",	"Country_NA"),
               InterventionType = c('InterventionCareDelivery', 'InterventionDiagnostic', 'InterventionEnvironmentalaugmen', 'InterventionEnvironmentalremedi', 'InterventionHealthEducationOrBe',
                                    'InterventionImmunization', 'InterventionLegislation', 'InterventionMaternalNeonatalCa', 'InterventionMedicalDevice', 'InterventionMedicalProcedure', 
                                    'InterventionPharmaceutical', 'InterventionNutrition', 'InterventionOther', 'InterventionScreening', 'InterventionSurgical')
)
               
Names_methods<-list(Country = c("Afghanistan",	"Albania",	"Algeria",	"American Samoa",	"Andorra",	 "Angola",	"Antigua and Barbuda",
                                       "Argentina",	"Armenia",	 "Aruba",	 "Australia",	"Austria",	"Azerbaijan",	"Bahamas",	"Bahrain",	
                                       "Bangladesh",	"Barbados",	"Belarus",	"Belgium",	 "Belize",	 "Benin",	"Bermuda",	 "Bhutan",	"Bolivia",
                                       "Bosnia and Herzegovina",	"Botswana",	 "Brazil",	"British Virgin Islands", "Brunei",	"Bulgaria",	 
                                       "Burkina Faso",	"Burundi",	"Cabo Verde",	"Cambodia",	"Cameroon",	 "Canada",	 "Cayman Islands",	
                                       "Central African Republic",	"Chad",	 "Channel Islands", "Chile",	 "China",	"Colombia",	"Comoros",	
                                       "Democratic Republic of Congo",	 "Congo",	"Costa Rica",	"Cote dIvoire",	"Croatia",	"Cuba",	"Curacao", 
                                       "Cyprus",	"Czech Republic",	"Denmark",	"Djibouti",	"Dominica",	 "Dominican Republic",	"Ecuador",	 "Egypt",
                                       "El Salvador",	 "Equatorial Guinea",	"Eritrea",	"Estonia",	"Ethiopia", "Faroe Islands",	"Fiji",	"Finland",
                                       "France",	 "French Polynesia",	 "Gabon",	"The Gambia",	"Georgia",	"Germany",	 "Ghana",	"Gibraltar",
                                       "Greece",	 "Greenland",	"Grenada","Guam",		"Guatemala",	 "Guinea",	"Guinea Bissau",	 "Guyana",	 "Haiti",
                                       "Honduras",	"Hong Kong",	"Hungary",	"Iceland",	 "India",	"Indonesia",	"Iran",	"Iraq",	"Ireland","Isle of Man",	
                                       "Israel",	 "Italy",	"Jamaica",	 "Japan",	 "Jordan",	"Kazakhstan",	 "Kenya",	"Kiribati",
                                       "North Korea",	 "South Korea",	"Kosovo","Kuwait",	 "Kyrgyz Republic",	"Laos",	 "Latvia",	"Lebanon",
                                       "Lesotho",	"Liberia",	 "Libya","Liechtenstein",	"Lithuania",	"Luxembourg",	"Macao", "Macedonia",	
                                       "Madagascar",	 "Malawi",	"Malaysia",	"Maldives",	"Mali",	 "Malta",	 "Marshall Islands",	"Mauritania",	
                                       "Mauritius",	 "Mexico",	"Micronesia",	"Moldova",	 "Monaco",	"Mongolia",	"Montenegro",	"Morocco",	
                                       "Mozambique",	"Myanmar",	"Namibia",	 "Nauru",	 "Nepal",	"Netherlands", "New Caledonia", "New Zealand",
                                       "Nicaragua",	 "Niger",	"Nigeria",	"Northern Mariana Islands",	 "Norway",	"Oman",	"Pakistan",	 "Palau",	
                                       "Panama",	 "Papua New Guinea",	"Paraguay",	"Peru",	"Philippines",	 "Poland",	"Portugal",	"Puerto Rico",
                                       "Qatar",	"Romania",	"Russian Federation",	 "Rwanda",	 "Samoa",	"San Marino",	 "Sao Tome and Principe",	 
                                       "Saudi Arabia",	"Senegal",	 "Serbia",	 "Seychelles",	 "Sierra Leone",	 "Singapore",	 "Sint Maarten",
                                       "Slovak Republic",	"Slovenia",	"Solomon Islands",	"Somalia",	"South Africa",
                                       "South Sudan",	"Spain",	"Sri Lanka",	"St Kitts and Nevis",	"St Lucia",	"St Martin",	
                                       "St Vincent and the Grenadines",	"Sudan",	"Suriname",	"Swaziland",	 "Sweden",	"Switzerland",	"Syria",
                                       "Tajikistan",	"Tanzania",	"Thailand",	 "Timor Leste",	 "Togo",	"Tonga",	"Trinidad and Tobago",	"Tunisia",
                                       "Turkey",	"Turkmenistan",	 "Turks and Caicos",	 "Tuvalu",	 "Uganda",	"Ukraine",	"United Arab Emirates",
                                       "United Kingdom",	"United States",	"Uruguay",	 "Uzbekistan",	"Vanuatu",	"Venezuela",	"Vietnam",
                                       "Virgin Islands US","West Bank and Gaza",	"Yemen",	 "Zambia",	"Zimbabwe",	 "Taiwan",	"Cook Islands",	
                                       "Niue",	 "Wallis and Futuna",	"NA"),
        InterventionType = c("Care Delivery","Diagnostic","Environmental Augmentation","Environmental Remediation","Health Education or Behavior","Immunization","Legislation",
                             "Maternal or Neonatal Care","Medical Device","Medical Procedure","Pharmaceutical","Nutrition","Other","Screening","Surgical")
)
  
#Create all new string, multiples, and display variables in league tables data frame
#strings
for(list in Strings_methods){
  for(string in list){
    methods_raw[,string] <- NA
  }
}  

#populate new string variables with names
for(list in 1:length(Strings_methods)){
  for(num in 1:length(Strings_methods[[list]])){
    string_colnum<-which(colnames(methods_raw) == Strings_methods[[list]][num])
    binary_colnum<-which(colnames(methods_raw) == Binaries_methods[[list]][num])
    methods_raw[,string_colnum]<-ifelse(methods_raw[,binary_colnum] == 1, Names_methods[[list]][num], NA)
  }
}


#--# TARGET POPULATION #--# 
#create gender string variable
leaguetable_data$Gender[leaguetable_data$TargetPopulationGender == 1]<-"Women"
leaguetable_data$Gender[leaguetable_data$TargetPopulationGender == 2]<-"Men"
leaguetable_data$Gender[leaguetable_data$TargetPopulationGender == 3]<-"Both women and men"

#create health state string variable
leaguetable_data$HealthState<-ifelse(is.na(leaguetable_data$TargetPopulationHealthText), "Healthy", paste("with ", leaguetable_data$TargetPopulationHealthText))

#concatenate health state, gender, age, and other text to make string target population variable
leaguetable_data$TargetPopulation<-ifelse(is.na(leaguetable_data$TargetPopulationHealthText), 
                                          paste(leaguetable_data$HealthState, "; ", leaguetable_data$Gender, "; Age:", leaguetable_data$TPage_display, ifelse(is.na(leaguetable_data$TargetPopulationOther), "", paste(";",leaguetable_data$TargetPopulationOther))),
                                          paste(leaguetable_data$Gender, "; Age: ", leaguetable_data$TPage_display, ";", leaguetable_data$HealthState, ifelse(is.na(leaguetable_data$TargetPopulationOther), "", paste(";",leaguetable_data$TargetPopulationOther))))

#concatenate author name
leaguetable_data$Author<-paste(leaguetable_data$PrimAuthFirstName.x, leaguetable_data$PrimAuthLastName.x, sep = " ")

#--# GDP #--# 
#special case because of Display Ratio field
#create GDP string variable
leaguetable_data$GDPCategory<-NA
leaguetable_data$GDPCategory[leaguetable_data$lessthan_onexGDP == 1]<-"Less than 1xGDP"
leaguetable_data$GDPCategory[leaguetable_data$oneto_threexGDP == 1]<-"1-3xGDP"
leaguetable_data$GDPCategory[leaguetable_data$greaterthan_threexGDP == 1]<-"Greater than 3xGDP"
leaguetable_data$GDPCategory[is.na(leaguetable_data$GDP_percapita2017)]<-" "
leaguetable_data$GDPCategory[is.na(leaguetable_data$DisplayRatio)]<-" "
leaguetable_data$GDPCategory[leaguetable_data$dominated_GDP == 1]<-"Dominated"
leaguetable_data$GDPCategory[leaguetable_data$cost_savingGDP == 1]<-"Cost-Saving"
leaguetable_data$GDPCategory[leaguetable_data$DisplayRatio == "Cost-saving"]<-"Cost-Saving"
leaguetable_data$GDPCategory[leaguetable_data$DisplayRatio == "Dominated"]<-"Dominated"
#Create GDP binary variables
leaguetable_data$R_CS<-0
leaguetable_data$R_CS[leaguetable_data$GDPCategory == "Cost-Saving"]<-1
leaguetable_data$R_LT1<-0
leaguetable_data$R_LT1[leaguetable_data$GDPCategory == "Less than 1xGDP"]<-1
leaguetable_data$R_1to3<-0
leaguetable_data$R_1to3[leaguetable_data$GDPCategory == "1-3xGDP"]<-1
leaguetable_data$R_GT3<-0
leaguetable_data$R_GT3[leaguetable_data$GDPCategory == "Greater than 3xGDP"]<-1
leaguetable_data$R_D<-0
leaguetable_data$R_D[leaguetable_data$GDPCategory == "Dominated"]<-1

#destring Display ratio
leaguetable_data$NumericICER<-leaguetable_data$DALYinCurrentUSDollarsRou
#clean to be -999999 for cost saving and 9999999 for dominated
leaguetable_data$NumericICER[leaguetable_data$DisplayRatio == "Cost-Saving"]<-(-999999)
leaguetable_data$NumericICER[leaguetable_data$DisplayRatio == "Cost-saving"]<-(-999999)
leaguetable_data$NumericICER[leaguetable_data$DisplayRatio == "Dominated"]<-9999999

#-----# MISC DATA CLEANING #-----#

#concatenate paper title and author name
leaguetable_data$TitleAut<-paste(leaguetable_data$Title.x, " (",leaguetable_data$PrimAuthLastName.x, ")", sep = "")

#concatenate intervention type and intervention phrase
leaguetable_data$InterventionCombined<-paste(leaguetable_data$IntType_display, ": ", leaguetable_data$InterventionPhrase, sep = "")

#remove ICERS that are not populated
leaguetable_data<-subset(leaguetable_data, !is.na(leaguetable_data$DisplayRatio))

#rename columns
colnames(leaguetable_data)[colnames(leaguetable_data)=="PubYear.x"] <- "Year"
colnames(leaguetable_data)[colnames(leaguetable_data)=="Country.x"] <- "Country"
colnames(leaguetable_data)[colnames(leaguetable_data)=="Intervention"] <- "Intervention1"
colnames(leaguetable_data)[colnames(leaguetable_data)=="ComparatorPhrase"] <- "Comparator"
colnames(leaguetable_data)[colnames(leaguetable_data)=="PrimaryDisCode"] <- "ICD10Code"
colnames(leaguetable_data)[colnames(leaguetable_data)=="SuperRegion_display"] <- "Region"

#disease choices selectize input

disease_choices<-c("",sort(leaguetable_data$GBDDis1_tier1), sort(leaguetable_data$GBDDis1_tier2), sort(leaguetable_data$GBDDis1_tier3),
                   sort(leaguetable_data$GBDDis1_tier4))
disease_choices<-disease_choices[!is.na(disease_choices)]

#country data
countries_data<-read.csv('data/COUNTRY CODES.csv')