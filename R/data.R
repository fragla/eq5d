#' EQ-5D-3L TTO value set data.
#'
#' Coefficients for the estimation of the EQ-5D-3L index values based on TTO valuation studies
#'   for Denmark, Germany, Japan, Netherlands, Spain, UK, USA and Zimbabwe.
#'   
#' @source
#' Szende, A., Oppe, M., & de Charro, F. (2007), Comparative review of Time Trade-Off value sets. 
#'   In Szende, A., Oppe, M., & Devlin, N. (Ed.), EQ-5D Value Sets: Inventory, Comparative Review
#'   and User Guide (pp. 27-28). Dordrecht, The Netherlands: Springer.
#'   
"TTO"

#' EQ-5D-3L VAS value set data.
#'
#' Coefficients for the estimation of the EQ-5D-3L index values based on VAS valuation studies
#'  for Belgium, Denmark, Europe, Finland, Germany, New Zealand, Slovenia, Spain and UK.
#' 
#' @source 
#' Oppe, M., Szende, A., & de Charro, F. (2007), Comparative review of Visual Analogue Scale value sets.
#'   In Szende, A., Oppe, M., & Devlin, N. (Ed.), EQ-5D Value Sets: Inventory, Comparative Review
#'   and User Guide (pp. 37-38). Dordrecht, The Netherlands: Springer.  
#' 
"VAS"

#' EQ-5D-5L VT value set data.
#'
#' EQ-5D-5L VT value set calculation data for Canada, China, England, Germany, 
#'   Hong Kong, Indonesia, Ireland, Japan, Korea, Malaysia, Netherlands, Spain,
#'   Thailand and Uruguay.
#'
#' @source 
#'   \enumerate{
#'     \item{\strong{Canada}: Xie F, Pullenayegum E, Gaebel K, Bansback N, Bryan S, Ohinmaa A, Poissant L, Johnson JA. A Time Trade-off-derived Value Set of the EQ-5D-5L for Canada. Med Care. 2016;54(1):98-105. \href{https://www.ncbi.nlm.nih.gov/pubmed/26492214}{PubMed}}
#'     \item{\strong{China}: Luo N, Liu G, Li M, Guan H, Jin X, Rand-Hendriksen K. Estimating an EQ-5D-5L Value Set for China. Value Health. 2017 Apr;20(4):662-669. doi: 10.1016/j.jval.2016.11.016. Epub 2017 Feb 9. \href{https://www.ncbi.nlm.nih.gov/pubmed/28408009}{PubMed}}
#'     \item{\strong{England}: Devlin N, Shah K, Feng Y, Mulhern B, van Hout B. Valuing health-related quality of Life: An EQ-5D-5L Value Set for England. Health Economics. 2018 Jan;27(1):1-22 \href{https://www.ncbi.nlm.nih.gov/pubmed/28833869}{PubMed}}
#'     \item{\strong{Germany}: Ludwig K, Graf von der Schulenburg JM, Greiner W. German Value Set for the EQ-5D-5L. Pharmacoeconomics. 2018 Feb;36(6):663-674. doi: 10.1007/s40273-018-0615-8. \href{https://www.ncbi.nlm.nih.gov/pubmed/29460066}{PubMed}}
#'     \item{\strong{HongKong}: Wong ELY, Ramos-Goñi JM, Cheung AWL, Wong AYK, Rivero-Arias O. Assessing the Use of a Feedback Module to Model EQ-5D-5L Health States Values in Hong Kong. Patient. 2018 Apr;11(2):235-247. doi: 10.1007/s40271-017-0278-0. \href{https://www.ncbi.nlm.nih.gov/pubmed/29019161}{PubMed}}
#'     \item{\strong{Indonesia}: Purba FD, Hunfeld JAM, Iskandarsyah A, Fitriana TS, Sadarjoen SS, Ramos-Goñi JM, Passchier J, Busschbach JJ. The Indonesian EQ-5D-5L Value Set. PharmacoEconomics. 2017 Nov;35(11)1153-1165. doi: 10.1007/s40273-017-0538-9. \href{https://www.ncbi.nlm.nih.gov/pubmed/28695543}{PubMed}}
#'     \item{\strong{Ireland}: Hobbins A, Barry L, Kelleher D, Shah K, Devlin N, Ramos Goni JM, O’Neill C. Utility Values for Health States in Ireland: A Value Set for the EQ‑5D‑5L. PharmacoEconomics. 2018 Nov;36(11):1345-1353. doi: 10.1007/s40273-018-0690-x. \href{https://www.ncbi.nlm.nih.gov/pubmed/30051267}{PubMed}}
#'     \item{\strong{Japan}: Shiroiwa T, Ikeda S, Noto S, Igarashi A, Fukuda T, Saito S, Shimozuma K. Comparison of Value Set Based on DCE and/or TTO Data: Scoring for EQ-5D-5L Health States in Japan. Value Health. 2016 Jul-Aug;19(5):648-54. \href{https://www.ncbi.nlm.nih.gov/pubmed/27565282}{PubMed}}
#'     \item{\strong{Korea} Kim SH, Ahn J, Ock M, Shin S, Park J, Luo N, Jo MW. The EQ-5D-5L valuation study in Korea. Qual Life Res. 2016 Jul;25(7):1845-52. doi: 10.1007/s11136-015-1205-2. \href{https://www.ncbi.nlm.nih.gov/pubmed/26961008}{PubMed}}
#'     \item{\strong{Malaysia}: Shafie AA; Vasan Thakumar A; Lim CJ;Luo N; Rand-Hendriksen K; Yusof FA. EQ-5D-5L Valuation for the Malaysian Population. PharmacoEconomics. 2019 May;37(5):715-725. doi: 10.1007/s40273-018-0758-7. \href{https://www.ncbi.nlm.nih.gov/pubmed/30535779}{PubMed}}
#'     \item{\strong{Netherlands} Versteegh MM, Vermeulen KM, Evers SM, de Wit GA, Prenger R, Stolk EA. Dutch Tariff for the Five-Level Version of EQ-5D. Value Health. 2016 Jun;19(4):343-52.  doi: 10.1016/j.jval.2016.01.003. \href{https://www.ncbi.nlm.nih.gov/pubmed/27325326}{PubMed}}
#'     \item{\strong{Spain}: Ramos-Goñi JM, Craig B, Oppe M, Ramallo-Fariña Y, Pinto-Prades JL, Luo N, Rivero-Arias O. Handling data quality issues to estimate the Spanish EQ-5D-5L Value Set using a hybrid interval regression approach.  Value in Health 2018 May;21(5):596-604. doi: 10.1016/j.jval.2017.10.023. \href{https://www.ncbi.nlm.nih.gov/pubmed/29753358}{PubMed}}
#'     \item{\strong{Thailand} Pattanaphesaj J, Thavorncharoensap M, Ramos-Goñi JM, Tongsiri S, Ingsrisawang L, Teerawattananon Y. The EQ-5D-5L Valuation study in Thailand. Expert Review of Pharmacoeconomics & Outcomes Research. 2018 Oct;18(5):551-558. doi: 10.1080/14737167.2018.1494574 \href{https://www.ncbi.nlm.nih.gov/pubmed/29958008}{PubMed}}
#'     \item{\strong{Uruguay}: Augustovski F, Rey-Ares L, Irazola V, Garay OU, Gianneo O, Fernández G, Morales M, Gibbons L, Ramos-Goñi JM. An EQ-5D-5L value set based on Uruguayan population preferences. Qual Life Res. 2016 Feb;25(2):323-33. doi: 10.1007/s11136-015-1086-4. \href{https://www.ncbi.nlm.nih.gov/pubmed/26242249}{PubMed}}
#'   }
"VT"